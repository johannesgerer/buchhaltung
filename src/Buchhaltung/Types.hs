{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE CPP #-}
#define DERIVING Generic, Default, Show, FromJSON
module Buchhaltung.Types
  (module Control.Monad.Except
  ,module Buchhaltung.Types
  )where

import           Buchhaltung.Utils
import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad.Except
import           Control.Monad.RWS.Strict
import           Control.Monad.Reader
import qualified Data.Aeson as A
import qualified Data.Aeson.Text as A
import qualified Data.Aeson.Types as A
import           Data.Char
import           Data.Default
import           Data.Function
import qualified Data.HashMap.Strict as HM
import           Data.Hashable
import qualified Data.ListLike as L
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.String
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import           Data.Yaml
import           Formatting (sformat, (%))
import           Formatting.Internal (Format)
import qualified Formatting.ShortFormatters as F
import           GHC.Generics
import           Hledger.Data
import           Prelude hiding (lookup)
import           System.FilePath
import qualified Text.Regex.TDFA as R
import  Text.Regex.TDFA.Text()

-- * Monad used for most of the funtionality

type CommonM env = RWST (FullOptions env) () () (ErrorT IO)

-- * The Source of an imported transaction

-- | represents a key value store and a protocol
data Source = Source { sFormat :: T.Text
                     , sStore :: M.Map T.Text T.Text }
  deriving (Generic, Show, Eq, Ord, Read)


-- | Creates a 'Source' from non null values of a HashMap (e.g. from
-- 'MyRecord')
fromMapToSource :: T.Text -> HM.HashMap T.Text T.Text -> Source
fromMapToSource format = Source format . M.fromList .
                         filter (not . L.null . snd) . HM.toList

-- | produces a map that includes 'sFormat' under the key @"format"@
sourceToMap :: Source -> M.Map T.Text T.Text
sourceToMap s = M.insertWith (\x y -> x <> ", " <> y) "format"
                (sFormat s) $ sStore s

json :: Source -> TL.Text
json = A.encodeToLazyText

instance FromJSON Source where
  parseJSON = A.genericParseJSON $ stripPrefixOptions 1

instance ToJSON Source where
  toEncoding = A.genericToEncoding $ stripPrefixOptions 1

stripPrefixOptions n = A.defaultOptions{A.fieldLabelModifier = g}
  where g = drop n . fmap toLower

-- * Import Tag


newtype ImportTag = ImportTag { fromImportTag :: T.Text }
  deriving ( Generic, Show)

instance Default ImportTag where
  def = "SOURCE"

instance IsString ImportTag where
  fromString = ImportTag . fromString

-- * Error handling

type Msg = T.Text
type Error = Either Msg
type ErrorT = ExceptT Msg

throwFormat
  :: MonadError Msg m => Format T.Text t -> (t -> Msg) -> m b
throwFormat msg a = throwError $ a $ sformat msg

maybeThrow
  :: MonadError Msg m => Format T.Text t
  -> (t -> Msg) -> (a1 -> m b) -> Maybe a1 -> m b
maybeThrow msg a = maybe (throwFormat msg a)


lookupErrD
  :: Show t => [Char] -- ^ additional description
  -> (t -> t1 -> Maybe a) -- ^ lookup function
  -> t -- ^ lookup arg2
  -> t1 -- ^ lookup arg2
  -> a
lookupErrD d l k m =
  either (error . T.unpack) id $ runExcept $ lookupErrM d l k m


lookupErrM
  :: (MonadError Msg m, Show a) =>
     String -> (a -> t -> Maybe b) -> a -> t -> m b
lookupErrM description lookup k container =
  maybeThrow ("lookupErr: " %F.sh% " not found: " %F.s)
  (\f -> f k description) return
  $ lookup k container

-- * Options

data Options user config env = Options
  { oUser :: user
  , oProfile :: FilePath
  , oAction :: Action
  , oConfig :: config
  , oEnv :: env
  }
  deriving (Show, Generic, NFData)

type FullOptions = Options User Config
type RawOptions = Options (Maybe Username) ()

toFull :: MonadError Msg m
       => RawOptions env
       -> Config
       -> m (FullOptions env)
toFull opts1@Options{oUser=user} config =
  (\u -> opts2{oUser = u}) <$>
  runReaderT (maybe (defaultUser 0) lookupUser user) opts2
  where opts2 = opts1 { oConfig = config }

-- ** Reading options

readConfig :: MonadReader (Options user config env) m => (config -> a) -> m a
readConfig f = reader $ f. oConfig

readUser  :: MonadReader (Options user config env) m => (user -> a) -> m a
readUser f = reader $ f . oUser

user  :: MonadReader (Options user config env) m => m user
user = readUser id

readLedger
  :: MonadReader (Options User config env) m =>
     (Ledgers -> FilePath) -> m FilePath
readLedger = (<$> readUser ledgers)

-- | get absolute paths in profile dir
absolute
  :: MonadReader (Options user config env) m =>
     FilePath -> m FilePath
absolute f = do prof <- reader oProfile
                return $ prof </> f


-- * Config

data Config = Config
  {
    cUsers :: Users
  , cUserList :: V.Vector Username
  , cImportTag :: ImportTag
  , cTodoAccount :: AccountName
  -- ^ account for unmatched imported transactions
  , cFormats :: HM.HashMap T.Text [T.Text]
  -- ^ for every format a list of columns used in the bayesian
  -- classifier used in match
  , cDbaclExecutable :: FilePath
  , cLedgerExecutable :: FilePath
  , cHledgerExecutable :: FilePath
  }
  deriving ( Generic, Show )

askTag :: MonadReader (Options user Config env) m =>  m ImportTag
askTag = readConfig $ cImportTag

askTodoFilter :: MonadReader (Options user Config env) m
              => m (AccountName -> Bool)
askTodoFilter = return . L.isPrefixOf =<< readConfig cTodoAccount

instance FromJSON Config where
  parseJSON (Object v) = do
    -- ^ Users are configured as list, to have a defined order
    users <- parseJSON =<< v .: "users" :: A.Parser (V.Vector User)
    formats <- v .:? "formats" .!= mempty
    dbEx <- v .:? "dbaclExecutable" .!= "dbacl" :: Parser FilePath
    [lEx, hlEx] <- forM ["", "h"] $ \pfx ->
      v .:? (T.pack pfx <> "ledgerExecutable") .!= (pfx <> "ledger")
      :: Parser FilePath
    return Config
      { cUsers = HM.fromList $ (\u -> (name u, u)) <$> V.toList users
      , cUserList = name <$> users
      , cImportTag = def
      , cTodoAccount = "TODO"
      , cFormats = formats
      , cDbaclExecutable = dbEx
      , cLedgerExecutable = lEx
      , cHledgerExecutable = hlEx
      }

  parseJSON invalid    = A.typeMismatch "Config" invalid

readConfigFromFile :: FilePath -> IO Config
readConfigFromFile path = either (error . prettyPrintParseException) id <$>
  decodeFileEither (path </> "config" <.> "yml") :: IO Config


-- * User

data User = User
  { name :: Username
  , ledgers :: Ledgers
  , accountPrefixOthers :: Maybe AccountName
  -- ^ the account prefix for accounts receivable or payable
  -- (depending on current balance) to other users
  , aqBanking :: Maybe AQBankingConf
  , bankAccounts :: BankAccounts
  , ignoredAccountsOnAdd :: [Regex]
  , numSuggestedAccounts :: Maybe Int
  }
  deriving ( Generic, Show, FromJSON )

type Users = HM.HashMap Username User

newtype Username = Username T.Text
  deriving ( Generic, FromJSON, NFData, Eq, Hashable, A.FromJSONKey)

fromUsername :: Username -> T.Text
fromUsername (Username n) = n

instance Show Username where
  show = T.unpack . fromUsername

instance Eq User where
  (==) = (==) `on` name

-- ** Reading User settings

-- | Looks up a user and throws an error if they do not exist.
lookupUser :: ( MonadError Msg m
              , MonadReader (Options user Config e) m)
           => Username
           -> m User
lookupUser user = do
  maybeThrow msg ($ user) return . HM.lookup user . cUsers =<< reader oConfig
  where msg = "User '"%F.sh%"' not found"

defaultUser :: ( MonadError Msg m
               , MonadReader (Options user Config e) m)
           => Int -- ^ default position in user list
           -> m User
defaultUser ix = do
  config <- reader oConfig
  maybeThrow msg ($ succ ix) lookupUser $ cUserList config V.!? ix
  where msg = "There are less than "%F.d%
          " users configured. Please add a user to the config file."

-- ** A User's ledger files

data Ledgers = Ledgers
  { imported :: FilePath
  , addedByThisUser  :: FilePath
  , addedByOthers :: Maybe FilePath
  , mainLedger :: FilePath -- ^ ledger file for 'ledger' CLI
  , mainHledger :: Maybe FilePath -- ^ ledger file for 'hledger' CLI
  }
  deriving (Generic, Default, Show, FromJSON)

-- | generates the receiable/payable account for between two users
-- (suffixed by the current, the recording, user)
receivablePayable
  :: (MonadError Msg m, MonadReader (FullOptions env) m)
  => Bool
  -- ^ TRUE | FALSE = for (this | the other) user's ledger
  -> User -- ^  the other user
  -> m T.Text
receivablePayable forThis other = do
  this <- readUser id
  let (ledgerU, accountU) =
        if forThis then (this, other) else (other, this)
  let full pref = return $ intercalateL ":" $ pref :
        (fromUsername . name <$> [accountU, this])
  maybeThrow msg ($ name ledgerU) full
    $ accountPrefixOthers ledgerU
  where msg =  "User '"%F.sh%"' has no accountPrefixOthers configured"

-- ** A user's bank accounts

askAccountMap :: MonadReader (Options User config env) m =>  m AccountMap
askAccountMap = readUser $ fromBankAccounts . bankAccounts


newtype BankAccounts = BankAccounts AccountMap
  deriving (Generic, Default, Show)


fromBankAccounts :: BankAccounts -> AccountMap
fromBankAccounts (BankAccounts b) = b

isIgnored :: User -> AccountName -> Bool
isIgnored user acc = or $ g <$> ignoredAccountsOnAdd user
  where g ign = R.match (rRegex ign) acc

data Regex = Regex
             {rShow :: T.Text
             , rRegex :: R.Regex
             }

instance FromJSON Regex where
  parseJSON (String v) = Regex v <$> R.makeRegexM v
  parseJSON invalid    = A.typeMismatch "regex string" invalid

instance Show Regex where
  show = T.unpack . rShow

data AccountId = AccountId { aBank :: T.Text
                           , aAccount :: T.Text }
  deriving (Generic, Eq, Ord, Hashable, Show)

type AccountMap = HM.HashMap AccountId AccountName

instance (Hashable a, Eq a) => Default (HM.HashMap a b) where
  def = mempty

instance FromJSON BankAccounts where
  parseJSON (Object v) = BankAccounts . HM.fromList . concat
    <$> traverse parseAccountMap (HM.toList v)
  parseJSON invalid    = A.typeMismatch "BankAccounts" invalid

parseAccountMap :: FromJSON b => (T.Text, Value) -> Parser [(AccountId, b)]
parseAccountMap (bank, (Object m)) = traverse f $ HM.toList m
  where f (acc,v) = (,) (AccountId bank acc) <$> parseJSON v
parseAccountMap (_, invalid)       = A.typeMismatch "parseAccountMap" invalid

-- * AQBanking

data AQBankingConf = AQBankingConf
  { connections :: [AQConnection]
  , configDir :: FilePath
  , aqBankingExecutable :: Maybe FilePath
  , aqhbciToolExecutable :: Maybe FilePath
  }
  deriving ( DERIVING )

data AQConnection = AQConnection
    { aqUser :: String
    , aqBlz :: String
    , aqUrl :: String
    , aqHbciv :: HBCIv
    , aqName :: String
    , aqType :: AQType
    }
  deriving ( Generic, Show)

instance FromJSON AQConnection where
  parseJSON = A.genericParseJSON $ stripPrefixOptions 2

instance ToJSON AQConnection where
  toEncoding = A.genericToEncoding $ stripPrefixOptions 2

-- -- | workaround yaml problem, where 46549 does not parse as a string.
-- newtype String2 = String2 String
--   deriving ( Generic, Show)

-- instance FromJSON String2 where
--   parseJSON (String v) = String2 v
--   parseJSON (Number v) = String2 v

-- | other modes have to be setup manually. Refer to the AQBanking
-- manual. Use the '-C' to point to the configured 'configDir'.
data AQType = PinTan
            | Other
  deriving ( Generic, Show, FromJSON, ToJSON, Eq )

data HBCIv = HBCI201
           | HBCI210
           | HBCI220
           | HBCI300
  deriving ( Generic, Show, FromJSON, ToJSON )

toArg HBCI201 = "201"
toArg HBCI210 = "210"
toArg HBCI220 = "220"
toArg HBCI300 = "300"

-- readAQ
--   :: (MonadError Msg m, MonadReader (Options User config env) m) =>
--      (AQBankingConf -> m b) -> m b
-- readAQ f = do

-- * Actions


type PaypalUsername = T.Text

data Action = Add { aPartners :: [Username] }
            | Match
            | Import FilePath ImportAction
            | Update { aqMatch :: Bool
                        -- ^ run match after import
                        , aqRequest :: Bool
                        -- ^ request new transactions
                        }
            | Commit { cArgs :: [String] }
            | ListBalances
            | Setup
            | Ledger { lArgs :: [String] }
            | HLedger { hlArgs :: [String] }
            | AQBanking { aqArgs :: [String] }

  deriving (Show, Generic, NFData)


data ImportAction = Paypal PaypalUsername
                  | AQBankingImport
  deriving (Show, Generic, NFData)


-- * Misc


type Comment = T.Text
