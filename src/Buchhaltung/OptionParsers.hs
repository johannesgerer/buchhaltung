module Buchhaltung.OptionParsers where

import           Buchhaltung.Common
import           Control.Exception
import           Control.Monad.RWS.Strict
import           Data.Maybe
import           Data.Foldable
import qualified Data.Text as T
import           Options.Applicative
import           System.Directory
import           System.Environment
import           System.FilePath
import qualified Text.PrettyPrint.ANSI.Leijen as D
import           Text.Printf



-- * Option parsers
--
-- https://hackage.haskell.org/package/optparse-applicative/docs/Options-Applicative-Builder.html

mainParser = do
  env <- getEnvironment
  home <- try getHomeDirectory :: IO (Either SomeException FilePath)
  return $ info
    ( helper *> (Options
                <$> userP
                <*> profile env home
                <*> subparser commands
                <*> pure ()
                <*> pure ()
              )
    ) mempty

paragraph = foldr ((D.</>) . D.text) mempty . words

userP :: Parser (Maybe Username)
userP = optional $ (Username . T.pack) <&> strOption $ long "user"
        <> short 'u' <> metavar "USER"
        <> helpDoc
        (Just $ paragraph "Select the user. Default: first configured user")

envVar = "BUCHHALTUNG"

-- | optparse profile folder
profile
  :: Exception b =>
     [(String, FilePath)] -> Either b FilePath -> Parser FilePath
profile env home = strOption $
  long "profile"
  <> value (fromMaybe (either throw buch home) envBuch)
  <> short 'p'
  <> metavar "FOLDER"
  <>  helpDoc (Just $
       paragraph "path to the profile folder. Precedence (highest to lowerst):"
        D.<$> D.text "1. this command line option"
        D.<$> paragraph (printf "2. Environment option: \"%s\" %s" envVar $
                         (maybe "(not set)" (printf "= '%s'") envBuch :: String ))
        D.<$> paragraph (printf "3. %s %s" (buch "~") $
                          either (const "(home dir not available)")
                          (("= " ++) . buch) home)
              )
  where buch = (</> ".buchhaltung")
        envBuch = lookup envVar env

versionP :: Parser (Maybe T.Text)
versionP = optional . fmap T.pack
           $ strOption $ long "version"
            <> short 'v' <> metavar "VERSION"

-- | optparse command parser
-- commands :: Parser Action
commands :: Mod CommandFields Action
commands =
  command' "add"
  (Add . fmap (Username . T.pack) <$> many
    (strOption (short 'w' <> help "with partner USERNAME"
               <> long "with"
                 <> metavar "USERNAME")))
  (progDesc "manual entry of new transactions")

  <> command' "import"
  (Import <$> versionP
    <*> strArgument (metavar "FILENAME")
    <*> subparser importOpts)
  (progDesc "import transactions from FILENAME")

  <> command' "update"
  (Update <$> versionP
    <*> (switch $ short 'm' <> long "match" <> help "run match after import")
    <*> (fmap not $ switch $ short 'n' <> long "no-fetch"
         <> help "do not fetch new transactions"))
  (progDesc
   "fetch and import AQ Banking transactions (using \"aqbanking request\" and \"listtrans\")")

  <> command' "match" (pure Match)
  (progDesc "manual entry of new transactions")

  <> command' "lb" (pure ListBalances)
  (progDesc "list balances of all AQBanking accounts (only available after 'update')")

  <> command' "setup" (pure Setup)
  (progDesc "initial setup of AQBanking")

  -- todo: pass through, that only sets env var and runs remaining args as "command args"

  <> passThrough Commit "commit"  (Just "c") (Just $ concat
    ["run git commit in the dir of the user's mainLedger file and pass all "
    ,"following arguments to git. "
    ,"The Message will contain all AQBalances "
    ,"and the Ledger balance sheet"])

  <> passThrough Ledger "ledger" (Just "l") Nothing

  <> passThrough HLedger "hledger" (Just "hl") Nothing

  <> passThrough AQBanking "aqbanking" (Just "aq") Nothing

passThrough
  :: ([String] -> Action)
  -> String -- ^ command
  -> Maybe String -- ^ Short version
  -> Maybe String -- ^ Message
  -> Mod CommandFields Action
passThrough constr long short desc = mconcat $ f <$> long : toList short
  where f cmdName = command' cmdName
          (constr <&> many $ strArgument mempty)
          $ noIntersperse
          <> (progDesc $ fromMaybe
              ("pass args through to '" ++ long ++ "'") desc)

command' :: String -> Parser a -> InfoMod a -> Mod CommandFields a
command' str parser infomod =
  command str $ info (helper *> parser) infomod

importOpts :: Mod CommandFields ImportAction
importOpts =
  metavar "FORMAT"
  <>
  commandGroup "Available FORMATS"
  <>
  command' "barclaycardus"
  (pure BarclaycardUs)
  (progDesc $ concat ["import from BarclaycardUs web export. "
                     ,"versions: May 2017"])

  <>
  command' "comdirectVisa"
  (ComdirectVisa . T.pack <$> strArgument
    (metavar "BLZ"))
  (progDesc $ concat ["import from german Comdirect Visa Statements. "
                     ,"versions: manuell, export"])

  <>
  command' "paypal"
  (Paypal . T.pack <$> strArgument
    (help "paypal username (as configured in 'bankAccounts')"
      <> metavar "PAYPAL_USERNAME"))
  (progDesc $ concat ["import from german Paypal CSV export with "
                     ,"\"alle guthaben relevanten Zahlungen "
                     ,"(kommagetrennt) ohne warenkorbdetails\". "
                     ,"versions: 2013, 2014, 2016"])

  <>
  command' "pncbank"
  (Pncbank . T.pack <$> strArgument
    (help "PNC account identifier or number (as configured in 'bankAccounts')"
      <> metavar "ACCOUNT_ID"))
  (progDesc $ concat ["import from PNC Bank web export. "
                     ,"versions: May 2017"])

  <> 
  command' "aqbanking"
  (pure AQBankingImport)
  (progDesc $ concat ["import CSV file generated by "
                     ,"\"aqbankingcli listtrans\". "
                     ,"versions: 4"])
