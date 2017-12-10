{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

-- convert account transactions in csv format to ledger entries
-- pesco, 2009 http://www.khjk.org/log/2009/oct.html

module Buchhaltung.Common
  (module Buchhaltung.Common
  ,module Buchhaltung.Utils
  ,module Buchhaltung.Types
  ,textstrip
  )
where

import           Buchhaltung.Types
import           Buchhaltung.Utils
import           Control.Applicative ((<$>))
import           Control.Arrow
import           Control.Lens (Traversal', Lens', lens)
import           Control.Monad.RWS.Strict
import           Control.Monad.Reader
import           Control.Monad.Writer
import qualified Data.Aeson as A
import           Data.Char
import qualified Data.Csv as CSV
import           Data.Csv.Parser
import           Data.Decimal
import           Data.Foldable
import qualified Data.HashMap.Strict as HM
import           Data.List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as E
import qualified Data.ListLike as L
import qualified Data.ListLike.String as L
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Ord
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy.Encoding as S
import           Data.Time.Calendar
import           Data.Time.Format
import           Data.Traversable (traverse)
import qualified Data.Vector as V
import           Hledger (textstrip)
import           Hledger.Data hiding (at)
import           Hledger.Query
import           Hledger.Read
import           Hledger.Reports (defreportopts)
import           Hledger.Reports.EntriesReport (entriesReport)
import           System.IO
import           Text.Parsec
import qualified Text.Parsec.Text as T
import qualified Text.PrettyPrint.Boxes as P
import           Text.Printf

-- * CONFIGURATION


-- * CSV PARSER
readcsv :: Char -> T.Text -> [[T.Text]]
readcsv sep = map (readcsvrow sep) . T.lines

readcsvrow :: Char -> T.Text -> [T.Text]
readcsvrow sep s = either (error.msg.show) id (parse (p_csvrow sep) "stdin" s)
  where msg = printf "CSV (sep %c) Parsing error:\n\n%v\n\n%s" sep s

p_csvrow :: Char -> T.Parser [T.Text]
p_csvrow sep = sepBy1 (p_csvfield sep) (char sep)

p_csvfield :: Char -> T.Parser T.Text
p_csvfield sep = fmap T.pack $ between (char '"') (char '"') p_csvstring
                 <|> many (noneOf [sep])

p_csvstring :: T.Parser String
p_csvstring = many (noneOf "\"" <|> (string escapedDoubleQuotes >> return '"'))

escapedDoubleQuotes = "\\\""

parens :: Parsec T.Text () Int
parens = ( do char ('('::Char)
              m <- parens
              char ')'
              n <- parens
              return $ max (m+1) n
         )  <|> return 0

-- * dbacl output parser

testdbacl = parseTest (
  dbacl_parser [
      "Aktiva:Transfer:Visa"
      ,"Aktiva:Transfer"
      ]
  ) ("Aktiva:Transfer 134.32 Aktiva:Transfer:Visa Aktiva:Transfer:Visa 9129.73 a " :: String)

-- | parse dbacl output (see testdbacl for the format of dbacl's output)
dbacl_parse :: [AccountName]
            -> String
            -> Either ParseError [(AccountName,String)]
dbacl_parse accounts = fmap conv . parse (dbacl_parser sorted) ""
  where conv = fmap $ second L.unwords
        sorted = sortBy (flip $ comparing T.length) $ accounts

dbacl_parser :: [AccountName] -> Parsec String () [(AccountName, [String])]
dbacl_parser accounts = weiter []
  where weiter :: [(AccountName, [String])] -> Parsec String () [(AccountName, [String])]
        weiter res = choice ((map (cat res) accounts) ++ [info res] )
        cat res y = do newc <- try $ do string $ T.unpack y
                                        space
                                        return y
                       spaces
                       weiter $ (newc,[]) : res
        info ((c,i):res) = do w <- try $ manyTill anyChar (many1 space)
                              weiter $ (c,i <> [w]) : res
                           <|> do { w <- many anyChar;  return ((c,i++[w]):res) }
        info [] = fail "empty list in dbacl_parser: This was not planned"


-- * Utilities



idx :: (Eq a, Show a) => [a] -> a -> Int
idx xs x = maybe (error (show x++": CSV Field not found")) id (findIndex (==x) xs)

-- * Dates 


-- | Read the journal file again, before applying Changes (to not
-- overwrite possible changes, that were made in the mean time)
-- saveChanges :: String -- ^ journal path
--                -> (Journal-> (Journal, Integer))  
--                -> IO Journal
saveChanges
  :: (MonadReader (Options User config env) m, MonadIO m)
  => Maybe Journal
  -- this journal will be also changed and then returned
  ->  (Journal -> (Journal, Integer))
  -- ^ modifier, returning number of changed
  -> m Journal
saveChanges journal change = do
  journalPath <- absolute =<< readLedger imported
  liftIO $ do
    ej <- readJournalFile Nothing Nothing False -- ignore balance assertions
          journalPath
    -- print $ length todos
    -- putStr $ unlines $ show <$> todos
    -- either error (print.length.jtxns) ej
    let (j, n) = either error change ej
    if n == 0 then putStrLn "\nNo transactions were changed!\n"
      else do let res = showTransactions j
              writeFile journalPath res
              putStrLn $ "\n"++ show n ++" Transactions were changed"
    return $ maybe j (\j -> 
      let (j2, m) = change j
      in if (n == m) then j2
         else error $ printf
              "Error 123, see source code. Solution: Use a proper database instead of a file. read: %d passed: %d" n m
            ) journal

mixed' :: Amount -> MixedAmount
mixed' = mixed . (:[])

showTransactions :: Hledger.Data.Journal -> [Char]
showTransactions = concatMap showTransactionUnelided .
  entriesReport defreportopts Hledger.Query.Any

-- * Lenses
 
jTrans :: Lens' Journal [Transaction]
jTrans = lens jtxns $ \j y->j{jtxns=y}

tPosts :: Lens' Transaction [Posting]
tPosts = lens tpostings $ \t y -> t{tpostings=y}

pAcc :: Lens' Posting AccountName
pAcc = lens paccount $ \p y -> p{paccount=y}

-- | replaces every matching transaction in the given journal counts
-- the number of changed transactions
changeTransaction
  :: [(Transaction, Transaction)]
  -> Journal
  -> (Journal, Integer)
changeTransaction ts = countUpdates (jTrans . traverse) h
  where
    h t1 = asum $ fmap g ts
      where g (t2, tNew) = guard (t1 == t2) *> Just tNew
  
-- | Update a traversal and count the number of updates
countUpdates :: Traversal' s a
             -> (a -> Maybe a)
             -> s -> (s, Integer)
countUpdates trav mod = second getSum . runWriter . trav g
  where g x = maybe (return x) ((tell (Sum 1) >>) . return) $ mod x

-- instance Monoid.Monoid Integer where
--   mempty = 0
--   mappend = (+)

data WithSource a = WithSource { wTx :: Transaction
                               , wIdx :: Int
                               -- ^ index of the posting with source
                               , wPosting :: Posting
                               , wSource :: Source
                               , wInfo :: a
                               }
  deriving (Functor)



-- instance Hashable Day where 
--   hash = fromInteger . toModifiedJulianDay
--   hashWithSalt salt = hashWithSalt salt . toModifiedJulianDay
  
-- instance Hashable Transaction where 
  
-- instance Hashable Posting where 
-- instance Hashable PostingType where 
-- instance Hashable MixedAmount where 
-- instance Hashable Amount where 
  
-- | extracts the source line from a Transaction
extractSource :: ImportTag -> Transaction
              -> Either String (WithSource ())
extractSource tag' tx =
  left (<> "\nComments: "
        <> T.unpack (L.unlines $ pcomment <$> ps))
  $ g $ asum $ zipWith f [0..] ps
  where f i p = fmap ((,,) i p) . E.nonEmpty . tail
              . T.splitOn tag $ pcomment p
        tag = commentPrefix tag'
        g Nothing = Left $ printf "no comment with matching tag '%s' found." tag
        g (Just (i,p,n)) = do
          source <- A.eitherDecode' . S.encodeUtf8
                    . TL.fromStrict . E.head $ n
          return $ WithSource tx i p source ()
        ps = tpostings tx

injectSource ::  ImportTag -> Source -> Transaction -> Transaction
injectSource tag source t = t
  {tpostings = reverse $ p1{pcomment =
                            commentPrefix tag <> TL.toStrict (json source)
                           } : rest}
  where (p1 : rest) = reverse $ tpostings t

-- instance MonadReader (Options user Config env) m => ReaderM user env m

  
commentPrefix :: ImportTag -> T.Text
commentPrefix (ImportTag tag) = tag <> ": "


trimnl :: T.Text -> T.Text
trimnl = mconcat . T.lines

-- * make CSV data easier to handle

-- http://hackage.haskell.org/package/cassava-0.4.1.0/docs/Data-Csv.html#t:NamedRecord
-- parseCsv :: CSV.FromField a => String -> V.Vector (HM.HashMap B.ByteString a)

type MyRecord = HM.HashMap T.Text T.Text

stripCsv :: ([T.Text], [MyRecord]) -> ([T.Text], [MyRecord])
stripCsv = fmap textstrip ***
  fmap (HM.fromList . fmap (textstrip *** textstrip ) . HM.toList)
  
parseCsv :: Char -- ^ separator
         -> TL.Text -> ([T.Text], [MyRecord])
parseCsv sep = either error ((fmap T.decodeUtf8 . V.toList)
                             *** V.toList)
               . CSV.decodeByNameWith CSV.defaultDecodeOptions
               { decDelimiter = fromIntegral $ ord sep }
               . encodeUtf8

getCsvCreditDebit :: T.Text -> T.Text -> MyRecord -> T.Text
getCsvCreditDebit creditColumn debitColumn record =
  if T.any isDigit creditValue 
  then "-" <> creditValue 
  else debitValue where
  creditValue = getCsv creditColumn record
  debitValue = getCsv debitColumn record

getCsvConcat :: [T.Text] -> MyRecord -> T.Text
getCsvConcat fields record = L.unwords $ flip getCsv record <$> fields
  
getCsvConcatDescription
  :: env -> [Description env] -> MyRecord -> T.Text
getCsvConcatDescription env x record = L.unwords $ g <$> x
  where g (Field f) = getCsv f record
        g (Const t) = t
        g (Read f)  = f env

getCsv :: T.Text -> MyRecord -> T.Text
getCsv c x = lookupErrD (show (HM.keys x)) HM.lookup c x

-- * Import Types


data ImportedEntry' a s = ImportedEntry {
  ieT :: Transaction -- ^ transaction without postings (they will be inserted later)
  ,iePostings :: a
  ,ieSource :: s -- ^ source to check for duplicates and for Bayesian matching
  } deriving Show

type ImportedEntry =  ImportedEntry'
  [(AccountId, T.Text, Maybe T.Text, Bool)] Source
  -- ^ postings of [acount,amount]: only ImportedEntry with one
  -- posting is currently implemented in the statists functionality of
  -- Add.hs (See PROBLEM1) as well in the duplicates algorithm in 'addNew'

type FilledEntry =  ImportedEntry' () Source

fromFilled :: FilledEntry -> Entry
fromFilled x = x{ieSource = Right $ ieSource x}
  
type Entry =  ImportedEntry' () (Either String Source)

-- | helper function to create transaction for ImportedEntry
genTrans :: Day -> Maybe Day -> T.Text -> Transaction
genTrans date date2 desc =
  nulltransaction{tdate=date, tdescription=desc, tdate2=date2}

normalizeMixedAmountWith
  :: (Amount -> Decimal) -> MixedAmount -> MixedAmount
normalizeMixedAmountWith f (Mixed ams) = Mixed $ g <$> ams
  where g a =  a{aquantity = normalizeDecimal $ f a}
  
data Importer env = Importer
  { iModifyHandle :: Maybe (Handle -> IO ())
  -- ^ e.g. 'windoof'
  , iImport :: T.Text -> CommonM (env, Maybe Version) [ImportedEntry]
  }

windoof :: Maybe (Handle -> IO ())
windoof = Just $ \h -> hSetEncoding h latin1
                       >> hSetNewlineMode h universalNewlineMode


parseDate :: String -> T.Text -> Day
parseDate format = parseTimeOrError True defaultTimeLocale format . T.unpack

parseDateDE = parseDate "%d.%m.%Y"
parseDateUS = parseDate "%m/%d/%Y"
  
-- | retrieval function
type Getter a = MyRecord -> a

data CsvPostingImport = CsvPosting
  { cAccount :: Getter T.Text
  , cAmount  :: Getter T.Text
  , cSuffix  :: Maybe (Getter T.Text)
  , cNegate  :: Getter Bool
  -- ^ Amount parsable by 'mamoumtp\''
  }

data CsvImport env = CSV
  { cFilter :: MyRecord -> Bool
  -- ^ should this csv line be processed?
  , cDate :: Getter Day
  , cStrip :: Bool
  , cVDate :: Getter (Maybe Day)
  , cBank :: env -> Getter T.Text
  , cHeader      :: [T.Text]
  , cBayes       :: [T.Text]
  , cDescription :: [Description env]
  , cVersion     :: Version
  , cSeparator :: Char
  , cPostings :: [env -> CsvPostingImport]
  }

data Description env = Field T.Text | Const T.Text | Read (env -> T.Text)

toField (Field t) = Just t
toField _ = Nothing


data CheckedCsvImport a = UnsafeCSV { cRaw :: CsvImport a }
  -- deriving (Show)

toVersionedCSV
  :: SFormat DefaultVersion
     -> [CsvImport a] -> VersionedCSV a
toVersionedCSV format headers = sequence $ (,) format $ fromListUnique $
  (cVersion . cRaw &&& id) . checkRawCSV format <$> headers

type VersionedCSV env = forall m. MonadError Msg m
                      => m (SFormat DefaultVersion, M.Map Version (CheckedCsvImport env))
                      -- ^ (format with default version, _)

data DefaultVersion = DefaultVersion { fromDefaultVersion :: Version }

checkRawCSV :: SFormat b -> CsvImport a -> CheckedCsvImport a
checkRawCSV format rh =
  if null missing then UnsafeCSV rh
  else error $ printf
       ("format '%s', version '%s': The configured header misses the following "
        ++ "fields required for Bayes or Description:\n%s")
       (fName format) (cVersion rh) $ unlines $ uncurry (printf "%s: %s") <$> missing
  where [head, bayes, desc] = S.fromList  . ($rh) <$>
          [cHeader, cBayes, mapMaybe toField . cDescription]
        missing =
          concatMap (uncurry zip <&> repeat *** (fmap T.unpack . toList . flip S.difference head))
                  [("cBayes", bayes), ("cDescription", desc)] :: [(String, String)]


-- * Pretty Printing

table :: [Int] -- ^ max width
      -> [T.Text] -- ^ Header
      -> [[T.Text]] -- ^ list of cols
      -> P.Box
table w h = table1 . table2 w h
  
table1 :: NonEmpty [P.Box] -- ^ list of rows
       -> P.Box
table1 (header :| rows) = P.punctuateH P.top
             (P.vcat P.top $ replicate (ml P.rows cols2) $ P.text " | ")
             cols2
   where h colHead col = P.vcat P.left $ colHead : sep : col
           where sep = text' $ L.replicate (ml P.cols $ colHead : col) '-'
         ml f = maximum . fmap f
         cols2 = zipWith h header $ transpose rows
                                      
table2 :: [Int] -- ^ max width
       -> [T.Text] -- ^ Header
       -> [[T.Text]] -- ^ list of cols
       -> NonEmpty [P.Box] -- ^ list of rows
table2 widths header cols =
  toRow <$> (header :| transpose cols)
  where 
        toRow = g . zipWith asd widths
        asd w = P.para P.left w . T.unpack
        g row = P.alignVert P.top mr <$> row
          where mr = maximum $ P.rows <$> row

mlen :: L.ListLike l e => [l] -> Int
mlen = maximum . fmap L.length

text' :: T.Text -> P.Box
text' = P.text . T.unpack

loadJournal
  :: (MonadError Msg m, MonadIO m) =>
     [Ledgers -> Maybe FilePath]
     -> Options User config env -> m Journal
loadJournal journals options = do
  liftIO $ printf "(Reading journal from \n%s)\n...\n\n"
    $ intercalateL "\n" $ show <$> jfiles
  journal <- liftIO $
      -- to conquer problems with the `instance Monoid Journal`
     right mconcat' . sequence <$> mapM (readJournalFile Nothing Nothing False) jfiles
  either (throwError . T.pack) return journal
  where jfiles = runReader (catMaybes <$> mapM (mapM absolute <=< readLedger)
                           journals) options
        jfiles :: [FilePath]
