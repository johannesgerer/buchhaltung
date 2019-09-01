{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Buchhaltung.Importers
(
  paypalImporter
  , paypalEngImporter
  , aqbankingImporter
  , comdirectVisaImporter
  , monefyImporter
  , natwestIntlImporter
  , barclaysUkImporter
  , revolutImporter
  , barclaycardusImporter
  , pncbankImporter
  , module Buchhaltung.Import
  , getBayesFields
  )
where

import           Buchhaltung.Common
import           Buchhaltung.Import
import           Control.Arrow
import           Control.Monad.Cont
import           Control.Monad.RWS.Strict
import           Data.Foldable
import           Data.Functor.Identity
import qualified Data.HashMap.Strict as HM
import           Data.List
import qualified Data.ListLike as L
import qualified Data.ListLike.String as L
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Ord
import           Data.String
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import           Data.Time.Calendar
import           Debug.Trace
import           Formatting (sformat, (%), shown)
import qualified Formatting.ShortFormatters as F
import           Safe as S
import           System.IO
import           Text.Parsec
import qualified Text.ParserCombinators.Parsec as C
import           Text.Printf
import qualified Text.Regex.TDFA as R
import           Text.Regex.TDFA.Text ()
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as T

-- * CSV


-- findVersion
--   :: (MonadError Msg m, Show k, Ord k) => Maybe k -> M.Map k b -> m b
headerInfo
  :: MonadError Msg m =>
     VersionedCSV a -> Maybe Version -> m (SFormat Version, CsvImport a)
headerInfo g v = do
  (format, map) <- g
  let convert rh = (cVersion rh <$ format, rh)
  convert . cRaw <&> lookupErrM
    (printf "Version is not defined for format '%s%'" $ fName format)
    M.lookup (fromMaybe (fromDefaultVersion $ fVersion format) v) map

-- | Data type for preprocessing and meta-data extraction of CSV files
type Preprocessor env1 env2 = forall m. MonadError Msg m
                      => (T.Text, env1) -> m (T.Text, env2)

processLines :: ([T.Text] -> [T.Text]) -> Preprocessor env env
processLines f = return . (first $ T.unlines . f . T.lines)

csvImport = csvImportPreprocessed return

csvImportPreprocessed :: Preprocessor env1 env2
                      -> VersionedCSV env2
                      -> Importer env1
csvImportPreprocessed pp versionedCsv textOrHandle = do
  (env, version) <- reader oEnv
  (form, g@CSV{cHeader=expected,
     cDescription = desc,
     cVersion = version }) <- headerInfo versionedCsv version
  csv1 <- either return (liftIO . cGetContents g) textOrHandle
  (csv2, env2) <- pp (csv1, env)
  let toEntry x = ImportedEntry
          { ieT = genTrans date (vdate =<< cVDate g x)
                  (getCsvConcatDescription env2 desc x)
          , ieSource  = fromMapToSource form x
          , iePostings = 
            (\p -> (AccountId (cBank g env2 x) (cAccount p x)
                   , cAmount p x
                   , ($ x) <$> cSuffix p
                   , cNegate p x)) . ($ env2) <$> cPostings g
          }
          where
            vdate vd = if date == vd then Nothing
                       else Just vd
            date  = cDate g x
      (header, rows) = (if cStrip g then stripCsv else id) $
        parseCsv (cSeparator g) . TL.fromStrict $ csv2
  if expected == header then
    return $ fmap toEntry $ filter (cFilter g) rows
    else throwError $ L.unlines
                  [sformat ("Headers do not match. Expected by format '"%
                            F.st%"' version '"%F.st%"':")
                   (fName form) version
                  ,fshow expected
                  ,"Given:"
                  ,fshow header
                  ]


-- * AQBanking
--
-- imports output from @aqbankingcli listtrans@

aqbankingImporter :: Importer env
aqbankingImporter = csvImport aqbankingImport

aqbankingImport :: VersionedCSV env
aqbankingImport = toVersionedCSV (SFormat "aqBanking" $ DefaultVersion "4")
  [CSV
    { cFilter  = const True
    , cDate = readdate . getCsv "date"
        , cStrip = False
    , cVDate = Just . readdate . getCsv "valutadate"
    , cBank = const $ getCsv "localBankCode"
    , cPostings =
        [ const CsvPosting
          { cAccount = getCsv "localAccountNumber"
          , cAmount = getCsvConcat [ "value_value"
                                   , "value_currency"]
          , cSuffix = Nothing
          , cNegate = const False
          }]
    , cSeparator = ';'
    , cVersion= "4"
    , cHeader = ["transactionId"
                ,"localBankCode"
                ,"localAccountNumber"
                ,"remoteBankCode"
                ,"remoteAccountNumber"
                ,"date"
                ,"valutadate"
                ,"value_value"
                ,"value_currency"
                ,"localName"
                ,"remoteName"
                ,"remoteName1"
                ,"purpose"
                ,"purpose1"
                ,"purpose2"
                ,"purpose3"
                ,"purpose4"
                ,"purpose5"
                ,"purpose6"
                ,"purpose7"
                ,"purpose8"
                ,"purpose9"
                ,"purpose10"
                ,"purpose11"
                ,"category"
                ,"category1"
                ,"category2"
                ,"category3"
                ,"category4"
                ,"category5"
                ,"category6"
                ,"category7"]
    , cDescription = Field <$> desc
    , cBayes = ["remoteBankCode","remoteAccountNumber"]
               ++ desc
    , cGetContents = T.hGetContents
    }]
  where desc = concatMap (\(f,i) -> (f <>) <$> "":i)
               [ ("remoteName", ["1"])
               , ("purpose",  fshow <$> [1..11])
               , ("category", fshow <$> [1..7])]

-- * Postbank Germany Kontoauszüge (from PDF with @pdftotext@)

-- fromPostbankPDF2 :: T.Text -> [ImportedEntry]
-- fromPostbankPDF2 xx = fmap (f . mconcat) $ groupBy y $ readcsvrow ',' <$> L.lines xx
--   where y a b = head b==""
--         f :: [T.Text] -> ImportedEntry
--         f l@(dat:(_:(des:(am:rest)))) = ImportedEntry{
--            ieT = genTrans (parseDateDE $ dat <> "2014") Nothing (L.unwords s)
--            ,ieSource  = v $ T.intercalate (v $ T.singleton hbci_sep) l
--            ,iePostings=("Aktiva:Konten:Giro", a <> " EUR")
--            }
--           where s = (c des):rest
--                 c = L.unwords . tail . L.words
--                 a = mconcat $ L.words $ comma am
--                 v r = "\"" <> r <> "\""

-- -- "/home/data/finanzen/imported/daniela_manuell1.csv"
-- fromPostbankPDF1 :: T.Text -> [ImportedEntry]
-- fromPostbankPDF1 xx = fmap (f.concat) $ groupBy y $ readcsvrow ',' <$> L.lines xx
--   where y a b = head b==""
--         f :: [T.Text] -> ImportedEntry
--         f l@(dat:(des:(am:rest))) = ImportedEntry{
--            ieT = genTrans (parseDateDE $ dat <> "2013") Nothing (L.unwords s)
--            ,ieSource  = v $ T.intercalate (v $ T.singleton hbci_sep) l
--            ,iePostings=("Aktiva:Konten:Giro", a <> " EUR")
--            }
--           where s = (c des):rest
--                 c = L.unwords . tail . L.words
--                 a = mconcat $ L.words $ comma am
--                 v r = "\"" <> r <> "\""

-- * Comdirect Germany
--
-- uses the old way. Do not adopt!

comdirectToAqbanking :: IO ()
comdirectToAqbanking = toAqbanking2 ';' T.getContents comdirect_header comdirect_mapping $ const True


comdirect_header :: [[Char]]
comdirect_header = ["Buchungstag","Wertstellung (Valuta)","Vorgang","Buchungstext","Umsatz in EUR"]

comdirect_header_visa :: [[Char]]
comdirect_header_visa = ["Buchungstag","Umsatztag","Vorgang","Referenz","Buchungstext","Umsatz in EUR"]

comdirect_mapping_visa :: [([Char], T.Text -> T.Text)]
comdirect_mapping_visa = [
  ("Referenz",const ""),
  ( "Referenz" , undefined),
  ( "Referenz" , const "Visa")
  ] ++ empty 2 ++ [
  ( "Buchungstag", fshow . readdate2),
  ( "Umsatztag", fshow. readdate2),
  ( "Umsatz in EUR", comma ),
  ( "Umsatz in EUR", const "EUR" ),
  ( "Umsatz in EUR", const "Johannes Gerer" ),
  ( "Buchungstext", id),
  ("Referenz",const ""),
  ( "Vorgang",id),
  ( "Referenz",id)] ++ empty (32-14)
    where empty x = take x $ repeat ( "Referenz" , const "")

comdirect_mapping :: [([Char], T.Text -> T.Text)]
comdirect_mapping = [
  ("Buchungstag",const ""),
  ( "Buchungstag" , undefined),
  ( "Buchungstag" , const "Visa")
  ] ++ empty 2 ++ [
  ( "Buchungstag", fshow . readdate2),
  ( "Wertstellung (Valuta)", fshow . readdate2),
  ( "Umsatz in EUR", const "WTF" ),
  ( "Umsatz in EUR", const "EUR" ),
  ( "Umsatz in EUR", const "Johannes Gerer" ),
  ( "Buchungstext", id),
  ( "Vorgang",id)] ++ empty (32-10-2)
    where empty x = take x $ repeat ( "Buchungstag" , const "")

-- comdirectVisaCSVImport :: AccountMap -> T.Text -> [ImportedEntry]
-- comdirectVisaCSVImport accountMappings =
--   aqbankingCsvImport accountMappings . toAqbanking2Pure ';'
--   comdirect_header_visa comdirect_mapping_visa (const True)
--   . T.replace "\n\"Neu\";" "" . L.unlines . ok

ok :: (IsString b, Eq b, L.StringLike b) => b -> [b]
ok = L.takeWhile (/= fromString "")
     . L.dropWhile (/= fromString "\"Buchungstag\";\"Umsatztag\";\"Vorgang\";\"Referenz\";\"Buchungstext\";\"Umsatz in EUR\";")
     . L.lines


p :: ParsecT [Char] u Identity [Char]
p = do a <- C.manyTill C.anyChar (C.try $ C.string "\n\"Neu\";")
       return a

-- comdirectVisaImporter :: CustomImport2
-- comdirectVisaImporter = Importer windoof comdirectVisaCSVImport


-- paypalImport :: AccountMap -> T.Text -> [ImportedEntry]
-- paypalImport accountMappings = aqbankingCsvImport accountMappings . myf2 ','
--                paypal_header (paypal_mapping " Brutto" "") filt
--   where filt x = not $ "Storniert" `elem` x
--         myf2 sep header mapping' filtercond =
--          T.intercalate "\n" . show2 . (:) csv_header
--          . map appl . filter filtercond . drop 1 . (readcsv sep)
--                   where appl = map (\(a,b) -> a b) . zip transformation . description_list header mapping
--                         mapping = map fst mapping'

hbci_sep = ';'

-- | Descriptions create the description, by concatenation of all cols
description
  :: (L.ListLike c item, L.StringLike c, Show a, Eq a) =>
     [a] -> [c] -> c
description cols r = L.unwords . filter (not . L.null) $ description_list csv_header
  cols r

description_list :: (Show a, Eq a) => [a] -> [a] -> [b] -> [b]
description_list t cols r = map fst $ sorted r
  where
    desc_indices  = map (idx t)  cols  -- check if all desired columns exist
    magic v k = do i <- elemIndices k desc_indices
                   return (v,i)
    -- for every col in desc_cols get a pair containing the value and
    -- the index in the desc_cols arrays
    sorted r = sortBy (comparing snd) $ mconcat $ zipWith magic r [0..]
    -- extract all desc_cols in the specified order

                        -- transformation = map snd mapping'
csv_header = undefined

-- * PNC Bank USA transaction logs

pncbankImporter :: Importer T.Text
pncbankImporter = csvImport pncbank

pncbank :: VersionedCSV T.Text
pncbank = toVersionedCSV (SFormat "pncbank" $ DefaultVersion "May 2017")
  [CSV
        { cFilter  =(/= "") . getCsv "Date"
        , cDate = parseDateUS . getCsv "Date"
        , cStrip = False
        , cVDate = Just . parseDateUS . getCsv "Date"
        , cBank = const $ const "PNC Bank"
        , cPostings =
          [ \env -> CsvPosting
            { cAccount = const env
            , cAmount = textstrip . (T.replace "$" "") .
                        (T.replace "," "") . (<> " USD") .
                        getCsvCreditDebit "Withdrawals" "Deposits"
            , cSuffix = Nothing
            , cNegate = const False
            }]
        , cSeparator = ','
        , cHeader = ["Date"
                    ,"Description"
                    ,"Withdrawals"
                    ,"Deposits"
                    ,"Balance"
                    ]
        , cDescription = Field <$> desc
        , cBayes = desc
        , cVersion = "May 2017"
        , cGetContents = windoof
        }
  ]
  where desc = ["Description"]


-- * Barclaycard US transaction logs

barclaycardusImporter :: Importer ()
barclaycardusImporter = csvImportPreprocessed barclaycardPreprocessor barclaycardus

barclaycardPreprocessor :: Preprocessor () AccountId
barclaycardPreprocessor (wholeFile, _) =
    maybe e (return . (,) (T.unlines body) . AccountId bank . T.dropWhile (== 'X'))
    $ T.stripPrefix accountPrefix accountLine
  where
    (bank : accountLine : _ : _ : body) = T.lines wholeFile
    accountPrefix = "Account Number: "
    e = throwError $
      "Expected second line of file to begin with prefix " `T.append` accountPrefix

barclaycardus :: VersionedCSV AccountId
barclaycardus = toVersionedCSV (SFormat "barclaycard" $ DefaultVersion "May 2017")
  [CSV
        { cFilter  =(/= "") . getCsv "Transaction Date"
        , cDate = parseDateUS . getCsv "Transaction Date"
        , cStrip = False
        , cVDate = Just . parseDateUS . getCsv "Transaction Date"
        , cBank = const . aBank
        , cPostings =
          [ \env -> CsvPosting
            { cAccount = const $ aAccount env
            , cAmount = textstrip . (<> " USD") . getCsv "Amount"
            , cSuffix = Nothing
            , cNegate = const False
            }]
        , cSeparator = ','
        , cHeader = ["Transaction Date"
                    ,"Description"
                    ,"Category"
                    ,"Amount"
                    ]
        , cDescription = Field <$> desc
        , cBayes = "Category" : desc
        , cVersion = "May 2017"
        , cGetContents = windoof
        }
  ]
  where desc = ["Description"]

-- * Revolut Csv
  
revolutImporter :: Importer (RevolutSettings ())
revolutImporter = csvImportPreprocessed extractCurrency revolut

extractCurrency :: Preprocessor (RevolutSettings ()) (RevolutSettings T.Text)
extractCurrency (text, env) = do
  cur <- maybe (throwError $ "Cannot find currency (regular expression: "
                 <> T.pack (show currencyColumn) <> " in header:\n" <> header)
         (return . fst) $ (flip atMay 1 . toList =<<)
         $ listToMaybe $ currencyRegex header
  return (T.unlines $ T.replace (" (" <> cur <> ")") "" header:rest
         , const cur <$> env)
  where header:rest = T.lines text 

currencyRegex = R.matchAllText (R.makeRegex currencyColumn :: R.Regex)

currencyColumn = "\\bPaid Out \\(([^)]+)\\)" :: T.Text 
  

revolut :: VersionedCSV (RevolutSettings T.Text)
revolut = toVersionedCSV (SFormat "revolut" $ DefaultVersion "2017")
  [ v2017,
    v2017 { cHeader = ["Completed Date"
                    ,"Reference"
                    ,"Paid Out"
                    ,"Paid In"
                    ,"Exchange Out"
                    ,"Exchange In"
                    ,"Balance"
                    ,"Category"
                    ,"Notes"
                    ]
        , cDescription = [Const "Revolut"
                         , Field "Reference"
                         , Const ", Category"
                         , Field "Notes"
                         , Field "Category" ]
        , cBayes = ["Reference", "Category", "Notes" ]
        , cVersion = "Apr 2018"
        }
  ]
  where v2017 = 
          CSV { cFilter  = (/= "") . getCsv "Completed Date"
              , cDate = (\x -> headNote ("no parse of " <> T.unpack x) $ mapMaybe
                          (\format -> parseDateM format x) ["%b %e, %Y", "%e %b %Y"]) . getCsv "Completed Date"
              , cStrip = True
              , cVDate = const Nothing
              , cBank = const $ const "Revolut"
              , cPostings =
                [ \env -> CsvPosting
                  { cAccount = const $ revolutUser env
                  , cAmount = (<> revolutCurrency env) . getCsvCreditDebit "Paid Out" "Paid In"
                  , cSuffix = Just $ const $ revolutCurrency env
                  , cNegate = const False
                  }
                ]
              , cSeparator = ';'
              , cHeader = ["Completed Date"
                          ,"Reference"
                          ,"Paid Out"
                          ,"Paid In"
                          ,"Exchange Out"
                          ,"Exchange In"
                          ,"Balance"
                          ,"Category"
                          ,"Notes"
                          ]
              , cDescription = [Const "Revolut"
                               , Field "Reference"
                               , Const ", Category"
                               , Field "Notes"
                               , Field "Category" ]
              , cBayes = ["Reference", "Category", "Notes" ]
              , cVersion = "2017"
              , cGetContents = T.hGetContents
              }
  
-- remove currency signs and append corresponding currency name
normalizeCurrency :: T.Text -> T.Text
normalizeCurrency text = (`runCont` id) $ callCC $ \exit -> do
  let g (symbol, name) text1 = unless (T.length text2 == L.length text1)
        $ exit $ T.replace " " "" text2 <> " " <> name
        where text2 = T.replace symbol "" text1
  mapM_ (\x -> g x text) currencySymbols
  return text

currencySymbols = [ ("$", "USD")
                  , ("€", "EUR")
                  , ("£", "GBP") ]
  
-- * Monefy Csv
  
monefyImporter :: Importer MonefySettings
monefyImporter = csvImportPreprocessed unambiguousHeader monefy

-- | replace the header by one with unique column names (currency2)
unambiguousHeader :: Preprocessor env env
unambiguousHeader = processLines $ (h2:) . tail
  where h2 = "date,account,category,amount,currency,converted amount,currency2,description"
             -- (T.replace "ü" "ue" <$> rest)

monefy :: VersionedCSV MonefySettings
monefy = toVersionedCSV (SFormat "monefy" $ DefaultVersion "2017")
  [CSV
        { cFilter  = const True
        , cStrip = False
        , cDate = parseDate "%d/%m/%Y" . getCsv "date"
        , cVDate = const Nothing
        , cBank = const . monefyInstallation
        , cPostings =
          [ const CsvPosting
            { cAccount = getCsv "account"
            , cAmount = amt
            , cSuffix = Nothing
            , cNegate = const False
            }
          , \env -> let suf = monefyCategorySuffix env in CsvPosting 
            { cAccount = if suf then const "Monefy Category Account"
                         else getCsv "category"
            , cAmount = amt
            , cSuffix = if suf then Just $ getCsv "category"
                        else Nothing
            , cNegate = const True
            }
          ]
        , cSeparator = ','
        , cHeader = ["date"
                    ,"account"
                    ,"category"
                    ,"amount"
                    ,"currency"
                    ,"converted amount"
                    ,"currency2"
                    ,"description"
                    ]
        , cDescription = [Const "Monefy"
                         , Read monefyInstallation
                         , Field "description"]
        , cBayes = []
        , cVersion = "2017"
        , cGetContents = T.hGetContents
        }
  ]
  where amt = (\a b -> textstrip $ T.replace "," "" a <> " " <> b)
              <$> getCsv "amount" <*> getCsv "currency"

-- * BarclaysUk International CSV export

barclaysUkImporter :: Importer ()
barclaysUkImporter = csvImport barclaysUk
  
barclaysUk :: VersionedCSV ()
barclaysUk = toVersionedCSV (SFormat "barclaysUk" $ DefaultVersion "2017")
  [CSV
        { cFilter = (/= "") . getCsv "Date"
        , cStrip = False
        , cDate = parseDate "%d/%m/%Y" . getCsv "Date"
        , cVDate = const Nothing
        , cBank = const $ fst <$> accountBank
        , cPostings =
          [ const CsvPosting
            { cAccount = snd <$> accountBank
            , cAmount = (<> " GBP") <$> getCsv "Amount"
            , cSuffix = Nothing
            , cNegate = const False
            }
          ]
        , cSeparator = ','
        , cHeader = ["Number"
                    ,"Date"
                    ,"Account"
                    ,"Amount"
                    ,"Subcategory"
                    ,"Memo"
                    ]
        , cDescription = Field <$> desc 
        , cBayes = desc
        , cVersion = "2017"
        , cGetContents = T.hGetContents
        }
  ]
  where accountBank = (g . T.splitOn " ") . getCsv "Account"
        g [acc,bank] = (acc, bank)
        g _ = error "Expected 'Account' to be of the format \"{sort code} {account number}\""
        desc = ["Subcategory", "Memo"]

-- * Natwest International CSV export

natwestIntlImporter :: Importer ()
natwestIntlImporter = csvImportPreprocessed (processLines $ tail) natwestIntl
  
natwestIntl :: VersionedCSV ()
natwestIntl = toVersionedCSV (SFormat "natwestIntl" $ DefaultVersion "2017")
  [CSV
        { cFilter = (/= "") . getCsv "Date"
        , cStrip = False
        , cDate = parseDate "%d/%m/%Y" . getCsv "Date"
        , cVDate = const Nothing
        , cBank = const $ fst <$> accountBank
        , cPostings =
          [ const CsvPosting
            { cAccount = snd <$> accountBank
            , cAmount = (<> " GBP") <$> getCsv " Value"
            , cSuffix = Nothing
            , cNegate = const False
            }
          ]
        , cSeparator = ','
        , cHeader = ["Date"
                    ," Type"
                    ," Description"
                    ," Value"
                    ," Balance"
                    ," Account Name"
                    ," Account Number"
                    ]
        , cDescription = [Field " Description"
                         ,Const ", Type"
                         ,Field " Type"]
        , cBayes = [" Description"
                   ," Type"]
        , cVersion = "2017"
        , cGetContents = T.hGetContents
        }
  ]
  where accountBank = (g . T.splitOn "-" . T.tail ) . getCsv " Account Number"
        g [acc,bank] = (acc, bank)
        g _ = error "Expected ' Account Number' to be of the format \"'{sort code}-{account number}\""

-- natwestTransactionType = 
--           [("103"   ,"MT103 Payment")
--           ,("ACI"   ,"Interest on Account Balance")
--           ,("ADV"   ,"Separate Advice")
--           ,("AMD"   ,"Amendments History")
--           ,("ATM"   ,"Cash Withdrawal")
--           ,("BAC"   ,"Automated Credit")
--           ,("BAE"   ,"Branch Account Entry")
--           ,("BCO"   ,"Non Market Close Out")
--           ,("BGC"   ,"Bank Giro Credit")
--           ,("BGT"   ,"Guarantees")
--           ,("BLN"   ,"Bankline Charges")
--           ,("BOE"   ,"Bill of Exchange")
--           ,("BSP"   ,"Branch single payment")
--           ,("C/R"   ,"Credit")
--           ,("C/L"   ,"Automated teller machine cash withdrawal")
--           ,("CAE"   ,"Cheque Collection")
--           ,("CCB"   ,"Cheque Collection")
--           ,("CDM"   ,"Cash and Deposit Machine")
--           ,("CHG"   ,"Charges")
--           ,("CHP"   ,"CHAPS Transfer (NatWest only)")
--           ,("CHQ"   ,"Cheque")
--           ,("CNA"   ,"Clean Cheque Neg")
--           ,("CND"   ,"Cheque Negotiation")
--           ,("COM"   ,"Commission")
--           ,("CRD"   ,"Card Payment or Cash")
--           ,("D/D"   ,"Direct Debit")
--           ,("D/R"   ,"Debit")
--           ,("DCR"   ,"Documentary Credit")
--           ,("DFT"   ,"Foreign Draft")
--           ,("DIV"   ,"Dividend")
--           ,("DPC"   ,"Digital Banking Payment")
--           ,("EBP"   ,"Electronic Payment")
--           ,("FPAY"  ,"Faster Payment - Future Dated (Appears on statements as EBP)")
--           ,("GSD"   ,"Gov Stamp Duty")
--           ,("IBP"   ,"Inter Branch Payment")
--           ,("ICP"   ,"Inward Currency Payment")
--           ,("INT"   ,"Interest")
--           ,("INV"   ,"Investment")
--           ,("IPAY"  ,"Faster Payment -Immediate (Appears on statements as EBP)")
--           ,("ISP"   ,"Inward Sterling Payment")
--           ,("ITL"   ,"International Transfer & Treasury Settlements (and RBS CHAPS Payments)")
--           ,("ITM"   ,"Incoming CHAPS")
--           ,("LON"   ,"New Loan")
--           ,("LST"   ,"Supplementary List")
--           ,("LVP"   ,"Low Value Payment")
--           ,("MEC"   ,"Export Credits")
--           ,("MFD"   ,"Maturing Fwd Deal")
--           ,("MGT"   ,"Bonds & Guarantees")
--           ,("MIB"   ,"Inward Bills")
--           ,("MIC"   ,"Import Credits")
--           ,("MKD"   ,"Market Deal")
--           ,("MOB"   ,"Outward Bills")
--           ,("MSC"   ,"Miscellaneous Entry")
--           ,("NDC"   ,"No Dividend Counterfoil")
--           ,("NPAY"  ,"Faster Payment - Next Day (Appears on statement as EBP)")
--           ,("POS"   ,"Maestro Transaction")
--           ,("RTF"   ,"Relay Transfer")
--           ,("S/O"   ,"Standing Order")
--           ,("SBT"   ,"Funds Transfer")
--           ,("SCR"   ,"Sundry Credit Item")
--           ,("SDE"   ,"Urgent Euro Transfer")
--           ,("SDR"   ,"Sundry Debit Item")
--           ,("STF"   ,"Manually Keyed Standard Transfer")
--           ,("STL"   ,"Settlement")
--           ,("TFP"   ,"Trade Finance Product")
--           ,("TFR"   ,"Transfer")
--           ,("TRF"   ,"International Payment (NatWest only)")
--           ,("TLR"   ,"Card Payment or Cash")
--           ,("TEL"   ," Telephone Banking transaction")
--           ,("TSU"   ,"Telephone Banking")
--           ,("U/D"   ,"Unpaid Direct Debit")
--           ,("UTF"   ,"Urgent Transfer")
--           ,("WSF"   ,"Foreign Exchange Deal")
--           ,("WSM"   ,"Money Market Deal")]

-- * Comdirect Visa Statements

comdirectVisaImporter :: Importer T.Text
comdirectVisaImporter = csvImport comdirectVisa


comdirectVisa :: VersionedCSV T.Text
comdirectVisa = toVersionedCSV (SFormat "visa" $ DefaultVersion "manuell")
  [CSV
        { cFilter  =(/= "") . getCsv "Buchungstag"
        , cStrip = False
        , cDate = parseDateDE . getCsv "Buchungstag"
        , cVDate = Just . parseDateDE . getCsv "Valuta"
        , cBank = const
        , cPostings =
          [ const CsvPosting
            { cAccount = const "Visa"
            , cAmount = textstrip . comma . (<> " EUR") . getCsv "Ausgang"
            , cSuffix = Nothing
            , cNegate = const False
            }]
        , cSeparator = ','
        , cHeader = ["Buchungstag"
                    ,"Vorgang"
                    ,"Buchungstext"
                    ,"Ausgang"
                    ,"Valuta"
                    ,"Referenz"
                    ,"Buchungstext2"
                    ]
        , cDescription = Field <$> desc
        , cBayes = desc
        , cVersion = "manuell"
         -- hand extracted from @pdftotext -layout@
        , cGetContents = windoof
        }
  , CSV
        { cFilter  =(/= "") . getCsv "Buchungstag"
        , cDate = parseDateDE . getCsv "Buchungstag"
        , cStrip = False
        , cVDate = Just . parseDateDE . getCsv "Umsatztag"
        , cBank = const
        , cPostings =
          [ const CsvPosting
            { cAccount = const "Visa"
            , cAmount = comma . (<> " EUR") . getCsv "Umsatz in EUR"
            , cSuffix = Nothing
            , cNegate = const False
            }]
        , cSeparator = ','
        , cHeader = ["Buchungstag"
                    ,"Umsatztag"
                    ,"Vorgang"
                    ,"Referenz"
                    ,"Buchungstext"
                    ,"Umsatz in EUR"]
        , cDescription = Field <$> desc2
        , cBayes = desc2
        , cVersion = "export"
        , cGetContents = windoof
        }
  ]
  where desc = ["Vorgang"
                ,"Buchungstext"
                ,"Buchungstext2"
                ]
        desc2 = ["Vorgang"
                ,"Buchungstext"
                ]

-- * Paypal (German)
--
-- understands exports under the following setting:
--
-- @
-- alle guthaben relevanten Zahlungen (kommagetrennt) ohne warenkorbdetails!
-- @

paypalImporter :: Importer T.Text
paypalImporter = csvImport paypalImport

paypalImport :: VersionedCSV T.Text
paypalImport =
  let base2 state net ccy = CSV
        { cFilter  = (/= "Storniert") . getCsv state
        , cDate = parseDateDE . getCsv "Datum"
        , cStrip = False
        , cVDate = const Nothing
        , cBank = const $ const "Paypal"
        , cPostings =
          [ \env -> CsvPosting
            { cAccount = const env
            , cAmount = comma . getCsvConcat [net, ccy]
            , cSuffix = Nothing
            , cNegate = const False
            }]
        , cSeparator = ','
        , cVersion = "undefined"
        , cHeader = []
        , cBayes = ["undefined"]
        , cDescription = [Field "undefined"]
        , cGetContents = windoof
        }
      base = base2 " Status" " Netto" " W\228hrung"
      desc = Field <$> [" Name"
                       ," Verwendungszweck"
                       ," Art"
                       ," Zeit"]
      desc2 = Field <$> [" Name"
                       ," Artikelbezeichnung"
                       ," Typ"
                       ," Zeit"]
  in toVersionedCSV (SFormat "paypal" $ DefaultVersion "2018")
  [(base2 "Status" "Netto" "W\228hrung") { cVersion = "2018"
        , cGetContents = \h -> do hSetEncoding h utf8_bom
                                  T.hGetContents h
        , cHeader =
            ["Datum"
            ,"Uhrzeit"
            ,"Zeitzone"
            ,"Name"
            ,"Typ"
            ,"Status"
            ,"W\228hrung"
            ,"Brutto"
            ,"Geb\252hr"
            ,"Netto"
            ,"Absender E-Mail-Adresse"
            ,"Empf\228nger E-Mail-Adresse"
            ,"Transaktionscode"
            ,"Lieferadresse"
            ,"Adress-Status"
            ,"Artikelbezeichnung"
            ,"Artikelnummer"
            ,"Versand- und Bearbeitungsgeb\252hr"
            ,"Versicherungsbetrag"
            ,"Umsatzsteuer"
            ,"Option 1 Name"
            ,"Option 1 Wert"
            ,"Option 2 Name"
            ,"Option 2 Wert"
            ,"Zugeh\246riger Transaktionscode"
            ,"Rechnungsnummer"
            ,"Zollnummer"
            ,"Anzahl"
            ,"Empfangsnummer"
            ,"Guthaben"
            ,"Adresszeile 1"
            ,"Adresszusatz"
            ,"Ort"
            ,"Bundesland"
            ,"PLZ"
            ,"Land"
            ,"Telefon"
            ,"Betreff"
            ,"Hinweis"
            ,"L\228ndervorwahl"
            ,"Auswirkung auf Guthaben"]
        , cBayes = ["Name"
                   ,"Absender E-Mail-Adresse"
                   ,"Empf\228nger E-Mail-Adresse"
                   ,"Artikelbezeichnung"
                   ,"Typ"
                   ,"Status"
                   ,"Adress-Status"
                   ,"Adresszeile 1"
                   ,"Ort"
                   ,"PLZ"
                   ,"Land"
                   ,"Bundesland"
                   ,"Telefon"
                   ,"Betreff"
                   ,"Hinweis"
                   ]
        , cDescription = Field <$> ["Name"
                       ,"Artikelbezeichnung"
                       ,"Typ"
                       ,"Uhrzeit"]
        },
   base { cVersion = "2017"
        , cHeader = ["Datum"
                    ," Zeit" 
                    ," Zeitzone" 
                    ," Name" 
                    ," Typ" 
                    ," Status" 
                    ," Betreff" 
                    ," W\195\164hrung" 
                    ," Brutto" 
                    ," Geb\195\188hr" 
                    ," Netto" 
                    ," Hinweis" 
                    ," Von E-Mail-Adresse" 
                    ," An E-Mail-Adresse" 
                    ," Transactionscode" 
                    ," Zahlungsart" 
                    ,"Status der Gegenpartei" 
                    ," Lieferadresse" 
                    ," Adressstatus" 
                    ," Artikelbezeichnung" 
                    ," Artikelnummer" 
                    ," Betrag f\195\188r Versandkosten" 
                    ," Versicherungsbetrag" 
                    ," Umsatzsteuer" 
                    ," Trinkgeld" 
                    ," Rabatt" 
                    ," Mitgliedsname des Verk\195\164ufers" 
                    ," Option 1 - Name" 
                    ," Option 1 - Wert" 
                    ," Option 2 - Name" 
                    ," Option 2 - Wert" 
                    ," Auktions-Site" 
                    ," K\195\164ufer-ID" 
                    ," Artikel-URL" 
                    ," Angebotsende" 
                    ," Txn-Referenzkennung" 
                    ," Rechnungsnummer" 
                    ," Abonnementnummer" 
                    ," Individuelle Nummer" 
                    ," Belegnummer" 
                    ," Guthaben" 
                    ," Adresszeile 1" 
                    ," Zus\195\164tzliche Angaben" 
                    ," Ort" 
                    ," Staat/Provinz/Region/Landkreis/Territorium/Pr\195\164fektur/Republik" 
                    ," PLZ" 
                    ," Land" 
                    ," Telefonnummer" 
                    ," Auswirkung auf Guthaben" 
                    ," "]
        , cBayes = [" Name"
                   ," An E-Mail-Adresse"
                   ," Von E-Mail-Adresse"
                   ," Artikelbezeichnung"
                   ," Typ"
                   ," Status"
                   ," K\195\164ufer-ID"
                   , "Status der Gegenpartei"," Adressstatus"
                   , " Option 1 - Name"
                   , " Option 2 - Name"
                   ," Auktions-Site"
                   ," K\195\164ufer-ID"
                   ," Artikel-URL"
                   ," Adresszeile 1"
                   ," Zus\195\164tzliche Angaben"
                   ," Ort"
                   ," Staat/Provinz/Region/Landkreis/Territorium/Pr\195\164fektur/Republik"
                   ," PLZ"
                   ," Land"
                   ," Telefonnummer"
                   ]
        , cDescription = desc2
        }
  ,base { cVersion = "2016"
        , cHeader = ["Datum"
                    ," Zeit"
                    ," Zeitzone"
                    ," Name"
                    ," Typ"
                    ," Status"
                    ," W\228hrung"
                    ," Brutto"
                    ," Geb\252hr"
                    ," Netto"
                    ," Von E-Mail-Adresse"
                    ," An E-Mail-Adresse"
                    ," Transactionscode"
                    ," Status der Gegenpartei"
                    ," Adressstatus"
                    ," Artikelbezeichnung"
                    ," Artikelnummer"
                    ," Betrag f\252r Versandkosten"
                    ," Versicherungsbetrag"
                    ," Umsatzsteuer"
                    ," Option 1 - Name"
                    ," Option 1 - Wert"
                    ," Option 2 - Name"
                    ," Option 2 - Wert"
                    ," Auktions-Site"
                    ," K\228ufer-ID"
                    ," Artikel-URL"
                    ," Angebotsende"
                    ," Vorgangs-Nr."
                    ," Rechnungs-Nr."
                    ," Txn-Referenzkennung"
                    ," Rechnungsnummer"
                    ," Individuelle Nummer"
                    ," Belegnummer"
                    ," Guthaben"
                    ," Adresszeile 1"
                    ," Zus\228tzliche Angaben"
                    ," Ort"
                    ," Staat/Provinz/Region/Landkreis/Territorium/Pr\228fektur/Republik"
                    ," PLZ"
                    ," Land"
                    ," Telefonnummer"
                    ," "]
        , cBayes = [" Name"
                   ," An E-Mail-Adresse"
                   ," Von E-Mail-Adresse"
                   ," Artikelbezeichnung"
                   ," Typ"
                   ," Status"
                   ," Käufer-ID"
                   , " Status der Gegenpartei"," Adressstatus"
                   , " Option 1 - Name"
                   , " Option 2 - Name"
                   ," Auktions-Site"
                   ," K\228ufer-ID"
                   ," Artikel-URL"
                   ," Adresszeile 1"
                   ," Zus\228tzliche Angaben"
                   ," Ort"
                   ," Staat/Provinz/Region/Landkreis/Territorium/Pr\228fektur/Republik"
                   ," PLZ"
                   ," Land"
                   ," Telefonnummer"
                   ]
        , cDescription = desc2
        }
    ,base { cVersion = "2014"
         , cHeader = ["Datum"
                     ," Zeit"
                     ," Zeitzone"
                     ," Name"
                     ," Art"
                     ," Status"
                     ," W\228hrung"
                     ," Brutto"
                     ," Geb\252hr"
                     ," Netto"
                     ," Von E-Mail-Adresse"
                     ," An E-Mail-Adresse"
                     ," Transaktionscode"
                     ," Status der Gegenpartei"
                     ," Adressstatus"
                     ," Verwendungszweck"
                     ," Artikelnummer"
                     ," Betrag f\252r Versandkosten"
                     ," Versicherungsbetrag"
                     ," Umsatzsteuer"
                     ," Option 1 - Name"
                     ," Option 1 - Wert"
                     ," Option 2 - Name"
                     ," Option 2 - Wert"
                     ," Auktions-Site"
                     ," K\228ufer-ID"
                     ," Artikel-URL"
                     ," Angebotsende"
                     ," Vorgangs-Nr."
                     ," Rechnungs-Nr."
                     ," Txn-Referenzkennung"
                     ," Rechnungsnummer"
                     ," Individuelle Nummer"
                     ," Best\228tigungsnummer"
                     ," Guthaben"
                     ," Adresse"
                     ," Zus\228tzliche Angaben"
                     ," Ort"
                     ," Staat/Provinz/Region/Landkreis/Territorium/Pr\228fektur/Republik"
                     ," PLZ"
                     ," Land"
                     ," Telefonnummer der Kontaktperson"
                     ," "]
         , cBayes = [" Name"
                    ," An E-Mail-Adresse"
                    ," Von E-Mail-Adresse"
                    ," Verwendungszweck"
                    ," Art"
                    ," Status"
                    ," Käufer-ID"
                    , " Status der Gegenpartei"," Adressstatus"
                    , " Option 1 - Name"
                    , " Option 2 - Name"
                    ," Auktions-Site"
                    ," K\228ufer-ID"
                    ," Artikel-URL"
                    ," Adresse"
                    ," Zus\228tzliche Angaben"
                    ," Ort"
                    ," Staat/Provinz/Region/Landkreis/Territorium/Pr\228fektur/Republik"
                    ," PLZ"
                    ," Land"
                    ," Telefonnummer der Kontaktperson"
                    ]
        , cDescription = desc
         }
  , base { cVersion = "2013"
         , cHeader = ["Datum"
                     ," Zeit"
                     ," Zeitzone"
                     ," Name"
                     ," Art"
                     ," Status"
                     ," W\228hrung"
                     ," Brutto"
                     ," Geb\252hr"
                     ," Netto"
                     ," Von E-Mail-Adresse"
                     ," An E-Mail-Adresse"
                     ," Transaktionscode"
                     ," Status der Gegenpartei"
                     ," Adressstatus"
                     ," Verwendungszweck"
                     ," Artikelnummer"
                     ," Betrag f\252r Versandkosten"
                     ," Versicherungsbetrag"
                     ," Umsatzsteuer"
                     ," Option 1 - Name"
                     ," Option 1 - Wert"
                     ," Option 2 - Name"
                     ," Option 2 - Wert"
                     ," Auktions-Site"
                     ," K\228ufer-ID"
                     ," Artikel-URL"
                     ," Angebotsende"
                     ," Vorgangs-Nr."
                     ," Rechnungs-Nr."
                     ," Txn-Referenzkennung"
                     ," Rechnungsnummer"
                     ," Individuelle Nummer"
                     ," Menge"
                     ," Best\228tigungsnummer"
                     ," Guthaben"
                     ," Adresse"
                     ," Zus\228tzliche Angaben"
                     ," Ort"
                     ," Staat/Provinz/Region/Landkreis/Territorium/Pr\228fektur/Republik"
                     ," PLZ"
                     ," Land"
                     ," Telefonnummer der Kontaktperson"
                     ," "]
         , cBayes = [" Name"
                    ," An E-Mail-Adresse"
                    ," Von E-Mail-Adresse"
                    ," Verwendungszweck"
                    ," Art"
                    ," Status"
                    ," Käufer-ID"
                    ," Status der Gegenpartei"," Adressstatus"
                    ," Option 1 - Name"
                    ," Option 2 - Name"
                    ," Auktions-Site"
                    ," K\228ufer-ID"
                    ," Artikel-URL"
                    ," Adresse"
                    ," Zus\228tzliche Angaben"
                    ," Ort"
                    ," Staat/Provinz/Region/Landkreis/Territorium/Pr\228fektur/Republik"
                    ," PLZ"
                    ," Land"
                    ," Telefonnummer der Kontaktperson"
                    ]
        , cDescription = desc
         }]

-- * Paypal (English)

paypalEngImporter :: Importer T.Text
paypalEngImporter = csvImport paypalEngImport

paypalEngImport :: VersionedCSV T.Text
paypalEngImport =
  let base2 state net ccy = CSV
        { cFilter  = (/= "Canceled") . getCsv state
        , cDate = parseDate "%d/%m/%Y" . getCsv "Date"
        , cStrip = False
        , cVDate = const Nothing
        , cBank = const $ const "Paypal"
        , cPostings =
          [ \env -> CsvPosting
            { cAccount = const env
            , cAmount = getCsvConcat [net, ccy]
            , cSuffix = Nothing
            , cNegate = const False
            }]
        , cSeparator = ','
        , cVersion = "undefined"
        , cHeader = []
        , cBayes = ["undefined"]
        , cDescription = [Field "undefined"]
        , cGetContents = windoof
        }
      base = base2 " Status" " Net" " Currency"
  in toVersionedCSV (SFormat "paypalEng" $ DefaultVersion "2018")
  [(base2 "Status" "Net" "Currency") { cVersion = "2018"
        , cGetContents = \h -> do hSetEncoding h utf8_bom
                                  T.hGetContents h
        , cHeader =
            ["Date"
            ,"Time"
            ,"Time zone"
            ,"Name"
            ,"Type"
            ,"Status"
            ,"Currency"
            ,"Gross"
            ,"Fee"
            ,"Net"
            ,"From Email Address"
            ,"To Email Address"
            ,"Transaction ID"
            ,"Shipping Address"
            ,"Address Status"
            ,"Item Title"
            ,"Item ID"
            ,"Postage and Packaging Amount"
            ,"Insurance Amount"
            ,"VAT"
            ,"Option 1 Name"
            ,"Option 1 Value"
            ,"Option 2 Name"
            ,"Option 2 Value"
            ,"Reference Txn ID"
            ,"Invoice Number"
            ,"Custom Number"
            ,"Quantity"
            ,"Receipt ID"
            ,"Balance"
            ,"Address Line 1"
            ,"Address Line 2/District/Neighbourhood"
            ,"Town/City"
            ,"County"
            ,"Postcode"
            ,"Country"
            ,"Contact Phone Number"
            ,"Subject"
            ,"Note"
            ,"Country Code"
            ,"Balance Impact"]
        , cBayes = ["Name"
                   ,"From Email Address"
                   ,"To Email Address"
                   ,"Item Title"
                   ,"Type"
                   ,"Status"
                   ,"Address Status"
                   ,"Address Line 1"
                   ,"Town/City"
                   ,"Postcode"
                   ,"Country"
                   ,"County"
                   ,"Contact Phone Number"
                   ,"Subject"
                   ,"Note"
                   ]
        , cDescription = Field <$> ["Name"
                       ,"Item Title"
                       ,"Note"
                       ,"Type"
                       ,"Time"]
        }]

-- * other stuff


show2 :: [[T.Text]] -> [T.Text]
show2 = map (T.intercalate ";" . map (\x -> "\"" <> (quote x) <> "\""))
  where quote x = T.replace "\"" escapedDoubleQuotes x

hibiscusToAqbanking :: IO ()
hibiscusToAqbanking = toAqbanking ';'
  (T.readFile "/home/data/finanzen/imported/hibiscus-export-20120930.csv")
  hibicus_header hibiscus_mapping hibiscus_transf $ const True

toAqbankingPure
  :: (Show a, Eq a) =>
     Char
     -> [a]
     -> [a]
     -> [T.Text -> T.Text]
     -> ([T.Text] -> Bool)
     -> T.Text
     -> T.Text
toAqbankingPure sep header mapping transformation filtercond =
   T.intercalate "\n" . show2 . (:) csv_header
   . map appl . filter filtercond . drop 1 . (readcsv sep)
            where appl = map (\(a,b) -> a b) . zip transformation . description_list header mapping

toAqbanking
  :: (Show a, Eq a) =>
     Char
     -> IO T.Text
     -> [a]
     -> [a]
     -> [T.Text -> T.Text]
     -> ([T.Text] -> Bool)
     -> IO ()
toAqbanking sep f header mapping transf filt =
  T.putStr =<< toAqbankingPure sep header mapping transf filt <$> f

toAqbanking2Pure
  :: (Show a, Eq a) =>
     Char
     -> [a]
     -> [(a, T.Text -> T.Text)]
     -> ([T.Text] -> Bool)
     -> T.Text
     -> T.Text
toAqbanking2Pure sep header mapping filt = toAqbankingPure sep header mapping' transf filt
  where mapping' = map fst mapping
        transf = map snd mapping

toAqbanking2 sep f header mapping filt =
  T.writeFile "/tmp/new" =<< toAqbanking2Pure sep header mapping filt <$> f

hibicus_header :: [[Char]]
hibicus_header =["Kontonummer","BLZ","Konto","Gegenkonto","Gegenkonto BLZ","Gegenkonto Inhaber","Betrag","Valuta","Datum","Verwendungszweck","Verwendungszweck 2","Zwischensumme","Primanota","Kundenreferenz","Kategorie","Kommentar","Weitere Verwendungszwecke"]

hibiscus_mapping :: [[Char]]
hibiscus_mapping = ["Primanota","BLZ","Kontonummer","Gegenkonto BLZ","Gegenkonto","Datum","Valuta","Betrag"        , "Betrag"  , "Betrag"              , "Gegenkonto Inhaber", "Betrag", "Verwendungszweck","Verwendungszweck 2","Kommentar","Weitere Verwendungszwecke"] ++ (take 16 $ repeat "Betrag")

hibiscus_transf :: [T.Text -> T.Text]
hibiscus_transf  = [id,         id  , id          , id             , id         , fshow . readdate2, fshow . readdate2,comma ,const "EUR", const "Johannes Gerer", id                  , const ""] <> (take 4 $ repeat id) <> (take 16 $ repeat $ const "") :: [T.Text -> T.Text]

comma :: T.Text -> T.Text
comma  = T.replace "," "." . T.replace "." "" :: T.Text -> T.Text

readdate2 :: Stream t Data.Functor.Identity.Identity Char
          => t -> Date2
readdate2 s = either (error.show) id (parse p_date2 "date" s)

p_date2 :: Stream t Data.Functor.Identity.Identity Char
        => Parsec t () Date2
p_date2 = do d <- many1 digit
             char '.'
             m <- many1 digit
             char '.'
             y <- many1 digit
             return (D y m d)

readdate :: T.Text -> Day
readdate s = either (error.show) id (parse p_date "date" s)

p_date :: Stream t Data.Functor.Identity.Identity Char
        => Parsec t () Day
p_date = do y <- many1 digit
            char '/'
            m <- many1 digit
            char '/'
            d <- many1 digit
            return $ fromGregorian (read y) (read m) (read d)

instance Show Date2 where
  show (D y m d) = concat [y, "/", m, "/", d]

data Date2 = D String String String  -- year month day
  deriving (Eq)


toBayes
  :: MonadError Msg m
  => VersionedCSV a
  -> m (SFormat DefaultVersion, M.Map Version [T.Text])
toBayes = fmap (second $ fmap $ cBayes . cRaw)

-- create a map that associates each format with a map that associates
-- each version with a list of fields
defaultFields
  :: MonadError Msg m =>
     m (M.Map (SFormat ()) (M.Map Version [T.Text]))
defaultFields = fromListUnique . fmap (first $ (() <$))
  =<< sequence [toBayes paypalImport
               ,toBayes paypalEngImport
               ,toBayes aqbankingImport
               ,toBayes barclaycardus
               ,toBayes pncbank
               ,toBayes revolut
               ,toBayes natwestIntl
               ,toBayes barclaysUk
               ,toBayes monefy]

-- extract the values of all available bayes fields of a given
-- source.
getBayesFields
  :: MonadError Msg m
  => Source -> m [T.Text]
getBayesFields source = do
  -- extract the correct bayes fields that correspond to the source's
  -- format and version.
  fields <- lookupErrM "Version has to be configured" M.lookup
            (fVersion format)
            =<< lookupErrM "Format has to be configured" M.lookup
            (() <$ format)
            =<< defaultFields
  return $ mapMaybe (flip M.lookup $ sStore source) fields
    where format = sFormat source
