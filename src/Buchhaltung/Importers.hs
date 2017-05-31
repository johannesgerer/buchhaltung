{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Buchhaltung.Importers
(
  paypalImporter
  , aqbankingImporter
  , comdirectVisaImporter 
  , barclaycardusImporter 
  , module Buchhaltung.Import
  , getBayesFields
  )
where

import           Buchhaltung.Common
import           Buchhaltung.Import
import           Control.Arrow
import           Control.Monad.RWS.Strict
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
import           Formatting (sformat, (%), shown)
import qualified Formatting.ShortFormatters as F
import           Text.Parsec
import qualified Text.ParserCombinators.Parsec as C
import           Text.Printf

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
type Preprocessor env1 env2 = (T.Text, env1) -> (T.Text, env2)

csvImport = csvImportPreprossed id
  
csvImportPreprossed :: Preprocessor env1 env2
                    -> VersionedCSV env2
                    -> T.Text
                    -> CommonM (env1, Maybe Version) [ImportedEntry]
csvImportPreprossed pp versionedCsv csv1 = do
  (env, version) <- reader oEnv
  let (csv2, env2) = pp (csv1, env)
  (form, g@CSV{cHeader=expected,
     cDescription = desc,
     cVersion = version }) <- headerInfo versionedCsv version
  let toEntry x = ImportedEntry
          { ieT = genTrans date (vdate =<< cVDate g x)
                  (getCsvConcat desc x)
          , ieSource  = fromMapToSource form x
          , iePostings = (AccountId (cBank g env2 $ x) (cAccount g env2 $ x)
                         , cAmount g x)
          }
          where
            vdate vd = if date == vd then Nothing
                       else Just vd
            date  = cDate g x
      (header, rows) = parseCsv (cSeparator g) . TL.fromStrict $ csv2
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
aqbankingImporter = Importer Nothing $ csvImport aqbankingImport

aqbankingImport :: VersionedCSV env
aqbankingImport = toVersionedCSV (SFormat "aqBanking" $ DefaultVersion "4")
  [CSV
    { cFilter  = const True
    , cAmount = getCsvConcat [ "value_value"
                             , "value_currency"]
    , cDate = readdate . getCsv "date"
    , cVDate = Just . readdate . getCsv "valutadate"
    , cBank = const $ getCsv "localBankCode"
    , cAccount = const $ getCsv "localAccountNumber"
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
    , cDescription = desc
    , cBayes = ["remoteBankCode","remoteAccountNumber"]
               ++ desc
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

-- * Barclaycard US Visa transaction logs

barclaycardusImporter :: Importer T.Text
barclaycardusImporter = Importer windoof $ csvImport barclaycardus

barclaycardus :: VersionedCSV T.Text
barclaycardus = toVersionedCSV (SFormat "barclaycard" $ DefaultVersion "May 2017")
  [CSV
        { cFilter  =(/= "") . getCsv "Transaction Date" 
        , cAmount = textstrip . (<> " USD") . getCsv "Amount"
        , cDate = parseDateUS . getCsv "Transaction Date"
        , cVDate = Just . parseDateUS . getCsv "Transaction Date"
        , cBank = const $ const "Barclays Bank Delaware"
        , cAccount = const $ const "Barclaycard"
        , cSeparator = ','
        , cHeader = ["Transaction Date"
                    ,"Description"
                    ,"Category"
                    ,"Amount"
                    ]
        , cDescription = desc
        , cBayes = desc
        , cVersion = "May 2017"
        }
  ]
  where desc = ["Description"]

-- * Comdirect Visa Statements

comdirectVisaImporter :: Importer T.Text
comdirectVisaImporter = Importer windoof $ csvImport comdirectVisa
  

comdirectVisa :: VersionedCSV T.Text
comdirectVisa = toVersionedCSV (SFormat "visa" $ DefaultVersion "manuell")
  [CSV
        { cFilter  =(/= "") . getCsv "Buchungstag" 
        , cAmount = textstrip . comma . (<> " EUR") . getCsv "Ausgang"
        , cDate = parseDateDE . getCsv "Buchungstag"
        , cVDate = Just . parseDateDE . getCsv "Valuta"
        , cBank = const
        , cAccount = const $ const "Visa"
        , cSeparator = ','
        , cHeader = ["Buchungstag"
                    ,"Vorgang"
                    ,"Buchungstext"
                    ,"Ausgang"
                    ,"Valuta"
                    ,"Referenz"
                    ,"Buchungstext2"
                    ]
        , cDescription = desc
        , cBayes = desc
        , cVersion = "manuell"
         -- hand extracted from @pdftotext -layout@
        }
  , CSV
        { cFilter  =(/= "") . getCsv "Buchungstag" 
        , cAmount = comma . (<> " EUR") . getCsv "Umsatz in EUR"
        , cDate = parseDateDE . getCsv "Buchungstag"
        , cVDate = Just . parseDateDE . getCsv "Umsatztag"
        , cBank = const
        , cAccount = const $ const "Visa"
        , cSeparator = ','
        , cHeader = ["Buchungstag"
                    ,"Umsatztag"
                    ,"Vorgang"
                    ,"Referenz"
                    ,"Buchungstext"
                    ,"Umsatz in EUR"]
        , cDescription = desc2
        , cBayes = desc2
        , cVersion = "export"
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
paypalImporter = Importer windoof $ csvImport paypalImport

paypalImport :: VersionedCSV T.Text
paypalImport = 
  let base = CSV
        { cFilter  = (/= "Storniert") . getCsv " Status"
        , cAmount = comma . getCsvConcat [ " Netto"
                                         , " Währung"]
        , cDate = parseDateDE . getCsv "Datum"
        , cVDate = const Nothing
        , cBank = const $ const "Paypal"
        , cAccount = const
        , cSeparator = ','
        , cVersion = "undefined"
        , cHeader = []
        , cBayes = ["undefined"]
        , cDescription = ["undefined"]
        } in toVersionedCSV (SFormat "paypal" $ DefaultVersion "2016")
  [base { cVersion = "2016"
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
        , cDescription = [" Name"
                         ," Artikelbezeichnung"
                         ," Typ"
                         ," Zeit"]
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
         , cDescription = [" Name"
                          ," Verwendungszweck"
                          ," Art"
                          ," Zeit"]
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
         , cDescription = [" Name"
                          ," Verwendungszweck"
                          ," Art"
                          ," Zeit"]
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
  
defaultFields
  :: MonadError Msg m =>
     m (M.Map (SFormat ()) (M.Map Version [T.Text]))
defaultFields = fromListUnique . fmap (first $ (() <$))
  =<< sequence [toBayes paypalImport, toBayes aqbankingImport, toBayes barclaycardus]

getBayesFields
  :: MonadError Msg m
  => Source -> m [T.Text]
getBayesFields source = do
  fields <- lookupErrM "Version has to be configured" M.lookup
            (fVersion format)
            =<< lookupErrM "Format has to be configured" M.lookup
            (() <$ format)
            =<< defaultFields
  return $ mapMaybe (flip M.lookup $ sStore source) fields
    where format = sFormat source
