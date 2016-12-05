{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE OverloadedStrings #-}
module Buchhaltung.Importers
(
  paypalImporter
  , aqbankingImporter
  , module Buchhaltung.Import
  , askBayesFields
  )
where

import           Buchhaltung.Common
import           Buchhaltung.Import
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
import           Text.Parsec
import qualified Text.ParserCombinators.Parsec as C

-- * CSV

-- | retrieval function
type Getter a = MyRecord -> a

data CsvImport = CsvImport
  { cFilter :: MyRecord -> Bool
  -- ^ should this csv line be processed?
  , cFormat :: T.Text
  -- ^ Source Format 'sFormat'
  , cAmount :: Getter T.Text
  -- ^ Amount parsable by 'mamoumtp\''
  , cDescription :: Getter T.Text
  , cDate :: Getter Day
  , cVDate :: Getter (Maybe Day)
  , cBank :: Getter T.Text
  , cAccount :: Getter T.Text
  , cHeader :: Maybe [T.Text]
  -- ^ expected header
  , cSeparator :: Char
  }

csvImport
  :: MonadError Msg m
  => CsvImport
  -> T.Text -> m [ImportedEntry]
csvImport g csv = maybe result check $ cHeader g
  where f x = ImportedEntry
          { ieT = genTrans date (vdate =<< cVDate g x)
                  (cDescription g x)
          , ieSource  = fromMapToSource (cFormat g) x
          , iePostings = (AccountId (cBank g x) (cAccount g x)
                         , cAmount g x)
          }
          where
            vdate vd = if date == vd then Nothing
                       else Just vd
            date  = cDate g x
        (header, rows) = parseCsv (cSeparator g) . TL.fromStrict $ csv
        result = return $ fmap f $ filter (cFilter g) rows
        check h | h == header = result
                | True
          = throwError $ L.unlines
                  ["Headers do not match. Expected:"
                  ,fshow h
                  ,"Given:"
                  ,fshow header
                  ]

-- * AQBanking
--
-- imports output from @aqbankingcli listtrans@

aqbankingImporter :: Importer env
aqbankingImporter = Importer Nothing $ csvImport aqbankingImport

aqbankingImport :: CsvImport
aqbankingImport = CsvImport
        { cFilter  = const True
        , cFormat = "aqBanking"
        , cAmount = getCsvConcat [ "value_currency"
                                 , "value_value"]
        , cDescription = getCsvConcat aqBankingDescrFields
        , cDate = readdate . getCsv "date"
        , cVDate = Just . readdate . getCsv "valutadate"
        , cBank = getCsv "localBankCode"
        , cAccount = getCsv "localAccountNumber"
        , cHeader = Just $ aqbankingHeader 4
        , cSeparator = ';'
        }

aqBankingDescrFields = concatMap (\(f,i) -> (f <>) <$> "":i)
                       [ ("remoteName", ["1"])
                       , ("purpose",  fshow <$> [1..11])
                       , ("category", fshow <$> [1..7])]

aqBankingBayes = (cFormat aqbankingImport,
                ["remoteBankCode","remoteAccountNumber"]
                ++ aqBankingDescrFields)



aqbankingHeader :: IsString t => Int -- ^ version
                -> [t]
aqbankingHeader 4 = ["transactionId","localBankCode","localAccountNumber","remoteBankCode","remoteAccountNumber","date","valutadate","value_value","value_currency","localName","remoteName","remoteName1","purpose","purpose1","purpose2","purpose3","purpose4","purpose5","purpose6","purpose7","purpose8","purpose9","purpose10","purpose11","category","category1","category2","category3","category4","category5","category6","category7"]
aqbankingHeader v = versionError "aqbankingHeader" v

versionError :: String -> Int -> a
versionError h v =  error $ h ++ ": version " ++ show v ++ " not implemented"

-- * Postbank Germany Kontoauszüge (from PDF with @pdftotext@)

-- fromPostbankPDF2 :: T.Text -> [ImportedEntry]
-- fromPostbankPDF2 xx = fmap (f . mconcat) $ groupBy y $ readcsvrow ',' <$> L.lines xx
--   where y a b = head b==""
--         f :: [T.Text] -> ImportedEntry
--         f l@(dat:(_:(des:(am:rest)))) = ImportedEntry{
--            ieT = genTrans (parseDatum $ dat <> "2014") Nothing (L.unwords s)
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
--            ieT = genTrans (parseDatum $ dat <> "2013") Nothing (L.unwords s)
--            ,ieSource  = v $ T.intercalate (v $ T.singleton hbci_sep) l
--            ,iePostings=("Aktiva:Konten:Giro", a <> " EUR")
--            }
--           where s = (c des):rest
--                 c = L.unwords . tail . L.words
--                 a = mconcat $ L.words $ comma am
--                 v r = "\"" <> r <> "\""

-- * Comdirect Germany

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

-- * Paypal (German)
--
-- understands exports that used the foolowing setting:
--
-- @
-- alle guthaben relevanten Zahlungen (kommagetrennt) ohne warenkorbdetails!
-- @

paypalImporter :: Importer T.Text
paypalImporter = Importer windoof $ \text ->
  do imp <-reader $ paypalImport . oEnv
     csvImport imp text

paypalImport email = CsvImport
        { cFilter  = (/= "Storniert") . getCsv " Status"
        , cFormat = paypal_format
        , cAmount = comma . getCsvConcat [ " Netto"
                                         , " Währung"]
        , cDescription = getCsvConcat
                         [" Name"
                         -- ," An E-Mail-Adresse"
                         -- ," Von E-Mail-Adresse"
                         ," Verwendungszweck"
                         ," Art"
                         --," Status"
                         --," Käufer-ID"
                         ," Zeit"]
        , cDate = parseDatum . getCsv "Datum"
        , cVDate = const Nothing
        , cBank = const "Paypal"
        , cAccount = const email
        , cHeader = Just $ paypalHeader 2016
        , cSeparator = ','
        }
-- ++ if withGeb then [(  "Aktiva:Konten:Paypal", am geb)]
--    else []
-- withGeb = any (flip isInfixOf (getCsv geb x) . show ) [1..9]

paypal_format = "paypal"


paypal_bayes = (paypal_format,
  [" Name"
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
  ])

paypalHeader :: IsString t => Int -> [t]
paypalHeader 2016 = ["Datum"," Zeit"," Zeitzone"," Name"," Art"," Status"," W\228hrung"," Brutto"," Geb\252hr"," Netto"," Von E-Mail-Adresse"," An E-Mail-Adresse"," Transaktionscode"," Status der Gegenpartei"," Adressstatus"," Verwendungszweck"," Artikelnummer"," Betrag f\252r Versandkosten"," Versicherungsbetrag"," Umsatzsteuer"," Option 1 - Name"," Option 1 - Wert"," Option 2 - Name"," Option 2 - Wert"," Auktions-Site"," K\228ufer-ID"," Artikel-URL"," Angebotsende"," Vorgangs-Nr."," Rechnungs-Nr."," Txn-Referenzkennung"," Rechnungsnummer"," Individuelle Nummer"," Best\228tigungsnummer"," Guthaben"," Adresse"," Zus\228tzliche Angaben"," Ort"," Staat/Provinz/Region/Landkreis/Territorium/Pr\228fektur/Republik"," PLZ"," Land"," Telefonnummer der Kontaktperson"," "]

paypalHeader 2013 = ["Datum"," Zeit"," Zeitzone"," Name"," Art"," Status"," W\228hrung"," Brutto"," Geb\252hr"," Netto"," Von E-Mail-Adresse"," An E-Mail-Adresse"," Transaktionscode"," Status der Gegenpartei"," Adressstatus"," Verwendungszweck"," Artikelnummer"," Betrag f\252r Versandkosten"," Versicherungsbetrag"," Umsatzsteuer"," Option 1 - Name"," Option 1 - Wert"," Option 2 - Name"," Option 2 - Wert"," Auktions-Site"," K\228ufer-ID"," Artikel-URL"," Angebotsende"," Vorgangs-Nr."," Rechnungs-Nr."," Txn-Referenzkennung"," Rechnungsnummer"," Individuelle Nummer"," Menge"," Best\228tigungsnummer"," Guthaben"," Adresse"," Zus\228tzliche Angaben"," Ort"," Staat/Provinz/Region/Landkreis/Territorium/Pr\228fektur/Republik"," PLZ"," Land"," Telefonnummer der Kontaktperson"," "]

paypalHeader v = versionError "Paypal" v

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

defaultFormats :: HM.HashMap T.Text [T.Text]
defaultFormats = HM.fromList [ paypal_bayes
                             , aqBankingBayes]

askBayesFields
  :: (MonadError Msg m, MonadReader (Options user Config env) m) =>
     Source -> m [T.Text]
askBayesFields source = do
  formats <- readConfig cFormats
  fields <- lookupErrM "Format has to be configured" HM.lookup
            (sFormat source) $ HM.union formats defaultFormats
  return $ mapMaybe (flip M.lookup $ sStore source) fields
