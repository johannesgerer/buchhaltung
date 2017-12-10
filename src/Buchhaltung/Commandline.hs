{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Buchhaltung.Commandline where

import           Buchhaltung.AQBanking
import           Buchhaltung.Add
import           Buchhaltung.Common
import           Buchhaltung.Importers
import           Buchhaltung.Match
import           Buchhaltung.OptionParsers
import           Control.Arrow
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad.RWS.Strict
import           Control.Monad.Reader
import           Data.Maybe
import qualified Data.Text as T
import           Hledger.Data (Journal)
import           Hledger.Read (readJournalFile)
import           Options.Applicative
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.Process
import           Text.Printf

runMain :: IO ()
runMain = do
  opts <- evaluate . force =<<
    customExecParser (prefs $ showHelpOnError <> showHelpOnEmpty)
    =<< mainParser
    :: IO (RawOptions ())
  let
    prog ::  ErrorT IO ()
    prog = do
        config <- liftIO $ readConfigFromFile $ oProfile opts
        run (oAction opts) =<< toFull opts config
  either (error . T.unpack) return =<< runExceptT prog

-- * Running Option Parsers and Actions

run :: Action -> FullOptions () -> ErrorT IO ()
run (Add partners) options =
  void $ runRWST add options{oEnv = partners} mempty

run (Import version file action) options = runImport action
  where runImport (Paypal puser) =
          importReadWrite paypalImporter (options' puser) file
        runImport NatwestIntl =
          importReadWrite natwestIntlImporter (options' ()) file
        runImport BarclaysUk =
          importReadWrite barclaysUkImporter (options' ()) file
        runImport BarclaycardUs =
          importReadWrite barclaycardusImporter (options' ()) file
        runImport (ComdirectVisa blz) =
          importReadWrite comdirectVisaImporter (options' blz) file
        runImport (Pncbank accountId) =
          importReadWrite pncbankImporter (options' accountId) file
        runImport (Revolut user) =
          importReadWrite revolutImporter (options' user) file
        runImport (Monefy settings) =
          importReadWrite monefyImporter (options' settings) file
        runImport AQBankingImport =
          importReadWrite aqbankingImporter (options' ()) file
        options' env = options{oEnv = (env, version)}

run (Update version doMatch doRequest) options = do
  res <- runAQ options $ aqbankingListtrans doRequest
  void $ runRWST
    (mapM (importWrite $ iImport aqbankingImporter) res)
    options{oEnv = ((), version)} ()
  when doMatch $ run Match options

run (Commit hledger args) options = flip runReaderT options $ do
  un <- readUser $  show . name
  dir <- takeDirectory <&> absolute =<< readLedger mainLedger
  bal <- lift $ runAQ options $ readAqbanking ["listbal"]
  sheet <- lift $ run readProcess' ["balance", "-e", "tomorrow"] options
  liftIO $ do setCurrentDirectory dir
              callProcess "git" $ "commit":args ++
                ["-m", intercalateL "\n" $ ["User " ++ un, ""]
                  ++ bal ++ ["Balance Sheet:", sheet]]
  where run = if hledger then runHledger else runLedger

run ListBalances options = void $ runAQ options $ callAqbanking ["listbal"]
  
run Setup options = void $ runAQ options aqbankingSetup

run Match options =
  withSystemTempDirectory "dbacl" $ \tmpdir -> 
  loadJournal [Just . imported] options >>= match options{oEnv = tmpdir}

run (AQBanking args) options = void $ runAQ options $ callAqbanking args

run (Ledger args) options = runLedger callProcess args options

run (HLedger args) options = runHledger callProcess args options

runLedger run = runLedger' run cLedgerExecutable mainLedger
runHledger run = runLedger' run cHledgerExecutable (maybe mainLedger const =<< mainHledger)

runLedger' run getExec getLedger args options = flip runReaderT options $ do
  exec <- readConfig getExec
  ledger <- absolute =<< readLedger getLedger
  liftIO $ do
    setEnv "LEDGER" ledger
    run exec args

