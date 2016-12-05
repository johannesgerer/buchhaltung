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
  void $ withJournals [imported, addedByThisUser] options
  $ runRWST add options{oEnv = partners}

run (Import version file action) options = runImport action
  where runImport (Paypal puser) =
          importReadWrite paypalImporter (options' puser) file
        runImport (ComdirectVisa blz) =
          importReadWrite comdirectVisaImporter (options' blz) file
        runImport AQBankingImport =
          importReadWrite aqbankingImporter (options' ()) file
        options' env = options{oEnv = (env, version)}

run (Update version doMatch doRequest) options = do
  res <- runAQ options $ aqbankingListtrans doRequest
  void $ runRWST
    (mapM (importWrite $ iImport aqbankingImporter) res)
    options{oEnv = ((), version)} ()
  when doMatch $ run Match options

run (Commit args) options = flip runReaderT options $ do
  un <- readUser $  show . name
  dir <- takeDirectory <&> absolute =<< readLedger mainLedger
  bal <- lift $ runAQ options $ readAqbanking ["listbal"]
  sheet <- lift $ runLedger readProcess' ["balance", "-e", "tomorrow"] options
  liftIO $ do setCurrentDirectory dir
              callProcess "git" $ "commit":args ++
                ["-m", intercalateL "\n" $ ["User " ++ un, ""]
                  ++ bal ++ ["Balance Sheet:", sheet]]

run ListBalances options = void $ runAQ options $ callAqbanking ["listbal"]
  
run Setup options = void $ runAQ options aqbankingSetup

run Match options =
  withSystemTempDirectory "dbacl" $ \tmpdir -> do
  withJournals [imported] options $ match options{oEnv = tmpdir}

run (AQBanking args) options = void $ runAQ options $ callAqbanking args

run (Ledger args) options = runLedger callProcess args options

run (HLedger args) options =
  runLedger' callProcess cHledgerExecutable
  (maybe mainLedger const =<< mainHledger)
  args options

runLedger run = runLedger' run cLedgerExecutable mainLedger

runLedger' run getExec getLedger args options = flip runReaderT options $ do
  exec <- readConfig getExec
  ledger <- absolute =<< readLedger getLedger
  liftIO $ do
    setEnv "LEDGER" ledger
    run exec args

-- | performs an action taking a journal as argument. this journal is
-- read from 'imported' and 'addedByThisUser' ledger files
-- withJournals ::
--   [Ledgers -> FilePath]
--   ->  FullOptions ()
--   -> (Journal -> ErrorT IO b) -> ErrorT IO b
withJournals
  :: (MonadError Msg m, MonadIO m) =>
     [Ledgers -> FilePath]
     -> Options User config env -> (Journal -> m b) -> m b
withJournals journals options f = do
  liftIO $ printf "(Reading journal from \n%s)\n...\n\n"
    $ intercalateL "\n" $ show <$> jfiles
  journal <- liftIO $
      -- to conquer problems with the `instance Monoid Journal`
     right mconcat' . sequence <$> mapM (readJournalFile Nothing Nothing False) jfiles
  either (throwError . T.pack) f journal
  where jfiles = runReader (mapM (absolute <=< readLedger)
                           journals) options
