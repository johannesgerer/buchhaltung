{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Buchhaltung.AQBanking where

import           Buchhaltung.Common
import           Control.Arrow
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.RWS.Strict
import           Control.Monad.Reader
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Formatting (sformat, (%))
import qualified Formatting.ShortFormatters as F
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           System.IO.Temp
import           System.Process as P
import           Text.Printf

type AQM = CommonM (AQConnection, AQBankingConf)

askExec
  :: (AQBankingConf -> Maybe FilePath)
  -> FilePath -- ^ default
  -> String -- ^ config path argument
  -> AQM ([FilePath], FilePath)
  -- ^ Path and Args
askExec get def arg = do
  path <- askConfigPath
  readConf $ return . ((,) [arg, path]) . fromMaybe def . get

runProc ::
  (FilePath -> [String] -> IO a)
  -> [String] -> ([FilePath], FilePath)
  -> AQM a
runProc run args (argsC, bin) = liftIO $ run bin $ argsC ++ args

runAqhbci :: [String] -> AQM ()
runAqhbci args = runProc callProcess args
            =<< askExec aqhbciToolExecutable "aqhbci-tool4" "-C"

runAqbanking prc args = runProc prc args
               =<< askExec aqBankingExecutable "aqbanking-cli" "-D"

readPr x y = readProcess x y ""

aqbankingListtrans :: Bool
                      -- ^ request new transactions
                   -> AQM T.Text
aqbankingListtrans doRequest = do
  path <- askContextPath
  conn <- readConn return
  when doRequest $
    runAqbanking callProcess ["request"
                             , "-c", path
                             , "--transactions"
                             , "--ignoreUnsupported"
                             ]

  T.pack <$> runAqbanking readPr ["listtrans"
                                 , "-c", path
                                 ]

-- | Runs an AQBanking Action for all connections of the selected user
runAQ :: FullOptions () -> AQM a -> ErrorT IO [a]
runAQ options action = fst <$> evalRWST action2 options ()
  where action2 = do
          user <- user
          aqconf <- maybeThrow ("AQBanking not configured for user "%F.sh)
                    ($ user) return $ aqBanking user
          forM (connections aqconf) $ \conn ->
            withRWST (\r s -> (r{oEnv = (conn,aqconf)}, s)) action


aqbankingSetup :: AQM ()
aqbankingSetup = do
  path <- askConfigPath
  conn <- readConn return
  exists <- liftIO $ doesPathExist path
  when exists $ throwFormat
    ("Path '"%F.s%"' already exists. Cannot install into existing path.")
    ($ path)
  typ <- readConn $ return . aqType
  when (typ /= PinTan) $ throwError $ mconcat
    ["modes other than PinTan have to be setup manually. Refer to the "
    ,"AQBanking manual. Use the '-C' to point to the configured "
    ,"'configDir'."]
  liftIO $ createDirectoryIfMissing True path
  runAqhbci [ "adduser", "-t", "pintan", "--context=1"
            , "-b", aqBlz conn
            , "-u", aqUser conn
            , "-s", aqUrl conn
            , "-N", aqName conn
            , "--hbciversion=" <> toArg (aqHbciv conn)]
  runAqhbci [ "getsysid" ]
  runAqhbci [ "getaccounts" ]
  runAqhbci [ "listaccounts" ]


askContextPath = (<.> "context") <$> askConfigPath

askConfigPath :: AQM FilePath
askConfigPath = do
  conn <- readConn return
  makeValid . (</> aqBlz conn <> "-" <> aqUser conn)
          <$> readConf (absolute . configDir)

readConn :: (AQConnection -> AQM a) -> AQM a
readConn f = f =<< reader (fst . oEnv)


readConf :: (AQBankingConf -> AQM a) -> AQM a
readConf f = f =<< reader (snd . oEnv)
