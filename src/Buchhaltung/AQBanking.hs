{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Buchhaltung.AQBanking where

import           Buchhaltung.Common
import           Control.Monad.RWS.Strict
import           Data.Maybe
import qualified Data.Text as T
import           Formatting ((%))
import qualified Formatting.ShortFormatters as F
import           System.Directory
import           System.FilePath
import           System.Process as P

-- * The Monad Stack and its runner

type AQM = CommonM (AQConnection, AQBankingConf)

-- | Runs an AQBanking Action for all connections of the selected user
runAQ :: FullOptions () -> AQM a -> ErrorT IO [a]
runAQ options action = fst <$> evalRWST action2 options ()
  where action2 = do
          user <- user
          aqconf <- maybeThrow ("AQBanking not configured for user "%F.sh)
                    ($ user) return $ aqBanking user
          forM (connections aqconf) $ \conn ->
            withRWST (\r s -> (r{oEnv = (conn,aqconf)}, s)) action

-- * Direct access to the executables

runProc ::
  (FilePath -> [String] -> IO a)
  -> [String] -> ([FilePath], FilePath)
  -> AQM a
runProc run args (argsC, bin) = liftIO $ run bin $ argsC ++ args

callAqhbci :: AAQM ()
callAqhbci args = runProc callProcess args
            =<< askExec aqhbciToolExecutable "aqhbci-tool4" "-C"

runAqbanking'
  :: (FilePath -> [String] -> IO b) -> AAQM b
runAqbanking' prc args = do
  args' <- addContext args
  runProc prc args'
    =<< askExec aqBankingExecutable "aqbanking-cli" "-D"



callAqbanking :: AAQM ()
callAqbanking = runAqbanking' callProcess

readAqbanking :: AAQM String
readAqbanking = runAqbanking' $ readProcess'

type AAQM a = [String] -> AQM a

-- * Higher Level of Abstraction

aqbankingListtrans :: Bool
                      -- ^ request new transactions
                   -> AQM T.Text
aqbankingListtrans doRequest = do
  when doRequest $
    callAqbanking ["request"
                  , "--transactions"
                  , "--ignoreUnsupported"
                  ]

  T.pack <$> readAqbanking ["listtrans"
                           ]

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
  callAqhbci [ "adduser", "-t", "pintan", "--context=1"
            , "-b", aqBlz conn
            , "-u", aqUser conn
            , "-s", aqUrl conn
            , "-N", aqName conn
            , "--hbciversion=" <> toArg (aqHbciv conn)]
  callAqhbci [ "getsysid" ]
  callAqhbci [ "getaccounts" ]
  callAqhbci [ "listaccounts" ]

-- * Utils

addContext :: AAQM [FilePath]
addContext [] = return []
addContext args@(cmd:_) = do
  withC <- withContext cmd
  fmap (args ++) $
    if withC then (\x -> ["-c", x <.> "context"]) <$> askConfigPath
    else return []

withContext "listbal"    = return True
withContext "listtrans"  = return True
withContext "request"    = return True
withContext "listaccs"   = return False
withContext cmd          = throwFormat
  ("'withContext' not defined for command '"%F.s%"'.")
  ($ cmd)

askConfigPath :: AQM FilePath
askConfigPath = do
  conn <- readConn return
  makeValid . (</> aqBlz conn <> "-" <> aqUser conn)
          <$> readConf (absolute . configDir)

readConn :: (AQConnection -> AQM a) -> AQM a
readConn f = f =<< reader (fst . oEnv)

readConf :: (AQBankingConf -> AQM a) -> AQM a
readConf f = f =<< reader (snd . oEnv)

-- | Find out executable path and two args selecting the config file
askExec
  :: (AQBankingConf -> Maybe FilePath)
  -> FilePath -- ^ default
  -> String -- ^ config path argument
  -> AQM ([FilePath], FilePath)
  -- ^ Path and Args
askExec get def arg = do
  path <- askConfigPath
  readConf $ return . ((,) [arg, path]) . fromMaybe def . get
