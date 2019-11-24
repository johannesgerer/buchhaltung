{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Buchhaltung.AQBanking where

import           Buchhaltung.Common
import           Control.Monad.RWS.Strict
import           Data.Maybe
import           Text.Regex.TDFA
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
            =<< askExec aqhbciToolExecutable "aqhbci-tool4" "-D"

readAqhbci :: AAQM String
readAqhbci args = runProc readProcess' args
            =<< askExec aqhbciToolExecutable "aqhbci-tool4" "-D"

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

  T.pack <$> readAqbanking ["export"
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
    ,"AQBanking manual. Use the '-D' to point to the configured "
    ,"'configDir'."]
  liftIO $ createDirectoryIfMissing True path
  callAqhbci [ "adduser", "-t", "pintan", "--context=1"
            , "-b", aqBlz conn
            , "-u", aqUser conn
            , "-s", aqUrl conn
            , "-N", aqName conn
            , "--hbciversion=" <> toArg (aqHbciv conn)]
  id <- getUniqueID
  callAqhbci [ "getbankinfo", "-u" , id]
  callAqhbci [ "getsysid", "-u" , id]
  chooseITANMode id
  callAqhbci [ "getaccounts", "-u", id ]
  localUniqueIDs <- listAccounts
  sequence_ $ callAqhbci . ([ "getaccsepa", "-a" ] ++) . (:[]) <$> localUniqueIDs
  callAqhbci [ "listaccounts" ]
  lift . lift . putStrLn $
    "NOTE: If this didn't work, you might need to set up aqbanking manually "
    ++ "instead. See e.g. "
    ++ "https://www.aquamaniac.de/rdm/projects/aqbanking/wiki/SetupPinTan"

chooseITANMode :: String -> AQM ()
chooseITANMode id = do
  callAqhbci [ "getitanmodes", "-u", id]
  ss <- lines <$> readAqhbci [ "listitanmodes", "-u", id]
  let available = filter (=~ (".* \\[available" :: String)) ss
  let (_, _, _, [n]) =
        head available =~ ("^- ([[:digit:]]+)" :: String) :: (String, String, String, [String])
  callAqhbci [ "setitanmode", "-u", id, "-m", n]

listAccounts :: AQM [String]
listAccounts = do
  ss <- lines <$> readAqhbci [ "listaccounts", "-v" ]
  return $ localUniqueID <$> ss
  where
    localUniqueID s =
      let (_, _, _, [s']) =
            (s =~ (".* LocalUniqueId: (.*)" :: String) :: (String, String, String, [String]))
      in s'

getUniqueID :: AQM String
getUniqueID = do
  s <- readAqhbci [ "listusers" ]
  let (_, _, _, [s']) =
        (s =~ (".* Unique Id: (.*)\n" :: String) :: (String, String, String, [String]))
    in return s'

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
withContext "export"     = return True
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
