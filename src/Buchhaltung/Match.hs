{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Buchhaltung.Match
where

import           Buchhaltung.Ask
import           Buchhaltung.Common
import           Buchhaltung.Importers
import           Buchhaltung.Zipper
import           Control.Arrow
import           Control.Concurrent.Async
import           Control.Lens
import           Control.Monad.RWS
import           Control.Monad.Reader
import           Data.Either
import           Data.Function
import           Data.List
import qualified Data.List.NonEmpty as N
import qualified Data.ListLike as L
import qualified Data.ListLike.String as L
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Ord
import qualified Data.Semigroup as S
import qualified Data.Set as S
import qualified Data.Text as T
import           Hledger.Data hiding (at)
import           System.Console.Haskeline
import           System.Exit
import           System.FilePath
import           System.Process
import qualified Text.PrettyPrint.Boxes as P
import           Text.Printf
  
type MatchT m = RWST (FullOptions FilePath) ()
                (M.Map AccountName Bool, Zipper Update) (ErrorT m)
  -- ^ R: temporaray dbacl path
  --
  --   W: Set of learned accounts
  -- 
  --   S: Zipper with all transaction that neede to be matched

match :: FullOptions FilePath -> Journal -> ErrorT IO ()
match options j = maybe (liftIO $ print "No unmatched transactions")
                  g
                  $ runReader (groupByAccount j) options
  where
    g (done, todos) = void $ runRWST (learn done >> mainLoop "")
                      options
                      (mempty, differentiate todos) :: ErrorT IO ()

-- | Apply the first matching 'Todo'
updateAccountName :: Update -> Maybe (Transaction, Transaction)
updateAccountName WithSource{ wInfo = Nothing } = Nothing
updateAccountName up =
  Just (wTx up, tPosts . ix (wIdx up) . pAcc .~ (fromJust $ wInfo up) $ wTx up)

printSource :: Source -> String
printSource = P.render . table [25,35] ["Field", "Value"] .
  (\(x,z) -> [x,z]) . unzip . M.toList . sourceToMap

mainLoop :: String -> MatchT IO ()
mainLoop msg = do
  zip <- gets $ snd
  let tx = present zip
  liftIO $ do
    putStrLn $ printSource $ wSource $ tx
    printf "Current Transaction: %d, Remaining: %d\n"
      (length $ past zip )
      $ length $ future zip
    putStr msg
  account <- myAskAccount =<< suggestAccount tx -- 
  let
    next = modify (second fwd) >> mainLoop (fwdMsg zip)
    prev = modify (second back) >> mainLoop (backMsg zip)
    fwdMsg (LZ _ []) =  "<< DONE! Use 'save' to exit >>\n\n"
    fwdMsg _ = ""
    backMsg (LZ (_ :| []) _) =  "<< This is the first transaction >>\n\n"
    backMsg _ = ""
    g "save" = void $ saveChanges Nothing $ changeTransaction
               $ mapMaybe updateAccountName $ integrate zip
    g "<" = prev
    g ">" = next
    g _   = do
      learn [(account, return tx)]
      modify $ second $ modifyPresent (fmap $ const $ Just account)
      next
  g account

histfsuf :: String
histfsuf =   "learn"

-- | Data type describing the suggested or 'default' account when
-- asking the user for account input
data Default = Default  { prefixed :: T.Text, defAcc :: AccountName }
  
suggestAccount :: Update -> MatchT IO (Maybe Default)
suggestAccount tx = do
  accs <- getAccountList $ id
  args <- dbaclProcC <$> mapM tmp accs
  text <- bayesLine tx
  bin <- readConfig cDbaclExecutable
  let
    g = if null accs || T.null text then return Nothing
        else do
      (code, output, _) <-
        liftIO $ readProcessWithExitCode bin args $ T.unpack text
      case code of
        ExitSuccess -> return Nothing
        ExitFailure x ->
          return $ Just $ Default info sa
          where sa = accs !! (x-1)
                info :: T.Text
                info = either fshow (text . lookup sa)
                  (dbacl_parse accs output)
                text Nothing = "failed\t\t"
                text (Just te) = "uncertainty: " <> T.pack te <> "\t"
  maybe g (return . Just . Default "manual:\t\t\t") $ wInfo tx
  
bayesLine :: Monad m => WithSource a -> MatchT m T.Text
bayesLine w = T.strip . T.unwords <$> getBayesFields (wSource w)

learn :: [(AccountName, NonEmpty (WithSource a))]
      -> MatchT IO ()
learn pairs = do
  accs <- liftIO . runConcurrently . sequenceA =<< mapM learn' pairs
  forM_ accs $ \(k,v) -> modify $ first $ M.insertWith const k v
  where learn' (name,txs) = do 
          bin <- readConfig cDbaclExecutable
          text <- (L.unlines . filter (not . T.null)) <$>
                    mapM bayesLine (N.toList txs)
          file <- tmp name
          let action = if T.null text then return (name, False)
                       else do
                -- let text = if text'=="" then "\n" else text' PROBLEM-bayes_fields
                L.putStrLn $ "Learning: " <> name
                -- putStrLn $ "\n\n"++ (intercalate "\n\n" $ info <$> todos)
                -- putStrLn text
                let texts = T.unpack text
                (code, out, err) <- readProcessWithExitCode bin (dbaclProc file) texts
                appendFile (file <> "_raw" ) $ texts <> "\n\n" <>
                  out <> "\n\nStd error:\n" <> err
                let success = code == ExitSuccess && null err 
                L.putStrLn $ if success then "Done:     " <> name
                             else "Failed:   " <> name <> "\nwith Code "<> fshow code <> "\n\nAnd error:\n" <> T.pack err
                return (name, success)
          return $ Concurrently $ action
accountCompletion :: [String] -> CompletionFunc IO
accountCompletion cc = completeWord Nothing
                        "" -- don't break words on whitespace, since account names
                           -- can contain spaces.
                        $ \s -> return $ map (\x -> Completion x x  False)
                                        $ filter (s `isInfixOf`) cc

type Update = WithSource (Maybe AccountName)

-- | Group all transactions with source into those that already have
--  an account (ignoring those in 'ignoredAccountsOnMatch') and those
--  that start with 'cTodoAccount'
--
-- returns `Nothing` if there are no todo transactions
groupByAccount
  :: MonadReader (Options User Config env) m =>
     Journal
     -> m (Maybe ( [(AccountName, NonEmpty (WithSource ()))]
                 , NonEmpty Update))
groupByAccount j = do
  ignored <- readUser ignoredAccountsOnMatch
  tag <- askTag
  todoFilt <- askTodoFilter
  let acc = paccount . wPosting
      f s = if todoFilt ac then Right $ fmap (const Nothing) <$> s
            else Left (ac, s)
        where ac = acc $ N.head s
  return
    -- combine all transactions with different todo accounts
    $ traverse (fmap S.sconcat . nonEmpty)
    $ first (filter $ not . isIgnored ignored . fst)
    $ partitionEithers $ fmap f
    $ N.groupBy ((==) `on` acc)
    $ sortBy (comparing acc) $ rights $ extractSource tag
    <$> jtxns j 

myAskAccount :: Maybe Default -> MatchT IO AccountName
myAskAccount acc = getAccountList (const True) >>= \accs -> do
  revAccount <- askReverseAccount
  let prompt = Right $ T.unlines
        [""
        ,maybe "" ((<> "\n\nHit 'Enter' to use the above account, or") . showdef) acc
        ,"enter one of the following: account name (in reverse notation), "<>
         "'<', '>' to navigate, or 'save'"]
      showdef (Default d a) = d <> (revAccount a) :: T.Text
  askAccount accs (defAcc <$> acc) (Just histfsuf) prompt

getAccountList :: Monad m => (Bool -> Bool) -> MatchT m [AccountName]
getAccountList f = gets $ M.keys . M.filter f . fst

tmp :: Monad m => T.Text -> MatchT m FilePath
tmp name = reader $ (</> T.unpack name) . oEnv 

-- * dbacl arguments

-- | learning
dbaclProc :: String -> [String]
dbaclProc x = [  "-g" , oneword
              , "-g" , twowords
              --,"-D" -- interessant "-D" zeigt welche features gefunden wurden  (use grep match)
              , "-d"
              -- ,"-w 1" -- use N-grams with N=2
              --  "-S" -- ignore line breaks
              , "-0" -- do not preload (this is done by -o)
              -- ,"-e", "alnum" -- alpha numeric
              , "-j" -- lowercase
              , "-l", x
              , "-o", x ++ "_online"
              ] -- category name
  where word = "(^|[^[:alpha:]])([[:alpha:]]{3,})"
        oneword = wrap $ word ++"||2"
        twowords = wrap $ word++"[^[:alpha:]]+"++word++"||24"
        wrap = id --x = "'"++x++"'"

-- | classification   
dbaclProcC :: [String] -> [String]
dbaclProcC cats = let cats' = concat $ sequence [["-c"],cats]
                 in ( cats' ++ [  -- search this file for  'debugging'
                               -- "-v" -- output name of best (dont know when useful)
                               "-n" -- neg. logaritm
                               -- , "-N" -- prob
                               -- ,"-X"
                               -- ,"-d" -- sehr hilfreich, see manual (aber nur mit weniger categorien
                               ] )

  
