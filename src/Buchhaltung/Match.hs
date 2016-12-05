{-# LANGUAGE RankNTypes #-}
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
                (S.Set AccountName, Zipper Update) (ErrorT m)
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
                      (S.fromList $ fst <$> done, differentiate todos) :: ErrorT IO ()

-- | Apply the first matching 'Todo'
updateAccountName :: Update -> Maybe (Transaction, Transaction)
updateAccountName WithSource{ wInfo = Nothing } = Nothing
updateAccountName up =
  Just (wTx up, tPosts . ix (wIdx up) . pAcc .~ (fromJust $ wInfo up) $ wTx up)

printSource :: Source -> String
printSource = P.render . table [25,35] ["Field", "Value"] .
  (\(x,z) -> [x,z]) . unzip . M.toList . sourceToMap

mainLoop :: String -> MatchT IO ()
mainLoop i = do
  zip <- gets $ snd
  let tx = present zip
  liftIO $ do
    putStrLn $ printSource $ wSource $ tx
    printf "Current Transaction: %d, Remaining: %d\n"
      (length $ past zip )
      $ length $ future zip
    putStr i
  account <- myAskAccount =<< suggestAccount tx -- 
  let
    g "save" = void $ saveChanges $ changeTransaction
               $ mapMaybe updateAccountName $ integrate zip
    g "<" = prev zip
    g ">" = next zip
    g _   = do
      modify $ first $ S.insert account
      learn [(account, return tx)]
      modify $ second $ fwd . modifyPresent (fmap $ const $ Just account)
      next zip
  g account
  where
    next (LZ _ []) = mainLoop "<< DONE! Use 'save' to exit >>\n\n"
    next z = modify (second $ const fwd z) >> mainLoop ""
    prev (LZ (_ :| []) _) =
      mainLoop "<< This is the first transaction >>\n\n"
    prev z = modify (second $ const back z) >> mainLoop ""

histfsuf :: String
histfsuf =   "learn"

data Default = Default  { display :: T.Text, defAcc :: AccountName }
  
suggestAccount :: Update -> MatchT IO (Maybe Default)
suggestAccount tx = do
  accs <- getAccountList
  args <- dbaclProcC <$> mapM tmp accs
  text <- bayesLine tx
  bin <- readConfig cDbaclExecutable
  let
    g [] = return Nothing
    g accounts = do
      (code, output, _) <- liftIO $ readProcessWithExitCode bin args text
      case code of
        ExitSuccess -> return Nothing
        ExitFailure x ->
          return $ Just $ Default info sa
          where sa = accounts !! (x-1)
                info :: T.Text
                info = either fshow (text . lookup sa)
                  (dbacl_parse accs output)
                text Nothing = "failed\t\t"
                text (Just te) = "uncertainty: " <> T.pack te <> "\t"
  maybe (g accs) (return . Just . Default "manual:\t\t\t") $ wInfo tx
  
bayesLine :: Monad m => WithSource a -> MatchT m String
bayesLine w = T.unpack . T.unwords <$> askBayesFields (wSource w)

learn :: [(AccountName, NonEmpty (WithSource a))]
      -> MatchT IO ()
learn pairs = liftIO . runConcurrently . mconcat =<< mapM learn' pairs
  where learn' (name,txs) = do 
          bin <- readConfig cDbaclExecutable
          text <- L.unlines <$> mapM bayesLine (N.toList txs)
          file <- tmp name
          return $ Concurrently $ do
            -- let text = if text'=="" then "\n" else text' PROBLEM-bayes_fields
            L.putStrLn $ "Learning: " <> name
            -- putStrLn $ "\n\n"++ (intercalate "\n\n" $ info <$> todos)
            -- putStrLn text
            out <- readProcess bin (dbaclProc file) text
            appendFile (file <> "_raw" ) $ text <> "\n\n" <> out
            L.putStrLn $ "Done:     " <> name

accountCompletion :: [String] -> CompletionFunc IO
accountCompletion cc = completeWord Nothing
                        "" -- don't break words on whitespace, since account names
                           -- can contain spaces.
                        $ \s -> return $ map (\x -> Completion x x  False)
                                        $ filter (s `isInfixOf`) cc

type Update = WithSource (Maybe AccountName)

-- | Group all transactions with source into those that already have
-- an account and those that starting with 'cTodoAccount'
groupByAccount
  :: MonadReader (Options user Config env) m =>
     Journal
     -> m (Maybe ( [(AccountName, NonEmpty (WithSource ()))]
                 , NonEmpty (WithSource (Maybe a))))
groupByAccount j = do
  tag <- askTag
  todoFilt <- askTodoFilter
  let acc = paccount . wPosting
      f s = if todoFilt ac then Right $ fmap (const Nothing) <$> s
            else Left (ac, s)
        where ac = acc $ N.head s
  return $ traverse (fmap S.sconcat . nonEmpty) $ partitionEithers $ fmap f
    $ N.groupBy ((==) `on` acc)
    $ sortBy (comparing acc) $ rights $ extractSource tag
    <$> jtxns j 

myAskAccount :: Maybe Default -> MatchT IO AccountName
myAskAccount acc = getAccountList >>= \accs -> 
  liftIO $ askAccount accs (defAcc <$> acc) (Just histfsuf) prompt
  where prompt = Right $ maybe "" showdef acc <> "\n[<, >, save, RET]:\t"
        showdef (Default d a) = d <> (revAccount2 a) :: T.Text

getAccountList :: Monad m => MatchT m [AccountName]
getAccountList = gets $ S.toList . fst

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

  
