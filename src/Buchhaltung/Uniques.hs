{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Buchhaltung.Uniques 
where

import           Buchhaltung.Common
import           Control.Applicative
import           Control.Arrow hiding (loop)
import           Control.Monad.RWS.Strict
import           Data.Either
import           Data.Function
import qualified Data.HashMap.Strict as HM
import           Data.List
import qualified Data.ListLike as L
import qualified Data.ListLike.String as L
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import qualified Data.Set as S
import           Data.String
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import           Data.Time.Calendar
import qualified Data.Traversable as T
import           Formatting as F
import qualified Formatting.ShortFormatters as F
import           Hledger.Data
import           Hledger.Read
import           System.Environment (getArgs)
import           System.IO
import           Text.EditDistance
import qualified Text.PrettyPrint.Boxes as P
import           Text.Printf
  
addNew :: (MonadIO m, MonadReader (Options user Config env) m)
       => [FilledEntry]
          -- ^ new candidates including entries already existing in
          -- journal possible duplicates
       -> Journal 
       -> m [Entry]
addNew newTxs journal = do
  tag <- askTag
  let g i tx = (key tx i
               , ImportedEntry tx () $ wSource <$> extractSource tag tx)
  fmap (M.elems . fst)
    <$> execRWST (zipWithM (loop $ length newTxs) [1..] newTxs)
    () $ M.fromList
    $ zipWith g [1..] (jtxns journal)


-- | the key that is used to compare transactions. The Int is a
-- counter, to ensure there are no actual duplicates in the
-- map.
--
-- Instead duplicates are found by extraction of a key range using
-- 'M.split'. See 'findDuplicates'.
type Key = ([(CommoditySymbol,Quantity)], AccountName, Day, Int)
           
key :: Transaction -> Int -> Key          
key tx i = (compAmount $ pamount p, paccount p, tdate tx, i)
  where compAmount (Mixed am) = sort
          $ fmap (\x -> (acommodity x, aquantity x)) am
        p = head $ tpostings tx
  
type M m a = RWST () () (M.Map Key Entry) m a
                                 
loop :: (MonadIO m, MonadReader (Options user Config env) m)
     => Int -> Int -> FilledEntry -> M m ()
loop totalTx iTx new = do
  new' <- gets $ \old -> (key (ieT new) $ M.size old + 1, new)
  dups <- findDuplicates new'
  let msg iDup = printf
                 "(Showing %d. of %d duplicates of the %d. of %d transactions)\n"
                 iDup (length dups) iTx totalTx
  checkOrAsk new' msg
    $ sortBy (flip $comparing snd)
    $ (id &&& g) <$> dups
    where g (k,y) = negate . on
            (restrictedDamerauLevenshteinDistance defaultEditCosts)
            (TL.unpack . json)  (ieSource new)
            <$> (eitherToMaybe $ ieSource y)

eitherToMaybe :: Either b a -> Maybe a
eitherToMaybe = either (const Nothing) Just  

findDuplicates :: Monad m => (Key, FilledEntry) -> M m [(Key,Entry)]
findDuplicates ((ams,acc,day,ix),new) = gets $ \old ->
   let later = snd $ M.split (ams,acc,day,0) old in
   M.toList $ fst $ M.split (ams,acc,addDays 1 day,ix) later

-- | check single new entry against a list of conflict
-- candidates, and insert new entry (if list is empty), or keep old
-- entry (if identical to new one), or ask weither to modify old entry
-- or insert new entry.
checkOrAsk :: (MonadIO m, MonadReader (Options user Config env) m)
           => (Key, FilledEntry)
           -> (Int -> String)
           -> [((Key,Entry), Maybe Int)] -> M m ()
checkOrAsk new msg [] = do
  modify $ uncurry M.insert $ second fromFilled new
  liftIO $ T.putStrLn "\nSaved new transaction.\n"
checkOrAsk new msg (( (oldKey,oldEntry), cost):remaining) = do
  if cost == Just 0 then return () -- do nothing, i.e. use old unchanged
    else if False && cost > Just ( - 98)
            && on (==) (tdate.ieT) oldEntry (fromFilled $ snd new) then do
      -- liftIO $ do print (fst new) >> print (oldKey)
      --             print (tdate.ieT.snd $ new) >> print (tdate.ieT $ oldEntry)
      --             print (ieSource.snd $ new) >> print (ieSource $ oldEntry)
    overwriteOldSource
    else do
    let question = (answer =<<) . liftIO $ do
          L.putStr $ L.unlines
            [ prettyPrint cost (snd new) oldEntry $ length remaining 
            , T.pack $ msg 1
            , "Yes, they are duplicates. Update the source [y]"
            , "No, " <> (if null remaining then "Save as new transaction"
                         else "Show next duplicate") <> " [n]"
            , "Skip this new transaction [q]"
            , "Your answer:" ]
          getLine
        answer "y" = overwriteOldSource
        answer "n" = checkOrAsk new (msg . succ) remaining
        answer "q" = return ()
        answer _   = question
    question
  where 
    overwriteOldSource = do
          tag <- lift $ askTag
          modify $ M.adjust (applyChanges tag new oldKey) oldKey
          liftIO $ T.putStrLn "Updated duplicate's source.\n"
  
prettyPrint :: Maybe Int -> FilledEntry -> Entry -> Int -> T.Text
prettyPrint cost new old remain =
      let union2 = f . second unzip . unzip
                          . M.toList . union
          union old = M.mergeWithKey g
            (fmap $ flip (,) "<empty>") (fmap $ (,) "<empty>")
             old $ sourceToMap $ ieSource new
          g k x y | x == y = Just $ (x, "")
                  | True   = Just $ (x, y)
          f (k,(old,new)) = T.pack $ P.render
            $ table [20, 25, 25] header [k, old, new]
          header = ["Field", "Old", "New"]
          oldSource = ieSource old
          showError (Left x) = [""
                               ,"Error retrieving old transaction's source:"
                               , T.pack x]
          showError (Right _) = []
          def _ Nothing = "n.a."
          def f (Just x) = f x
        in
        L.unlines $
        [ union2 . either (const mempty) sourceToMap $ oldSource ]
        ++ [ sformat
             ("changes: "%F.s%"/"%F.s%", remaining number of old canditates "%F.d)
             (def (show . negate) cost)
             (def (show . TL.length . json) $ eitherToMaybe $ ieSource old)
             remain
           ]
        ++ showError oldSource


-- | todo undefined check do not upate ieSource?
applyChanges :: ImportTag -> (Key, FilledEntry)
             -> Key -> Entry -> Entry
applyChanges tag ((ams2,acc2,day2,_),newEntry)
  (ams1,acc1,day1,_) oldEntry =
  if (ams2,acc2) /= (ams1,acc1) then
    error $ unlines ["Change not supported: "
                    , show newEntry
                    , show oldEntry]
 else oldEntry{ieT= (injectSource tag (ieSource newEntry)
                      $ ieT oldEntry){tdate = day2}}
