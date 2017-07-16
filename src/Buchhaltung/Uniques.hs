{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Buchhaltung.Uniques
where

import           Buchhaltung.Common
import           Control.Arrow hiding (loop)
import           Control.Monad.RWS.Strict
import           Control.Monad.Trans.Cont
import           Data.Function
import           Data.List
import qualified Data.ListLike as L
import qualified Data.ListLike.String as L
import qualified Data.Map as M
import           Data.Ord
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import           Data.Time.Calendar
import           Formatting as F
import qualified Formatting.ShortFormatters as F
import           Hledger.Data
import           System.IO
import           Text.EditDistance
import qualified Text.PrettyPrint.Boxes as P
import           Text.Printf

-- | The monad stack
type M r m = ContT r (RWST () () (M.Map KeyIx (Aged Entry)) m)
  
data Aged a = Aged { getAge :: Age
                   , unAged :: a
                   }
  deriving Show

data Age = Old | New
  deriving (Show, Eq)

-- | the key that is used to compare transactions. The Int is a
-- counter, to ensure there are no actual duplicates in the
-- map and to be able to update entries.
--
-- Duplicates are then found by extraction of a key range using
-- 'M.split'. See 'findDuplicates'.
type Key = ([(CommoditySymbol,Quantity)], AccountName, Day)
type KeyIx = (Key, Int) 


-- | Takes a list of new entries, removes duplicates or updates
-- existing transactions and adds new entries.
addNewEntriesToJournal :: (MonadIO m, MonadReader (Options user Config env) m)
       => [FilledEntry]
          -- ^ new candidates including entries already existing in
          -- journal possible duplicates
       -> Journal
       -> m [Entry]
addNewEntriesToJournal newTxs journal = do
  tag <- askTag
  let toKeyVal i tx = ((deriveKey tx, i)
                      , Aged Old $ ImportedEntry tx () $
                        wSource <$> extractSource tag tx)
  fmap (fmap unAged . M.elems . fst)
    <$> execRWST (evalContT $ callCC $ \exit -> zipWithM_
                   (loop exit $ length newTxs) [1..] newTxs)
    () $ M.fromList
    $ zipWith toKeyVal [1..] (jtxns journal)

 
-- | Derive a key from a transaction and an index
deriveKey :: Transaction -> Key
deriveKey tx = (compAmount $ pamount p, paccount p, tdate tx)
  where compAmount (Mixed am) = sort
          $ fmap (acommodity &&& aquantity) am
        p = head $ tpostings tx

-- | loop through all existing possible duplicates of a new
-- transaction
loop :: (MonadIO m, MonadReader (Options user Config env) m)
     => (() -> M r m ())
     -> Int -> Int -> FilledEntry -> M r m ()
loop exit totalTx iTx new = do
  let msg = format ("Transaction: "%F.d%" of "%F.d%" new\n") iTx totalTx
      key = deriveKey $ ieT new
  dups <- findDuplicates key
  checkOrAsk exit (key, new) msg
    $ sortBy (flip $ comparing snd) $ (id &&& distance) <$> dups
    where distance (_, y) =
            -- careful: the negate is fmapped over the Maybe value
            -- which changes the relative order between Nothing and
            -- Justs.
            negate . on
            (restrictedDamerauLevenshteinDistance defaultEditCosts)
            (TL.unpack . json)  (ieSource new)
            <$> (eitherToMaybe $ ieSource $ unAged y)

eitherToMaybe :: Either b a -> Maybe a
eitherToMaybe = either (const Nothing) Just

-- | Find all duplicates for a given key
findDuplicates :: Monad m => Key -> M r m [(KeyIx,Aged Entry)]
findDuplicates key@(ams,acc,day) = lift $ gets $ \old ->
   let later = snd $ M.split (key,0) old in
   M.toList $ fst $ M.split ((ams,acc,addDays 1 day),0) later

-- | check single new entry against a list of conflict
-- candidates, and insert new entry (if list is empty), or keep old
-- entry (if identical to new one), or ask weither to modify old entry
-- or insert new entry.
checkOrAsk :: (MonadIO m, MonadReader (Options user Config env) m)
           => (() -> M r m ())
           -> (Key, FilledEntry)
           -> TL.Text -- ^ message
           -> [((KeyIx, Aged Entry), Maybe Int)] -> M r m ()
checkOrAsk _ (newKey, new) _ []  = do
  newIx <- gets $ \old -> M.size old + 1
  modify $ uncurry M.insert $ ((newKey, newIx), Aged New $ fromFilled new)
  liftIO $ T.putStrLn "\nSaved new transaction.\n"
checkOrAsk exit new msg (( (oldKey, oldEntry), cost):remaining) = do
  liftIO $ print $ cost
  if getAge oldEntry == Old && cost == Just 0
    then return () -- do nothing, i.e. use old unchanged
    else if False && cost > Just ( - 98)
            && on (==) (tdate.ieT) (unAged oldEntry) (fromFilled $ snd new)
         then do
      -- liftIO $ do print (fst new) >> print (oldKey)
      --             print (tdate.ieT.snd $ new) >> print (tdate.ieT $ oldEntry)
      --             print (ieSource.snd $ new) >> print (ieSource $ oldEntry)
    overwriteOldSource
    else do
    let question = (answer =<<) . liftIO $ do
          L.putStr $ L.unlines
            [ prettyPrint cost (snd new) oldEntry msg $ length remaining
            , "Yes, they are duplicates. Update the source [y]"
            , "No, " <> (if null remaining then "Save as new transaction"
                         else "Show next duplicate") <> " [n]"
            , "Skip this new transaction [q]"
            , "Skip this and all remaining transactions [Q]"
            , "Your answer:" ]
          hSetBuffering stdin NoBuffering
          getChar <* putStrLn ""
        answer 'y' = overwriteOldSource
        answer 'n' = checkOrAsk exit new msg remaining
        answer 'q' = return ()
        answer 'Q' = exit ()
        answer _   = question
    question
  where
    overwriteOldSource = lift $ do
          tag <- lift $ askTag
          modify $ M.adjust (applyChanges tag new $ fst oldKey) oldKey
          liftIO $ T.putStrLn "\nUpdated duplicate's source.\n"

prettyPrint :: Maybe Int -> FilledEntry -> Aged Entry -> TL.Text -- ^ Message
            -> Int -- ^ Remaining
            -> T.Text
prettyPrint cost new (Aged age old) msg remain =
      let union2 = f . second unzip . unzip . M.toList $ M.mergeWithKey g
            (fmap $ flip (,) "<empty>") (fmap $ (,) "<empty>")
            (either (const mempty) sourceToMap $ oldSource)
            $ sourceToMap $ ieSource new
          g _ x y | x == y = Just $ (x, "")
                  | True   = Just $ (x, y)
          f (k,(old,new)) = T.pack $ P.render
            $ table [20, 25, 25] header [k, old, new]
          oldName Old = "Old"
          oldName New = "Imported earlier"
          header = ["Field", oldName age, "New"]
          oldSource = ieSource old
          showError (Left x) = [""
                               ,"Error retrieving old transaction's source:"
                               , T.pack x]
          showError (Right _) = []
          def _ Nothing = "n.a."
          def f (Just x) = f x
        in
        L.unlines $
        [ union2 ]
        ++ [ sformat
             ("changes: "%F.s%"/"%F.s%"\n"%F.t%"Remaining existing duplicates: "%F.d)
             (def (show . negate) cost)
             (def (show . TL.length . json) $ eitherToMaybe $ ieSource old)
             msg
             remain
           ]
        ++ showError oldSource


applyChanges :: ImportTag -> (Key, FilledEntry)
             -> Key -> Aged Entry -> Aged Entry
applyChanges tag ((ams2,acc2,day2), newEntry)
  (ams1, acc1, _) oldEntry =
  if (ams2,acc2) /= (ams1,acc1) then
    error $ unlines ["Change not supported: "
                    , show newEntry
                    , show oldEntry]
 else Aged New $ (unAged oldEntry){
    ieT= (injectSource tag (ieSource newEntry)
           $ ieT $ unAged oldEntry){tdate = day2} }
