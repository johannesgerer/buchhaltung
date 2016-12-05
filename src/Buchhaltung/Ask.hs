{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Buchhaltung.Ask where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.Trans.Class
import           Data.Char
import           Data.Function
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Hledger.Data.Types
import           System.Console.Haskeline
import           System.Console.Haskeline.History


-- ############## Readline editing loops ############
               
editLoop :: MonadException m =>
             (T.Text -> Either String c) -- ^ input to value extractor
            -> String -- ^ history File suffix
            -> Maybe (c, T.Text) -- ^ Default value and its string
            -> Maybe [T.Text] -- ^ completion list
            -> Either T.Text T.Text -- ^ use promtp: Left promp ++ [Def]:, or Right prompt
            -> Maybe T.Text -- ^ intitial readline text
            -> m c
editLoop extractor = editLoopM $ return . extractor 
  
editLoopM :: MonadException m =>
             (T.Text -> m (Either String c)) -- ^ input to value extractor
            -> String -- ^ history File suffix
            -> Maybe (c, T.Text) -- ^ Default value and its string
            -> Maybe [T.Text] -- ^ completion list
            -> Either T.Text T.Text -- ^ use promtp: Left promp ++ [Def]:, or Right prompt
            -> Maybe T.Text -- ^ intitial readline text
            -> m c
editLoopM extract histFileSuf def completionList prompt init =
  runInputT2 settings loop
  where loop =  do s <- fromJust <$> getInput
                   let useDef = if s=="" then Just fst else Nothing
                       tryExtract = do
                         modifyHistory $ addHistoryRemovingAllDupes s
                         either ((>> loop) . outputStr . (++"\n")) return =<<
                           (lift $ extract $ T.pack s)
                   maybe tryExtract return $ useDef <*> def
        getInput = getInputLineWithInitial (T.unpack prompt')
                   (maybe "" T.unpack init, "")
        prompt' = either (<>(maybe ": " (\x -> " ["<> snd x <>"]: ") def)) id prompt :: T.Text
        settings = Settings {  historyFile = histFile  histFileSuf
                            ,complete = completeFunc, autoAddHistory = False }
        completeFunc = maybe noCompletion (customCompl . fmap T.unpack)
                       completionList
        customCompl list = completeWord Nothing "" -- don't break words on whitespace, since account names
                                                    -- can contain spaces.
                           $ \s -> return $ map (g s) $ filter (on f (toLower<$>) s) list
          where g s x = Completion y x False
                  where y = concat $ "" : tail (ciSplitOn s x)
                f s = (||) <$> isPrefixOf s <*> isInfixOf (":"++s)
histFile suf = Just $  ".haskeline_history_"++suf

-- case insensitive char
newtype CiChar = CiChar { ciChar :: Char }
instance Eq CiChar where
  (==) = (==) `on` (toLower . ciChar)

-- ciSplitOn s x = (ciChar<$>) <$> (on splitOn (CiChar<$>) s x)
ciSplitOn s x = (ciChar<$>) <$> (on (split . onSublist) (CiChar<$>) s x)

myGetchar :: IO Char
myGetchar = fromJust <$> runInputT2 defaultSettings ( getInputChar "your action: ")

runInputT2 s i = runInputT s $ withInterrupt $ handle
                (\Interrupt -> outputStrLn "you will loose all unsaved data!" >> i) i

  
-- ziplist action        
editHaskeline :: (a -> String) -> (a -> String -> a) -> a -> IO a
editHaskeline show modify v = liftM (modify v . fromJust) $
                              runInputT2 defaultSettings{historyFile = Just ".haskeline_history"} $
                              getInputLineWithInitial "edit: " (show v,"")

askAccount :: [AccountName] -- ^ completion list
             -> Maybe AccountName -- ^ default value, if "" is entered
             -> Maybe String -- ^ history file suffix
             -> Either T.Text T.Text -- ^ prompt
             -> IO AccountName
askAccount completionList def suf pr =
  revAccount2 <$> editLoop (maybe notNull (const Right) def)
  (fromMaybe "Account" suf)
  ((id &&& id) . revAccount2 <$> def)
  (Just $ revAccount2 <$> completionList) pr Nothing --def
  where notNull s = if s=="" then Left "Blank Account not allowed"
                    else Right s
                                       
revAccount2 :: T.Text -> T.Text
revAccount2 = T.intercalate ":" . reverse . T.splitOn ":"
