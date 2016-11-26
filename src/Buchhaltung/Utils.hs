{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
module Buchhaltung.Utils where

import           Control.Arrow
import qualified Control.Exception.Lifted as E
import           Control.Monad
import qualified Control.Monad.Except as E
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Data.Function
import qualified Data.ListLike as L
import           Data.Monoid
import           Data.String
import qualified Data.Text as T
import           Formatting (sformat, (%))
import qualified Formatting.ShortFormatters as F
import           System.Directory
import           System.IO
import qualified System.IO.Temp as T
 
-- | apply a funtion to the ith element of a list
modifyNth :: (a -> a) -> Int -> [a] -> [a]
modifyNth f n = zipWith z [0..]
  where z i | i == n = f
            | otherwise = id

fshow :: (Show a, IsString b) => a -> b
fshow = fromString . show

intercalateL
  :: (L.ListLike a item, L.ListLike b a) =>
     a -> b -> a
intercalateL x = L.concat . L.intersperse x


map2 :: Arrow a => a b' c' -> a (b', b') (c', c')
map2 = join (***)

-- withTempFileM :: (MonadIO m, MonadBaseControl IO m)
--               => FilePath -> IOMode -> (Handle -> m r) -> m r
-- withTempFileM name mode = E.bracket (liftIO $ openFile name mode)
--                       (liftIO . hClose)

withFileM :: (MonadIO m, MonadBaseControl IO m)
          => FilePath -> IOMode -> (Handle -> m r) -> m r
withFileM name mode = E.bracket (liftIO $ openFile name mode)
                      (liftIO . hClose)


#if ! MIN_VERSION_base(4,9,0)
instance Monoid a => Monoid (IO a) where
  mempty = pure mempty
  mappend = liftM2 mappend
#endif

  
doesPathExist :: FilePath -> IO Bool
doesPathExist = fmap getAny . (a . doesFileExist <> a . doesDirectoryExist)
  where a = fmap Any 


-- * Ported from 'System.IO.Temp' to work with 'MonadBaseControl'
  
withSystemTempFile template action = liftIO getTemporaryDirectory >>= \tmpDir -> withTempFile tmpDir template action
  
withSystemTempDirectory template action = liftIO getTemporaryDirectory >>= \tmpDir -> withTempDirectory tmpDir template action

withTempDirectory targetDir template =
  E.bracket
    (liftIO (T.createTempDirectory targetDir template))
    (liftIO . ignoringIOErrors . removeDirectoryRecursive)

  
withTempFile tmpDir template action =
 E.bracket
   (liftIO (openTempFile tmpDir template))
   (\(name, handle) -> liftIO (hClose handle >> ignoringIOErrors (removeFile name)))
   (uncurry action)


ignoringIOErrors :: MonadBaseControl IO m => m () -> m ()
ignoringIOErrors ioe = ioe `E.catch` (\e -> const (return ()) (e :: IOError))
