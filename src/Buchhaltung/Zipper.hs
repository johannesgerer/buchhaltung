{-# OPTIONS_HADDOCK ignore-exports #-}
module Buchhaltung.Zipper
  (module Buchhaltung.Zipper
  , E.NonEmpty(..)
  , E.nonEmpty
  ) where

import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as E
import           Data.Monoid

-- | Nonemtpy zipper
data Zipper a = LZ { past    :: E.NonEmpty a
                    , future  :: [a]
                    }

present :: Zipper a -> a
present = E.head . past

instance Functor Zipper where
  fmap f (LZ ps fs) = LZ (E.map f ps) (map f fs)

-- | Re-constitute a list from a zipper context.
integrate' :: Zipper a -> E.NonEmpty a
integrate' (LZ p f) = E.fromList $ reverse (E.toList p) <> f

integrate :: Zipper a -> [a]
integrate = E.toList . integrate'

-- | Turn a list into a context with the focus on the first element.
differentiate :: E.NonEmpty a -> Zipper a
differentiate (x :| xs) = LZ (pure x) xs

-- | Move the focus to the previous element. Do nothing if the focus
-- | is already on the first element.
back :: Zipper a -> Zipper a
back z@(LZ (_ :| []) _) = z
back (LZ (pr :| (np:ps)) fs) = LZ (np :| ps) (pr:fs)

-- | Move the focus to the next element.  Do nothing if the focus is
-- | already on the last element.
fwd :: Zipper a -> Zipper a
fwd z@(LZ _ []) = z
fwd (LZ ps (f:fs)) = LZ (f E.<| ps) fs

-- | Apply the given function to the currently focused element to
-- | produce a new currently focused element.
modifyPresent :: (a -> a) -> Zipper a -> Zipper a
modifyPresent f z@LZ{past=(present :| past)} = z { past = (f present :| past) }

-- | Apply the given function to all elements preceding the focus.
modifyBack :: ([a] -> [a]) -> Zipper a -> Zipper a
modifyBack f z@LZ{ past=(pr :| ps) } = z { past = (pr :| f ps) }

-- | Apply the given function to all elements after the focus.
modifyFwd :: ([a] -> [a]) -> Zipper a -> Zipper a
modifyFwd f z = z { future = f (future z) }

-- | Delete the currently focused element.  If there are no future
-- elements move the focus to the next last element.
delete :: Zipper a -> Zipper a
delete (LZ (_ :| []) [] ) = error "empty zipper not allowed"
delete (LZ (_ :| (np:past)) [] ) = LZ (np :| past) []
delete (LZ (_ :| past) (np:f) ) = LZ (np :| past) f

-- | Insert a new element just before the current focus, then move the
-- | focus to the newly inserted element.
insback :: a -> Zipper a -> Zipper a
insback x (LZ (pr :| ps) fs) = LZ (x :| ps) (pr:fs)

-- | Insert a new element just after the current focus, then move the
-- | focus to the newly inserted element.
insfwd :: a -> Zipper a -> Zipper a
insfwd x (LZ ps fs) = LZ (x E.<| ps) fs
