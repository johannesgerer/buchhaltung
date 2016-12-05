{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Console.ZipEdit
-- Copyright   :  (c) 2008  Brent Yorgey
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  <byorgey@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A library for creating simple interactive list editors, using a
-- zipper to allow the user to navigate forward and back within the
-- list and edit the list elements.
-----------------------------------------------------------------------------


module Buchhaltung.ZipEdit2
  (
    -- * Example usage
    -- $sample

    -- * Interface

    Action(..)
  , stdActions
  , (??)
  , EditorConf(..)
  , edit

  , LCont(..)
  , editWCont
  , Zipper(..)
  , integrate
  , differentiate
  , fwd, back
  , LState(..)       
  ) where

import           Buchhaltung.Zipper
import           Control.Arrow
import           Control.Monad.RWS.Strict
import qualified Data.List.NonEmpty as E
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.Directory (removeFile)
import           System.IO
import           System.Process

{- $sample

Here is a simple example of using the ZipEdit library:

> module Main where
>
> import System.Console.ZipEdit
>
> myEd = EC { display = const ""
>           , ecPrompt  = \n -> maybe "" show n ++ "? "
>           , actions = [ ('+', Modify (+1) ?? "Increment the current number.")
>                       , ('i', InsFwd "Value to insert: " read ?? "Insert a number.")
>                       ]
>                       ++ stdActions
>           }
>
> main = do
>   mxs <- edit myEd [1..10]
>   case mxs of
>     Nothing -> putStrLn "Canceled."
>     Just xs -> putStrLn ("Final edited version: " ++ show xs)

A session with this program might look something like this:

> $ test
>
> 1? k
>
> 1? j
>
> 2? j
>
> 3? +
>
> 4? +
>
> 5? j
>
> 4? i
> Value to insert: 98
>
> 98? d
> Final edited version: [1,2,5,4,98,5,6,7,8,9,10]

For more sophisticated examples, see @planethaskell.hs@ and @gmane.hs@ in
<http://code.haskell.org/~byorgey/code/hwn/utils>.

-}

-- | A continuation which can compute more of the list, along with
--   (maybe) another continuation.
data LCont a = LC (IO ([a], Maybe (LCont a)))

-- | The state of the editor consists of a current context, as well as
--   an optional continuation which can compute more list elements.
data LState a b = LS { ctx  :: Zipper a
                     , cont :: Maybe (LCont a)
                     , userSt :: b
                   }

-- | Perform a ModifyAllM action by running the given IO action and
-- | using it to replace all elements.
doModifyAllM :: Monad m => (Zipper e -> m (Zipper e)) -> Editor e u m ()
doModifyAllM m = do
  s <- get
  lift (m $ ctx s) >>= modifyCtx . const
  
-- | Perform a ModifyAllM action by running the given IO action and
-- | using it to replace all elements.
doModifyStateM :: Monad m => (LState e u -> m (LState e u)) -> Editor e u m ()
doModifyStateM m = do
  get >>= (lift . m) >>= put
                       

-- | Actions that can be taken by an editor in response to
--   user input.
data Action m a u = Comp (Action m a u) (Action m a u) | 
  Fwd                 -- ^ move forward one item.
              | Back                -- ^ move back one item.
              | Delete              -- ^ delete the current item.
              | Modify (a -> a)     -- ^ modify the current item by applying
                                    --   the given function.
              | ModifyState (LState a u -> LState a u)
                                    -- ^ modify complete state
              | ModifyStateM (LState a u -> m (LState a u))
                                    -- ^ modify complete state with IO.
              | ModifyAllM (Zipper a -> m (Zipper a))
                                    -- ^ modify everything with IO.
              | ModifyAll (Zipper a -> Zipper a)
                                    -- ^ modify everything.
              | ModifyM (a -> m a)
                                    -- ^ modify the current item by
                                    --   applying the given function,
                                    --   which gives its result in the
                                    --   IO monad.
              | ModifyFwd ([a] -> [a])
                                    -- ^ modify items following
                                    --   the current item by applying
                                    --   the given function.
              | ModifyBack ([a] -> [a])
                                    -- ^ modify items before the
                                    --   current item by applying the
                                    --   given function.
              | ModifyWInp String (String -> a -> a)
                                    -- ^ Using the given string as a
                                    --   prompt, obtain a line of user
                                    --   input, and apply the given
                                    --   function to the user input to
                                    --   obtain a function for
                                    --   modifying the current item.
              | ModifyWEditor (a -> String) (String -> a -> a)
                                    -- ^ Run the first function on the
                                    --   current item to produce a
                                    --   string, and open an editor
                                    --   (using the $EDITOR
                                    --   environment variable) on that
                                    --   string.  After the user is
                                    --   done editing, pass the
                                    --   resulting string to the
                                    --   second function to obtain a
                                    --   function for modifying the
                                    --   current element.
              | InsFwd String (String -> a)
                                    -- ^ Using the given string as a
                                    --   prompt, obtain a line of user
                                    --   input, and apply the given
                                    --   function to the user input to
                                    --   obtain a new item, which
                                    --   should be inserted forward of
                                    --   the current item.  The
                                    --   inserted item becomes the new
                                    --   current item.
              | InsBack String (String -> a)
                                    -- ^ Similar to InsFwd, except
                                    --   that the new item is inserted
                                    --   before the old current item.
              | Output (a -> String)
                                    -- ^ output a string which is a
                                    --   function of the current item.
              | Cancel              -- ^ cancel the editing session.
              | Done (LState a u -> m (Maybe (LState a u)))
                                      -- ^ complete the editing
                                      -- session, but if te function evaluates
                                      -- to "Just" and the suer
                                      -- answers y. In this case
                                      -- return the functions result.
              | Seq [Action m a u]      -- ^ perform a sequence of actions.
              | Help String (Action m a u)
                                    -- ^ an action annotated with a
                                    --   help string.

instance Monoid (Action m a u) where
  mappend = Comp
  mempty = Seq []
              
-- | Annotate a command with a help string.
(??) :: Action m a u -> String -> Action m a u
(??) = flip Help

-- | Some standard actions which can be used in constructing editor
--   configurations. The actions are: j - Fwd, k - Back, x -
--   Delete, q - Cancel, d - Done.
stdActions :: Monad m => [(Char, Action m a u)]
stdActions = [ ('j', Fwd    ?? "Move forward one item.")
             , ('k', Back   ?? "Move backward one item.")
             , ('x', Delete ?? "Delete the current item.")
             , ('q', Cancel ?? "Cancel the current editing session.")
             , ('d', Done (return . Just) ?? "Complete the current editing session.")
             ]

-- | A configuration record determining the behavior of the editor.
data EditorConf m a u = EC { display     :: LState a u -> m T.Text
                           -- ^ How to display info about the current state.
                           , ecPrompt      :: Zipper a -> String
                             -- ^ How to display info about the all elements.
                           , actions     :: [(Char, Action m a u)]
                             -- ^ A list specifying the actions to take
                             -- in response to user inputs.
                           , getchar :: Maybe (IO Char)
                           -- ^ optional different getChar implementation
                           }

-- | Editor monad: a reader monad with the editor configuration, plus
-- | a state monad for storing the context, plus IO for interacting
-- | with the user.
newtype Editor e userState m a = E (RWST
                                    (EditorConf m e userState) ()
                                    (LState e userState) m a)
  deriving (Functor, Monad, Applicative
           , MonadWriter ()
           , MonadState  (LState e userState)
           , MonadReader (EditorConf m e userState)
           , MonadRWS (EditorConf m e userState) () (LState e userState)
           , MonadIO)

instance MonadTrans (Editor e userState) where
  lift = E . lift

-- | Convenient shorthand for liftIO.
io :: MonadIO m => IO a -> m a
io = liftIO

-- | Run an action in the Editor monad, given an editor configuration,
-- | a starting list, and an optional continuation.
runEditor :: MonadIO m => Editor e u m a -> EditorConf m e u -> E.NonEmpty e ->
             Maybe (LCont e) -> u -> (Zipper e -> Zipper e) -> m a
runEditor (E e) ec l c userState mod = do
  io $ do hSetBuffering stdin NoBuffering
          hSetBuffering stdout NoBuffering
  fst <$> evalRWST e ec (LS (mod $ differentiate l) c userState)

-- | Lift a pure function on a context into a state modification
-- | action in the Editor monad.
modifyCtx :: Monad m => (Zipper e -> Zipper e) -> Editor e u m () -- 
modifyCtx f = do
  LS a b u <- get
  put (LS (f a) b u)

-- | Run the given editor on the given list, returning @Nothing@ if
--   the user canceled the editing process, or @Just l@ if the editing
--   process completed successfully, where @l@ is the final state of
--   the list being edited.
edit :: MonadIO m => EditorConf m a u       -- ^ editor configuration
     -> u            -- ^ initial userState
     -> E.NonEmpty a                -- ^ the list to edit
     -> (Zipper a -> Zipper a) -- ^ startupModifier
     -> m (Maybe (u,[a]))
edit ec u l mod = runEditor process ec l Nothing u mod

-- | Like 'edit', but with an additional parameter for a continuation
-- | which can be run to compute additional list elements and
-- | (optionally) another continuation.
editWCont :: MonadIO m => EditorConf m a u
          -> E.NonEmpty a                -- ^ the list to edit
          -> u            -- ^ initial userState
          -> IO ([a], Maybe (LCont a))
          -> (Zipper a -> Zipper a) -- ^ startupModifier
          -> m (Maybe (u,[a]))
editWCont ec l u c mod = runEditor process ec l (Just (LC c)) u mod

-- | The main Editor action implementing a zipedit-created interface.
process :: MonadIO m => Editor a u m (Maybe (u,[a]))
process = do
  s <- get
  e <- ask
  let cur = ctx s
  display' <- lift $ display e s
  ch <- io $ do putStr "\n"
                T.putStr display' 
                putStr (ecPrompt e cur)
                maybe getChar id $ getchar e
  io $ putStr "\n"

  -- res: Nothing = cancel, Just True = continue, Just False = done
  res <- if ch == '?'
           then showHelp (actions e) >> continue
           else case lookup ch (actions e) of
                  Nothing  -> return (Just True)
                  Just act -> doAction act
  case res of
    Nothing    -> return Nothing
    Just True  -> process
    Just False -> Just . (userSt &&& (integrate . ctx)) <$> get

-- | Display any help annotations provided by the user.
showHelp :: MonadIO m => [(Char, Action m a u)] -> Editor a u m ()
showHelp cs = io $ mapM_ (putStrLn . showCmdHelp) (helpCmd:cs)
  where helpCmd = ('?', Fwd ?? "Show this help.")
        showCmdHelp (c, Help s _) = c : (" - " ++ s)
        showCmdHelp (c, _)        = c : " -"

-- | Perform an action, returning an indication of the status: Nothing
-- | indicates cancellation of the editing process; Just True
-- | indicates that processing should continue; Just False indicates
-- | that processing is complete.
doAction :: MonadIO m => Action m a u -> Editor a u m (Maybe Bool)
doAction Fwd                 = doFwd >> continue
doAction Back                = modifyCtx back >> continue
doAction Delete              = modifyCtx delete >> continue
doAction (Modify f)          = modifyCtx (modifyPresent f) >> continue
doAction (ModifyM m)         = doModifyM m >> continue
doAction (ModifyFwd f)       = modifyCtx (modifyFwd f) >> continue
doAction (ModifyAll f)       = modifyCtx f >> continue
doAction (ModifyAllM f)      = doModifyAllM f >> continue
doAction (ModifyState f)     = modify f >> continue
doAction (ModifyStateM f)    = doModifyStateM f >> continue
doAction (ModifyBack f)      = modifyCtx (modifyBack f) >> continue
doAction (ModifyWInp p f)    = doModifyPrompt p f >> continue
doAction (ModifyWEditor f g) = doModifyWithEditor f g >> continue
doAction (InsFwd p f)        = doInsPrompt p f >>= modifyCtx . insfwd >> continue
doAction (InsBack p f)       = doInsPrompt p f >>= modifyCtx . insback >> continue
doAction (Output f)          = doOutput f >> continue
doAction Cancel              = doCancel
doAction (Comp a b)          = doAction a >> doAction b
doAction (Done f)            = doQuit f
doAction (Seq as)            = fmap (fmap and . sequence) $ mapM doAction as
doAction (Help _ a)          = doAction a

continue :: Monad m => Editor a u m (Maybe Bool)
continue = return $ Just True


doQuit :: MonadIO m
       => (LState a u -> m (Maybe (LState a u)))
       -> Editor a u m (Maybe Bool)
doQuit f = do s <-get
              (lift $ f s) >>= maybe (return (Just True)) ((>> quit) . put)
  where quit = io $ yesNo "Save? [y/N] "
               (Just False)  -- quit with result
               (Just True)   -- continue editing

-- | Prompt the user to confirm a cancel.
doCancel :: MonadIO m => Editor a u m (Maybe Bool)
doCancel = io $ 
  yesNo "Discard all edits, are you SURE? [y/N] "
  Nothing      -- quit with nothing
  (Just True)  -- continue editing

-- Ask yesNo question
yesNo :: String -- ^ question
         -> a -> a -> IO a
yesNo q a b = do putStr q
                 x <- getChar
                 return $ if x `elem` ("yY"::String) then a else b

-- | Move the focus one element forward, unless we are at the end of
-- | the list.  If we are at the end of a list and there is a
-- | continuation, run it and append the generated elements, moving to
-- | the first of the new elements; otherwise do nothing.
doFwd :: MonadIO m => Editor e u m ()
doFwd = do
  LS{ctx=z,cont=s} <- get
  case (future z, s) of
        ([], Just (LC c)) -> do (newElts, cont') <- io c
                                modifyCtx (fwd . modifyFwd (++newElts))
                                (LS l _ u) <- get
                                put (LS l cont' u)
        ([], Nothing)     -> return ()
        _                 -> modifyCtx fwd

-- | Perform a ModifyM action by running the given IO action and
-- | using it to replace the currently focused element.
doModifyM :: Monad m => (e -> m e) -> Editor e u m ()
doModifyM m = do
  pr <- gets $ present . ctx 
  lift (m pr) >>= modifyCtx . modifyPresent . const

-- | Perform a ModifyWInp action by prompting the user and using their
-- | input to modify the currently focused element.
doModifyPrompt :: MonadIO m => String -> (String -> e -> e) -> Editor e u m ()
doModifyPrompt p f = do
  io $ putStr p
  inp <- io getLine
  modifyCtx (modifyPresent $ f inp)

doModifyWithEditor :: MonadIO m =>
  (e -> String) -> (String -> e -> e) -> Editor e u m ()
doModifyWithEditor toStr fromStr = do
  pr <- gets $ present . ctx 
  editTmpFile pr >>= modifyCtx . modifyPresent . fromStr
 where editTmpFile z = io $ do
         (tmp,h) <- openTempFile "/tmp" "zipedit.txt"
         hPutStr h $ toStr z
         hClose h
         _ <- system $ "$EDITOR " ++ tmp
         txt <- readFile tmp
         removeFile tmp
         return txt

-- | Prompt the user, convert their input to an element, and return
-- | the element.
doInsPrompt :: MonadIO m => String -> (String -> e) -> Editor e u m e
doInsPrompt p f = do
  io $ putStr p
  f `fmap` io getLine

-- | Output a function of the currently focused element.
doOutput :: MonadIO m => (e -> String) -> Editor e u m ()
doOutput f = do
  io . putStr . f =<< gets (present . ctx)
