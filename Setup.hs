import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Haddock
main = do
  defaultMainWithHooks simpleUserHooks{
    haddockHook = \p l h flags -> haddockHook simpleUserHooks p l h flags{
        haddockHoogle       = Flag True,
        haddockHtml         = Flag True,
        haddockProgramArgs  = [("-q",["aliased"])], -- does not seam to do anything
        haddockExecutables  = Flag True,
        haddockHscolour     = Flag True
        }
    }

