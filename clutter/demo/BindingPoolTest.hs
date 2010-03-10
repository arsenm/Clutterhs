{-# LANGUAGE ScopedTypeVariables #-}

import Graphics.UI.Clutter hiding (show, Nothing)
import System.Glib.Signals (on, after)
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Maybe


action (_::Rectangle) str kc ms = print (str,kc,ms) >> return True

main = do
  clutterInit
  stg <- stageGetDefault

  bp <- bindingPoolNew "arst"

  -- LOL on shift-space
  -- Wowomg on ctrl-space
  bindingPoolInstallAction bp "lol" 65 [ShiftMask] action
  bindingPoolInstallAction bp "wow" 65 [ControlMask] action
  bindingPoolInstallAction bp "omg" 65 [ShiftMask, ControlMask] action
  bindingPoolInstallAction bp "aoeu" 65 [] action

  bindingPoolBlockAction bp "lol"
  bindingPoolBlockAction bp "aoeu"
  bindingPoolUnblockAction bp "aoeu"

  bindingPoolFind "I'm a random string" >>=
      (\x -> unless (isNothing x) (putStrLn "Find found something nonexistant. This is bad."))

  --FIXME: Keycode types annoying?

  -- lol should not work / blocked
  -- wow and omg should just work
  -- aoeu should be blocked, then unblocked and work

  stg `on` keyPressEvent $ do kc <- fmap fromIntegral eventKeyCode
                              ms <- eventState
                              liftIO (do x <- bindingPoolActivate bp kc ms stg
                                         print x
                                         return x)

  actorShowAll stg
  clutterMain

