module Game.State
  ( newState
  ) where

import qualified Types
import qualified Pointless

newState :: Types.State
newState =
  Types.State
    { Types.title = "Super Funky Title"
    , Types.keyPressed = Types.KeyNone
    , Types.oscillatingN = 1
    , Types.oscillatingDirection = Pointless.L
    }
