module Command.TestUtils (module Command.TestUtils) where

import           Command.Common
import           Control.Monad.State
import           Core.State
import           Data.Text           (Text)
import           Test.Hspec
import           Parser.Types        (Expression)


-- | Run a command with an Expression
runCommand :: CommandExecutor -> Expression -> GameWorld -> (Text, GameWorld)
runCommand executor expr = runState (executor expr)

-- Common test context helpers
verifyStartLocation :: GameWorld -> Text -> Expectation
verifyStartLocation gw expectedLoc = do
    let acLoc = locTag $ getActiveActorLoc gw
    acLoc `shouldBe` expectedLoc
