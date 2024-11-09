module MainSpec (spec) where

import           Control.Exception.Base (finally)
import           Core.Message
import           Data.Text
import           Prelude                hiding (sin)
import           System.Exit
import           System.IO              hiding (stdin, stdout)
import           System.Process
    (CreateProcess (std_in, std_out), ProcessHandle, StdStream (CreatePipe), createProcess, proc, terminateProcess,
    waitForProcess)
import           Test.Hspec

-- Process Setup Helpers
launchCmd :: CreateProcess
launchCmd = proc "stack" ["run", "TextAdventure-exe", "--", "-a", "Trial"]

launchWithPipes :: CreateProcess
launchWithPipes = launchCmd { std_in = CreatePipe , std_out = CreatePipe }

closeStdInOut :: Handle -> Handle -> ProcessHandle -> IO ()
closeStdInOut sin sout ph = do
    hClose sin
    hClose sout
    terminateProcess ph

closeHandles :: Maybe Handle -> Maybe Handle -> ProcessHandle -> IO ()
closeHandles sin sout ph = do
    mapM_ hClose sin
    mapM_ hClose sout
    terminateProcess ph

-- Test Action Wrapper
actionWrapper :: ((Handle, Handle, ProcessHandle) -> IO a) -> IO a
actionWrapper testAction = do
    result <- createProcess launchWithPipes
    case result of
        (Just stdin, Just stdout, _, ph) -> do
            let cleanup = closeStdInOut stdin stdout ph
            testAction (stdin, stdout, ph) `finally` cleanup
        (stdin, stdout, _, ph) -> do
            let cleanup = closeHandles stdin stdout ph
            cleanup
            fail "Failed to get process handles"

-- Test Helper Functions
type TestHandles = (Handle, Handle, ProcessHandle)

sendCommand :: Handle -> String -> IO ()
sendCommand stdin cmd = do
    hPutStrLn stdin cmd
    hFlush stdin

getResponse :: Handle -> IO String
getResponse = hGetLine

testCommand :: TestHandles -> String -> String -> IO ()
testCommand (stdin, stdout, _) cmd expectedResponse = do
    sendCommand stdin cmd
    response <- getResponse stdout
    response `shouldBe` "λ> " ++ expectedResponse

-- Specs
spec :: Spec
spec = do
    describe "Text Adventure Game Tests" $ do
        context "System Commands" $ do
            it "should exit successfully with :q command" $ do
                actionWrapper $ \(stdin, stdout, ph) -> do
                    sendCommand stdin ":q"
                    response <- getResponse stdout
                    response `shouldBe` "λ> Thanks for playing!"
                    exitCode <- waitForProcess ph
                    exitCode `shouldBe` ExitSuccess

            it "should handle unknown commands gracefully" $ do
                actionWrapper $ \handles -> do
                    testCommand handles "nonsense" "Don't know how to nonsense. Got error: Unknown command."

        context "Navigation Commands" $ do
            it "should handle 'look' command" $ do
                actionWrapper $ \handles -> do
                    testCommand handles "look" "You are in a flowery meadow."

            it "should handle 'look around' command" $ do
                actionWrapper $ \handles -> do
                    testCommand handles "look around"
                        "You are in a flowery meadow. You look around and see a simple bag, a shiny bauble, a large blue phone booth, and a satchel that is inky black on the inside."

            it "should handle valid 'go' commands" $ do
                actionWrapper $ \handles -> do
                    testCommand handles "go cave" "Moving to cave."
                    testCommand handles "look" "You are in a dark cave."

            it "should handle invalid 'go' commands" $ do
                actionWrapper $ \handles -> do
                    testCommand handles "go narnia" (unpack $ renderMessage $ NoPath "narnia")

        context "Inventory Commands" $ do
            it "should show empty inventory" $ do
                actionWrapper $ \handles -> do
                    testCommand handles "inventory" "Your inventory is: nothing"

            it "should handle 'get' commands" $ do
                actionWrapper $ \handles -> do
                    testCommand handles "get bauble" "Moved bauble to Alice the Adventurer"
                    testCommand handles "inventory" "Your inventory is: a shiny bauble"

            it "should handle 'drop' commands" $ do
                actionWrapper $ \handles -> do
                    testCommand handles "get bauble" "Moved bauble to Alice the Adventurer"
                    testCommand handles "drop bauble" "bauble dropped. Your inventory is now: nothing"

            it "should handle invalid 'get' commands" $ do
                actionWrapper $ \handles -> do
                    testCommand handles "get unicorn" "Cannot pick up \"unicorn\"."

            it "should handle invalid 'drop' commands" $ do
                actionWrapper $ \handles -> do
                    testCommand handles "drop unicorn" "You don't have a unicorn to drop."

        context "Container Commands" $ do
            it "should handle 'put' commands with valid containers" $ do
                actionWrapper $ \handles -> do
                    testCommand handles "get bauble" "Moved bauble to Alice the Adventurer"
                    testCommand handles "put bauble in bag" "bauble is now in the bag."

            it "should handle 'put' commands with invalid containers" $ do
                actionWrapper $ \handles -> do
                    testCommand handles "get bauble" "Moved bauble to Alice the Adventurer"
                    testCommand handles "put bauble in tardis" "The tardis is not a container."

            it "should handle malformed 'put' commands" $ do
                actionWrapper $ \handles -> do
                    testCommand handles "put bauble somewhere" "Don't know where to put bauble somewhere."
