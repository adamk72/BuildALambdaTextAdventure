module MainSpec (spec) where

import           Command.Go
import           Command.Common
import           Control.Exception.Base (finally)
import           Data.Text              (unpack)
import           Prelude                hiding (sin)
import           System.Exit
import           System.IO              hiding (stdin, stdout)
import           System.Process         (CreateProcess (std_in, std_out),
                                         ProcessHandle, StdStream (CreatePipe),
                                         createProcess, proc, terminateProcess,
                                         waitForProcess)
import           Test.Hspec

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

{- The `finally` version -}
actionWrapper :: ((Handle, Handle, ProcessHandle) -> IO a) -> IO a
actionWrapper testAction = do
    result <- createProcess launchWithPipes
    case result of
        (Just stdin, Just stdout, _, ph) -> do
            let cleanup = closeStdInOut stdin stdout ph
            testAction (stdin, stdout, ph) `finally` cleanup
        (stdin, stdout, _, ph) -> do
            let cleanup = closeHandles stdin stdout ph
            cleanup  -- Clean up immediately if we didn't get valid handles
            fail "Failed to get process handles"

{- Using `bracket` instead.
-- this actually handles things slightly different with the tests;
-- when I went from this to the above version, I encountered a problem where it
-- got caught on an Error that the this `bracket` version ignored.
actionWrapper :: ((Handle, Handle, ProcessHandle) -> IO a) -> IO a
actionWrapper testAction =
    bracket
        (createProcess launchWithPipes)
        (\case
            (Just stdin, Just stdout, _, ph) -> closeStdInOut stdin stdout ph
            (stdin, stdout, _, ph) -> closeHandles stdin stdout ph)
        (\case
            (Just stdin, Just stdout, _, ph) -> testAction (stdin, stdout, ph)
            _ -> fail "Failed to get process handles")
-}

spec :: Spec
spec = do
      describe "Wide spectrum checks" $ do
        context "Basic inputs and outputs" $ do
            it "should exit successfully with :q command" $ do
                actionWrapper $ \(stdin, stdout, ph) -> do
                    hPutStrLn stdin ":q"
                    hFlush stdin
                    goodbye <- hGetLine stdout
                    goodbye `shouldBe` "λ> Thanks for playing!"
                    exitCode <- waitForProcess ph
                    exitCode `shouldBe` ExitSuccess

            it "should handle unknown inputs" $ do
                actionWrapper $ \(stdin, stdout, _ph) -> do
                    hPutStrLn stdin "hello"
                    hFlush stdin
                    response <- hGetLine stdout
                    response `shouldBe` "λ> Don't know how to hello."

            it "should handle unknown gwLocations" $ do
                actionWrapper $ \(stdin, stdout, _ph) -> do
                    hPutStrLn stdin "go foo"
                    hFlush stdin
                    response <- hGetLine stdout
                    response `shouldBe` "λ> " <> unpack (renderMessage $ NoPath "foo")


