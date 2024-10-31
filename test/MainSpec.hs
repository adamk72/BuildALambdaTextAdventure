{-# LANGUAGE LambdaCase #-}
module MainSpec (spec) where

import           Control.Exception (bracket)
import           Prelude           hiding (sin)
import           System.Exit
import           System.IO         hiding (stdin, stdout)
import           System.Process
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

spec :: Spec
spec = do
      describe "Basic inputs and outputs" $ do
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
                _ <- hGetLine stdout
                response <- hGetLine stdout
                response `shouldBe` "λ> Don't know how to hello."

        it "should handle unknown locations" $ do
            actionWrapper $ \(stdin, stdout, _ph) -> do
                hPutStrLn stdin "go foo"
                hFlush stdin
                _ <- hGetLine stdout
                response <- hGetLine stdout
                response `shouldBe` "λ> Unknown location: foo."
