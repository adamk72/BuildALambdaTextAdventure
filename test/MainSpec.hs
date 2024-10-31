{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module MainSpec (spec) where

import           Control.Exception (bracket)
import           System.Exit
import           System.IO hiding (stdout, stdin)
import           System.Process
import           Test.Hspec

launchCmd :: CreateProcess
launchCmd = proc "stack" ["run", "TextAdventure-exe", "--", "-a", "Trial"]

launchWithPipes :: CreateProcess
launchWithPipes = launchCmd { std_in = CreatePipe , std_out = CreatePipe }

spec :: Spec
spec = do
      describe "Basic inputs and outputs" $ do
        it "should exit successfully with :q command" $ do
            let processConfig = launchWithPipes
            bracket
                (createProcess processConfig)
                (\(Just stdin, Just stdout, _, ph) -> do
                    hClose stdin
                    hClose stdout
                    terminateProcess ph)
                (\(Just stdin, Just stdout, _, ph) -> do
                    _ <- hGetLine stdout  -- skip initial prompt

                    hPutStrLn stdin ":q"
                    hFlush stdin

                    goodbye <- hGetLine stdout
                    goodbye `shouldBe` "λ> Thanks for playing!"

                    exitCode <- waitForProcess ph
                    exitCode `shouldBe` ExitSuccess)

        it "should handle unknown inputs" $ do
            let processConfig = launchWithPipes

            bracket
                (createProcess processConfig)
                (\(Just stdin, Just stdout, _, ph) -> do
                    hClose stdin
                    hClose stdout
                    terminateProcess ph)
                (\(Just stdin, Just stdout, _, _) -> do

                    hPutStrLn stdin "hello"
                    hFlush stdin

                    _ <- hGetLine stdout

                    response <- hGetLine stdout
                    response `shouldBe` "λ> Don't know how to hello.")

        it "should handle unknown locations" $ do
            let processConfig = launchWithPipes

            bracket
                (createProcess processConfig)
                (\(Just stdin, Just stdout, _, ph) -> do
                    hClose stdin
                    hClose stdout
                    terminateProcess ph)
                (\(Just stdin, Just stdout, _, _) -> do

                    hPutStrLn stdin "go foo"
                    hFlush stdin

                    _ <- hGetLine stdout

                    response <- hGetLine stdout
                    response `shouldBe` "λ> Unknown location: foo.")