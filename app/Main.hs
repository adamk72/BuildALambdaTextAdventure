--TextAdventure.hs
--Copyright Laurence Emms 2018
--Text adventure executable
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import           CmdOptions                 as Cmd (parse, showHelp)
import           Control.Monad              (void)
import           Data.Map                   (Map)
import           Data.Text                  as T (Text, concat, intercalate,
                                                  pack, unpack)
import           JsonProcessing             as Help (AdventureDetail (description, fullName, shortName),
                                                     getJsonFilePaths,
                                                     processJsonFiles,
                                                     storyDirectory)
import           NarrativeGraph             (Flags, Inventory, Scene, SceneKey,
                                             makeNarrativeGraph)
import           NaturalLanguageLexer       (Prepositions, Tokens, Verbs)
import           PrintUtils                 (printHelp, printIntro)
import qualified Stories.DummyAdventure     as D (allPrepositions, allScenes,
                                                  allTokens, allVerbs,
                                                  defaultScene, gameIntro,
                                                  startFlags, startInventory,
                                                  startScene)
import qualified Stories.Trial as Trial (allPrepositions, allScenes,
                                                  allTokens, allVerbs,
                                                  defaultScene, gameIntro,
                                                  startFlags, startInventory,
                                                  startScene)
import qualified Stories.NightmareAdventure as N (allPrepositions, allScenes,
                                                  allTokens, allVerbs,
                                                  defaultScene, gameIntro,
                                                  startFlags, startInventory,
                                                  startScene)
import           System.Environment         as E (getArgs)
import           System.IO                  (hFlush, stdout)
import           TextAdventureCore          (adventure)
import           TextReflow                 (reflowPutStr)

main :: IO ()
main =
    do
    jsonPaths <- getJsonFilePaths storyDirectory
    content <- processJsonFiles jsonPaths
    E.getArgs >>= \case
        -- Todo: remove this textual context.
        ["-a", "Dummy Adventure"]       -> runDummy
        ["-a", "Dummy"]                 -> runDummy
        ["-a", "Trial Story"]           -> runTrial
        ["-a", "Trial"]                 -> runTrial
        ["-a", "Nightmare Adventure"]   -> runNightmare
        ["-a", "Nightmare"]             -> runNightmare
        -- Todo: Give a warning; still doesn't check the adventure code isn't setup here in Main.hs
        -- but that might solve itself as I try to make adventures independent of Main.hs.
        ["-a", invalid]                 -> do
            putStrLn $ "Invalid adventure name: " ++ invalid ++ "\n"
            let adventureList = T.unpack $ T.intercalate "\n" (concatResults content)
            Cmd.showHelp adventureList
        _                               -> displayHelp (concatResults content)
        where
            concatResults :: [Either String AdventureDetail] -> [Text]
            concatResults = map formatResult
                where
                    formatResult (Right adv) =
                        T.concat [fullName adv, " (", shortName adv, ") -", description adv]
                    formatResult (Left err)= T.concat [T.pack err]

displayHelp :: [Text]-> IO ()
-- displayHelp t = void $ Cmd.parse (T.unpack $ T.intercalate "\n" t)
displayHelp = void . Cmd.parse . T.unpack . T.intercalate "\n"

runDummy :: IO ()
runDummy = runGame D.allVerbs D.allPrepositions D.allTokens D.gameIntro D.defaultScene D.startScene D.startInventory D.startFlags D.allScenes

runNightmare :: IO ()
runNightmare = runGame N.allVerbs N.allPrepositions N.allTokens N.gameIntro N.defaultScene N.startScene N.startInventory N.startFlags N.allScenes

runTrial :: IO ()
runTrial = runGame Trial.allVerbs Trial.allPrepositions Trial.allTokens Trial.gameIntro Trial.defaultScene Trial.startScene Trial.startInventory Trial.startFlags Trial.allScenes

runGame :: Verbs -> Prepositions -> Tokens -> String ->
    NarrativeGraph.Scene ->
    String ->
    NarrativeGraph.Inventory ->
    NarrativeGraph.Flags ->
    (Data.Map.Map NarrativeGraph.SceneKey NarrativeGraph.Scene, [NarrativeGraph.SceneKey]) ->
    IO ()
runGame verbs prepositions tokens gameIntro defaultScene startScene startInventory startFlags allScenes =
    printIntro >>
    reflowPutStr gameIntro >>
    putStr "\n" >>
    printHelp >>
    putStr "\n\n\n" >>
    hFlush stdout >>
    adventure verbs prepositions tokens (makeNarrativeGraph adventureScenes endScenes defaultScene) (Just (startScene, startInventory, startFlags)) >>
    return ()
        where (adventureScenes, endScenes) = allScenes
