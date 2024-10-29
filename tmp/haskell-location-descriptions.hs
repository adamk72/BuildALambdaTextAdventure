import qualified Data.Text as T
import qualified Data.Map as Map
import Control.Monad.State
import Data.Text (Text)
import Data.Map (Map)

main :: IO ()
main = testLocations

-- Enhanced Location type with directional descriptions
data Location = Location
    { locationName :: String
    , lookDesc :: String  -- General description when looking around
    , directionDescs :: Map String String  -- Descriptions for each direction
    } deriving Show

data Player = Player {location :: Location} deriving Show

-- Helper function to create a location with all its descriptions
makeLocation :: String -> String -> [(String, String)] -> Location
makeLocation name desc dirDescs = Location
    { locationName = name
    , lookDesc = desc
    , directionDescs = Map.fromList dirDescs
    }

-- Example locations
forestLocation :: Location
forestLocation = makeLocation
    "Dark Forest"
    "You're in a dense, dark forest. Ancient trees tower above you, their branches creating a thick canopy overhead."
    [ ("north", "A narrow path winds through the trees, gradually ascending.")
    , ("south", "The forest gets notably darker in this direction.")
    , ("east", "You see a small clearing in the distance.")
    , ("west", "The trees seem to thin out, suggesting the forest's edge might be that way.")
    ]

meadowLocation :: Location
meadowLocation = makeLocation
    "Sunny Meadow"
    "You're standing in a beautiful meadow filled with wildflowers. The grass sways gently in the breeze."
    [ ("north", "Rolling hills covered in flowers stretch as far as you can see.")
    , ("south", "The edge of a dark forest looms nearby.")
    , ("east", "A sparkling river winds its way through the landscape.")
    , ("west", "A rocky mountain range rises dramatically in the distance.")
    ]

-- Enhanced executeLook function
executeLook :: Player -> Maybe Text -> Text
executeLook player Nothing =
    let loc = location player
    in T.pack $ "You are in " <> locationName loc <> "."

executeLook player (Just "around") =
    let loc = location player
    in T.pack $ "You are in " <> locationName loc <> ". " <> lookDesc loc

executeLook player (Just direction) =
    let loc = location player
        dirStr = T.unpack direction
        defaultDesc = "You look " <> dirStr <> ", but see nothing special."
    in T.pack $ case Map.lookup dirStr (directionDescs loc) of
        Just desc -> "You look " <> dirStr <> ". " <> desc
        Nothing -> defaultDesc

-- Helper function for directional looking (unchanged)
isDirectionalLook :: Text -> Maybe Text
isDirectionalLook input =
    let directions = ["north", "south", "east", "west"]
        prefix = "look "
    in if T.isPrefixOf prefix input
       then let direction = T.drop (T.length prefix) input
            in if direction `elem` directions
               then Just direction
               else Nothing
       else Nothing

-- Main eval function
eval :: Text -> State Player Text
eval input = do
    player <- get
    let lowerInput = T.toLower input
    case lowerInput of
        "look" -> do
            return $ executeLook player Nothing

        "look around" -> do
            return $ executeLook player (Just "around")

        other -> case isDirectionalLook other of
            Just direction -> do
                return $ executeLook player (Just direction)

            Nothing ->
                return $ "I don't know how to '" <> input <> "'."

-- Test function to demonstrate the location-specific descriptions
testLocations :: IO ()
testLocations = do
    let forestPlayer = Player forestLocation
        meadowPlayer = Player meadowLocation
        commands = ["look", "look around", "look north", "look south",
                   "look east", "look west"]

        runCommand player cmd =
            let (result, _) = runState (eval (T.pack cmd)) player
            in T.unpack result

    putStrLn "Testing Dark Forest Location:"
    putStrLn "=========================="
    mapM_ (\cmd -> do
        putStrLn $ "\nCommand: " ++ cmd
        putStrLn $ "Result: " ++ runCommand forestPlayer cmd) commands

    putStrLn "\nTesting Sunny Meadow Location:"
    putStrLn "=========================="
    mapM_ (\cmd -> do
        putStrLn $ "\nCommand: " ++ cmd
        putStrLn $ "Result: " ++ runCommand meadowPlayer cmd) commands
