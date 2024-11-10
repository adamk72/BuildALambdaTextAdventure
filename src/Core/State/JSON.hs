{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Core.State.JSON (module Core.State.JSON) where

import           Core.State.Entity
import           Core.State.GameState
import           Core.State.Location
import           Core.State.TaggedEntity
import           Data.Aeson
import           Data.Aeson.Types        (Parser)
import qualified Data.ByteString.Lazy    as B
import qualified Data.List               as List
import           Data.Maybe
import           Data.Text               (Text)
import           GHC.Generics            (Generic)

-- needed to create this in order to overcome a cycle problem that kept occurring while breaking the old State.hs file into separate files.
newtype GameEnvironmentJSON = GameEnvironmentJSON { unGameEnvironment :: GameEnvironment }
    deriving (Show, Eq, Generic)

data EntityJSON = EntityJSON {
    jTag              :: Text,
    jName             :: Text,
    jLocTag           :: Maybe Text,
    jHasInventorySlot :: Maybe Bool
} deriving (Show, Eq, Generic)

instance FromJSON EntityJSON where
    parseJSON = withObject "EntityJSON" $ \v ->
        EntityJSON
            <$> v .: "tag"
            <*> v .: "name"
            <*> v .: "locationTag"
            <*> v .:? "hasInventorySlot"

data GameWorldJSON = GameWorldJSON {
    jStartingActorTag :: Text,
    jPlayableActors   :: [EntityJSON],
    jLocations        :: [Location],
    jItems            :: [EntityJSON]
} deriving (Show, Eq, Generic)

instance FromJSON GameWorldJSON where
    parseJSON = withObject "GameWorldJSON" $ \v ->
        GameWorldJSON
            <$> v .: "startingActor"
            <*> v .: "characters"
            <*> v .: "locations"
            <*> v .: "items"

-- Define FromJSON for the wrapper instead of GameEnvironment directly
instance FromJSON GameEnvironmentJSON where
    parseJSON = withObject "GameEnvironment" $ \v -> do
        metadata <- v .: "metadata"
        worldJSON <- v .:? "world"
        case worldJSON of
            Nothing -> return $ GameEnvironmentJSON $ GameEnvironment metadata Nothing
            Just worldData -> do
                let locs = jLocations worldData
                    items = jItems worldData
                playableActors <- mapM (convertActor locs items) (jPlayableActors worldData)
                gwItems <- mapM (convertItem locs items) (jItems worldData)
                startingActor <- case findStartingActor (jStartingActorTag worldData) playableActors of
                    Right actor -> return actor
                    Left err    -> fail err
                let world = GameWorld
                        { gwActiveActor = startingActor
                        , gwPlayableActors = playableActors
                        , gwLocations = locs
                        , gwItems = gwItems
                        }
                return $ GameEnvironmentJSON $ GameEnvironment metadata (Just world)

findStartingActor :: Text -> [Actor] -> Either String Actor
findStartingActor startingTag actors =
    case List.find (\c -> getTag c == startingTag) actors of
        Just actor -> Right actor
        Nothing    -> Left $ "Starting character with tag " ++ show startingTag ++ " not found"

convertActor :: [Location] -> [EntityJSON] -> EntityJSON -> Parser Actor
convertActor = convertEntityWithType ActorType

convertItem :: [Location] -> [EntityJSON] -> EntityJSON -> Parser Item
convertItem = convertEntityWithType ItemType

-- Todo: in blog, discuss why EntityJSON{..} with Wild Records didn't work here because of the naming conflict
convertEntityWithType :: EntityType -> [Location] -> [EntityJSON] -> EntityJSON -> Parser Entity
convertEntityWithType entityType locs items item = do
    case jLocTag item of
        Just targetTag ->
            case List.find (\loc -> locTag loc == targetTag) locs of
                Just loc -> makeEntity item loc
                Nothing -> case List.find (\itm -> jTag itm == targetTag &&
                                           fromMaybe False (jHasInventorySlot itm)) items of
                        Just containerItem ->
                            -- Create proper inventory location for items in containers
                            makeEntity item (Location
                                { locTag = jTag containerItem <> "-inventory"  -- unique inventory location
                                , locName = "inside " <> jName containerItem
                                , destinationTags = []
                                })
                        Nothing -> fail $ "Neither location nor container item found with tag " ++ show targetTag
            where
                makeEntity itm loc = return $ Entity
                    { entityTag = TaggedEntity
                        { tag = jTag itm
                        , name = jName itm
                        , location = loc
                        , inventory = addInventorySlot itm
                        }
                    , entityType = entityType
                    }
                addInventorySlot itm =
                    case (entityType, jHasInventorySlot itm) of
                        (ActorType, _) ->
                            Just Location { locTag = jTag itm, locName = "your pockets", destinationTags = [] }
                        (_, Just True) ->
                            Just Location { locTag = jTag itm <> "-inventory", locName = "inside " <> jName itm, destinationTags = [] }
                        _ -> Nothing
        Nothing -> fail "No location tag provided for entity"

loadGameEnvironmentJSON :: FilePath -> IO (Either String GameEnvironment)
loadGameEnvironmentJSON filePath = do
    jsonData <- B.readFile filePath
    case eitherDecode jsonData of
        Left err                        -> return $ Left $ "Error parsing JSON: " ++ err
        Right (GameEnvironmentJSON env) -> return $ Right env  -- Unwrap the newtype here
