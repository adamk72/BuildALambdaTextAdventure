{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module Core.State.JSON (module Core.State.JSON) where

import           Core.State.Entity
import           Core.State.GameState
import           Core.State.Location
import           Core.State.TaggedEntity
import           Data.Aeson
import           Data.Aeson.Types        (Parser)
import qualified Data.ByteString.Lazy    as B
import qualified Data.List               as List
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
                playableActors <- mapM (convertActor locs) (jPlayableActors worldData)
                gwItems <- mapM (convertItem locs) (jItems worldData)
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
        Nothing -> Left $ "Starting character with tag " ++ show startingTag ++ " not found"

convertActor :: [Location] -> EntityJSON -> Parser Actor
convertActor = convertEntityWithType ActorType

convertItem :: [Location] -> EntityJSON -> Parser Item
convertItem = convertEntityWithType ItemType

convertEntityWithType :: EntityType -> [Location] -> EntityJSON -> Parser Entity
convertEntityWithType entityType locs EntityJSON{..} = do
    case jLocTag of
        Just targetTag ->
            case List.find (\loc -> locTag loc == targetTag) locs of
                Just loc -> return $ Entity
                    { entityTag = TaggedEntity
                        { tag = jTag
                        , name = jName
                        , location = loc
                        , inventory = addInventorySlot
                        }
                    , entityType = entityType
                    }
                    where
                        addInventorySlot =
                            case (entityType, jHasInventorySlot) of
                                (ActorType, _) ->
                                    Just Location { locTag = jTag, locName = "your pockets", destinationTags = [] }
                                (_, Just True) ->
                                    Just Location { locTag = jTag, locName = jTag, destinationTags = [] }
                                _ -> Nothing

                Nothing -> fail $ "Location with tag " ++ show targetTag ++ " not found"
        Nothing -> fail "No location tag provided for entity"

loadGameEnvironmentJSON :: FilePath -> IO (Either String GameEnvironment)
loadGameEnvironmentJSON filePath = do
    jsonData <- B.readFile filePath
    case eitherDecode jsonData of
        Left err        -> return $ Left $ "Error parsing JSON: " ++ err
        Right (GameEnvironmentJSON env) -> return $ Right env  -- Unwrap the newtype here
