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
    jTag    :: Text,
    jName   :: Text,
    jLocTag :: Maybe Text
} deriving (Show, Eq, Generic)

instance FromJSON EntityJSON where
    parseJSON = withObject "EntityJSON" $ \v ->
        EntityJSON
            <$> v .: "tag"
            <*> v .: "name"
            <*> v .: "locationTag"

data GameWorldJSON = GameWorldJSON {
    jStartingCharacterTag :: Text,
    jPlayableCharacters   :: [EntityJSON],
    jLocations            :: [Location],
    jInteractables        :: [EntityJSON]
} deriving (Show, Eq, Generic)

instance FromJSON GameWorldJSON where
    parseJSON = withObject "GameWorldJSON" $ \v ->
        GameWorldJSON
            <$> v .: "startingCharacter"
            <*> v .: "playableCharacters"
            <*> v .: "locations"
            <*> v .: "interactables"



-- Define FromJSON for the wrapper instead of GameEnvironment directly
instance FromJSON GameEnvironmentJSON where
    parseJSON = withObject "GameEnvironment" $ \v -> do
        metadata <- v .: "metadata"
        worldJSON <- v .:? "world"
        case worldJSON of
            Nothing -> return $ GameEnvironmentJSON $ GameEnvironment metadata Nothing
            Just worldData -> do
                let locs = jLocations worldData
                playableChars <- mapM (convertCharacter locs) (jPlayableCharacters worldData)
                interactables <- mapM (convertInteractable locs) (jInteractables worldData)
                startingChar <- case findStartingCharacter (jStartingCharacterTag worldData) playableChars of
                    Right char -> return char
                    Left err -> fail err
                let world = GameWorld
                        { activeCharacter = startingChar
                        , playableCharacters = playableChars
                        , locations = locs
                        , interactables = interactables
                        }
                return $ GameEnvironmentJSON $ GameEnvironment metadata (Just world)

findStartingCharacter :: Text -> [Character] -> Either String Character
findStartingCharacter startingTag chars =
    case List.find (\c -> getTag c == startingTag) chars of
        Just char -> Right char
        Nothing -> Left $ "Starting character with tag " ++ show startingTag ++ " not found"

convertCharacter :: [Location] -> EntityJSON -> Parser Character
convertCharacter = convertEntityWithType CharacterType

convertInteractable :: [Location] -> EntityJSON -> Parser Interactable
convertInteractable = convertEntityWithType InteractableType

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
                        , inventory = if entityType == CharacterType then Just [Location { locTag = jTag, locName = "your pockets", destinationTags = [] }] else Nothing
                        }
                    , entityType = entityType
                    }
                Nothing -> fail $ "Location with tag " ++ show targetTag ++ " not found"
        Nothing -> fail "No location tag provided for entity"

loadGameEnvironmentJSON :: FilePath -> IO (Either String GameEnvironment)
loadGameEnvironmentJSON filePath = do
    jsonData <- B.readFile filePath
    case eitherDecode jsonData of
        Left err        -> return $ Left $ "Error parsing JSON: " ++ err
        Right (GameEnvironmentJSON env) -> return $ Right env  -- Unwrap the newtype here
