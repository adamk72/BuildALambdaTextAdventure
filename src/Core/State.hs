{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

-- Todo: later, fix this so only the state items are exported
module Core.State (module Core.State) where

import           Control.Monad        (mzero)
import           Data.Aeson
import           Data.Aeson.Types     (Parser)
import qualified Data.ByteString.Lazy as B
import qualified Data.List            as List
import           Data.Text            (Text)
import           GHC.Generics         (Generic)

data Entity = Entity {
    entityTag  :: TaggedEntity,
    entityType :: EntityType
} deriving (Show, Eq, Generic)

data EntityType = CharacterType | InteractableType
    deriving (Show, Eq, Generic)

type Character = Entity
type Interactable = Entity

-- Pattern synonyms for backward compatibility
pattern Character :: TaggedEntity -> Entity
pattern Character t <- Entity t CharacterType
    where Character t = Entity t CharacterType

pattern Interactable :: TaggedEntity -> Entity
pattern Interactable t <- Entity t InteractableType
    where Interactable t = Entity t InteractableType

-- Smart constructors
mkCharacter :: TaggedEntity -> Entity
mkCharacter t = Entity t CharacterType

mkInteractable :: TaggedEntity -> Entity
mkInteractable t = Entity t InteractableType

-- Helper functions for type checking
isCharacter :: Entity -> Bool
isCharacter (Entity _ CharacterType) = True
isCharacter _ = False

isInteractable :: Entity -> Bool
isInteractable (Entity _ InteractableType) = True
isInteractable _ = False

-- The rest of your original character functions can be preserved
setCharLoc :: Location -> Character -> Character
setCharLoc newLoc char =
    char { entityTag = (entityTag char) { location = newLoc } }

getActiveCharLocFromGW :: (GameWorld -> Character) -> GameWorld -> Location
getActiveCharLocFromGW ac gw = getLocation $ ac gw

data TaggedEntity = TaggedEntity
    { tag      :: Text
    , name     :: Text
    , location :: Location
    } deriving (Show, Eq, Generic, FromJSON)

class Tagged a where
    getTag :: a -> Text
    getName :: a -> Text
    getLocation :: a -> Location

instance Tagged Entity where
    getTag = tag . entityTag
    getName = name . entityTag
    getLocation = location . entityTag

-- example usages
{-
findByTag :: Tagged a => Text -> [a] -> Maybe a
findByTag searchTag = find (\x -> getTag x == searchTag)

displayName :: Tagged a => a -> Text
displayName = getName
-}

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

convertCharacter :: [Location] -> EntityJSON -> Parser Entity
convertCharacter = convertEntityWithType CharacterType

convertInteractable :: [Location] -> EntityJSON -> Parser Entity
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
                        }
                    , entityType = entityType
                    }
                Nothing -> fail $ "Location with tag " ++ show targetTag ++ " not found"
        Nothing -> fail "No location tag provided for entity"

setEntityLoc :: Location -> Entity -> Entity
setEntityLoc newLoc entity =
    entity { entityTag = (entityTag entity) { location = newLoc } }

getActiveEntityLocFromGW :: (GameWorld -> Entity) -> GameWorld -> Location
getActiveEntityLocFromGW ae gw = getLocation $ ae gw

data Location = Location {
  locTag          :: Text,
  locName         :: Text,
  destinationTags :: [Text]
} deriving (Show, Eq, Generic)

instance FromJSON Location where
  parseJSON (Object v) =
    Location <$> v .: "tag"
             <*> v .: "name"
             <*> v .:? "destinationTags" .!= []
  parseJSON _ = mzero

data GameWorld = GameWorld {
    activeCharacter    :: Entity,
    playableCharacters :: [Entity],
    locations          :: [Location],
    interactables      :: [Entity]
} deriving (Show, Eq, Generic)

data GameWorldJSON = GameWorldJSON {
    jStartingCharacter  :: EntityJSON,
    jPlayableCharacters :: [EntityJSON],
    jLocations          :: [Location],
    jInteractables      :: [EntityJSON]
} deriving (Show, Eq, Generic)

instance FromJSON GameWorldJSON where
    parseJSON = withObject "GameWorldJSON" $ \v ->
        GameWorldJSON
            <$> v .: "startingCharacter"
            <*> v .: "playableCharacters"
            <*> v .: "locations"
            <*> v .: "interactables"

instance FromJSON GameWorld where
    parseJSON v = do
        worldJSON <- parseJSON v :: Parser GameWorldJSON
        let locs = jLocations worldJSON
        startingChar <- convertCharacter locs (jStartingCharacter worldJSON)
        playableChars <- mapM (convertCharacter locs) (jPlayableCharacters worldJSON)
        interactables <- mapM (convertInteractable locs) (jInteractables worldJSON)
        return GameWorld
            { activeCharacter = startingChar
            , playableCharacters = playableChars
            , locations = locs
            , interactables = interactables
            }

data Metadata = Metadata {
    title       :: Text,
    launchTag   :: Text,
    description :: Text,
    version     :: Text,
    author      :: Text
} deriving (Show, Eq, Generic, FromJSON)

data GameEnvironment = GameEnvironment {
    metadata :: Metadata,
    world    :: Maybe GameWorld
} deriving (Show, Eq, Generic, FromJSON)

loadGameEnvironmentJSON :: FilePath -> IO (Either String GameEnvironment)
loadGameEnvironmentJSON filePath = do
  jsonData <- B.readFile filePath
  case eitherDecode jsonData of
        Left err        -> return $ Left $ "Error parsing JSON: " ++ err
        Right worldData -> return $ Right worldData
