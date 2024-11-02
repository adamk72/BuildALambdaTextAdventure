{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

-- Todo: later, fix this so only the state items are exported
module Core.State (module Core.State) where

import           Control.Monad        (mzero)
import           Data.Aeson
import           Data.Aeson.Types     (Parser)
import qualified Data.ByteString.Lazy as B
import qualified Data.List            as List
import           Data.Text            (Text)
import           GHC.Generics         (Generic)

class Tagged a where
    getTag :: a -> Text
    getName :: a -> Text
    getLocation :: a -> Location

-- example usages
{-
findByTag :: Tagged a => Text -> [a] -> Maybe a
findByTag searchTag = find (\x -> getTag x == searchTag)

displayName :: Tagged a => a -> Text
displayName = getName
-}

setCharLoc :: Location -> Character -> Character
setCharLoc newLoc char =
    char { charTag = (charTag char) { location = newLoc } }

getActiveCharLocFromGW :: (GameWorld -> Character) -> GameWorld -> Location
getActiveCharLocFromGW ac gw = getLocation $ ac gw

data TaggedEntity = TaggedEntity
    { tag      :: Text
    , name     :: Text
    , location :: Location
    } deriving (Show, Eq, Generic, FromJSON)

data Metadata = Metadata {
    title       :: Text,
    launchTag   :: Text,
    description :: Text,
    version     :: Text,
    author      :: Text
} deriving (Show, Eq, Generic, FromJSON)

-- Runtime Character structure (no startingLocationTag)
data Character = Character {
    charTag         :: TaggedEntity
} deriving (Show, Eq, Generic)

instance Tagged Character where
    getTag = tag . charTag
    getName = name . charTag
    getLocation = location . charTag

-- JSON parsing structure for Character
data CharacterJSON = CharacterJSON {
    jCharTag    :: Text,
    jCharName   :: Text,
    jCharLocTag :: Maybe Text
} deriving (Show, Eq, Generic)

instance FromJSON CharacterJSON where
    parseJSON = withObject "CharacterJSON" $ \v ->
        CharacterJSON
            <$> v .: "tag"
            <*> v .: "name"
            <*> v .: "locationTag"

-- Convert CharacterJSON to Character by resolving the location
convertCharacter :: [Location] -> CharacterJSON -> Parser Character
convertCharacter locs CharacterJSON{..} = do
    case jCharLocTag of
      Just targetTag ->
        case List.find (\loc -> locTag loc == targetTag) locs of
                  Just loc -> return $ Character
                    { charTag = TaggedEntity
                        { tag = jCharTag
                        , name = jCharName
                        , location = loc
                        }
                    }
                  Nothing -> fail $ "Location with tag " ++ show targetTag ++ " not found"
      Nothing -> fail "No location tag provided for a character."


data InteractableJSON = InteractableJSON {
    jInterTag    :: Text,
    jInterName   :: Text,
    jInterLocTag :: Maybe Text
} deriving (Show, Eq, Generic)

instance FromJSON InteractableJSON where
    parseJSON = withObject "InteractableJSON" $ \v ->
        InteractableJSON
            <$> v .: "tag"
            <*> v .: "name"
            <*> v .: "locationTag"

data Interactable = Interactable {
  interTag :: TaggedEntity
} deriving (Show, Eq, Generic)

instance Tagged Interactable where
    getTag = tag . interTag
    getName = name . interTag
    getLocation = location . interTag

createInteractables :: [Location] -> InteractableJSON -> Parser Interactable
createInteractables locs InteractableJSON{..} = do
    case jInterLocTag of
      Just targetTag ->
        case List.find (\loc -> locTag loc == targetTag) locs of
                  Just loc -> return $ Interactable
                    { interTag = TaggedEntity
                        { tag = jInterTag
                        , name = jInterName
                        , location = loc
                        }
                    }
                  Nothing -> fail $ "Location with tag " ++ show targetTag ++ " not found"
      Nothing -> fail "No starting location tag provided for an interactable."

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
  activeCharacter    :: Character,
  playableCharacters :: [Character],
  locations          :: [Location],
  interactables      :: [Interactable]
} deriving (Show, Eq, Generic)


-- JSON parsing structure for GameWorld
data GameWorldJSON = GameWorldJSON {
    jStartingCharacter  :: CharacterJSON,
    jPlayableCharacters :: [CharacterJSON],
    jLocations          :: [Location],
    jInteractables      :: [InteractableJSON]
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
        interactables <- mapM (createInteractables locs) (jInteractables worldJSON)
        return GameWorld
            { activeCharacter = startingChar
            , playableCharacters = playableChars
            , locations = locs
            , interactables = interactables
            }

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
