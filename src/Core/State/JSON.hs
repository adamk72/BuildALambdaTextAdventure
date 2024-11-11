{-# LANGUAGE DeriveGeneric #-}

module Core.State.JSON
    ( EntityJSON(..)
    , GameWorldJSON(..)
    , GameEnvironmentJSON(..)
    , loadGameEnvironmentJSON
    ) where

import           Core.State.GameState
import           Core.State.Location
import           Data.Aeson
import qualified Data.ByteString.Lazy    as B
import           Data.Text               (Text)
import           GHC.Generics            (Generic)

-- | JSON representation of an entity (actor or item)
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

-- | JSON representation of the game world
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

-- | Wrapper for GameEnvironment to handle JSON parsing
newtype GameEnvironmentJSON = GameEnvironmentJSON {
    unGameEnvironment :: GameEnvironment
} deriving (Show, Eq, Generic)

instance FromJSON GameEnvironmentJSON where
    parseJSON = withObject "GameEnvironment" $ \v -> do
        metadata <- v .: "metadata"
        worldJSON <- v .:? "world"
        return $ GameEnvironmentJSON $ GameEnvironment metadata worldJSON

-- | Load and parse a game environment from a JSON file
loadGameEnvironmentJSON :: FilePath -> IO (Either String GameEnvironment)
loadGameEnvironmentJSON filePath = do
    jsonData <- B.readFile filePath
    case eitherDecode jsonData of
        Left err -> return $ Left $ "Error parsing JSON: " ++ err
        Right (GameEnvironmentJSON env) -> return $ Right env