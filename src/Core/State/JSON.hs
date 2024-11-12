{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Core.State.JSON
    ( loadGameEnvironmentJSON
    , GameEnvironmentJSON(..)
    ) where

import Entity.Entity
import           Core.State.JSONTypes
import           Data.Aeson
import qualified Data.ByteString.Lazy    as B
import           GHC.Generics            (Generic)
import Core.State.GameState (Metadata)

-- | Wrapper for GameEnvironment with JSON data
newtype GameEnvironmentJSON = GameEnvironmentJSON {
    unGameEnvironment :: (Metadata, Maybe GameWorldJSON)
} deriving (Show, Eq, Generic)

instance FromJSON GameEnvironmentJSON where
    parseJSON = withObject "GameEnvironment" $ \v -> do
        metadata <- v .: "metadata"
        worldJSON <- v .:? "world"
        return $ GameEnvironmentJSON (metadata, worldJSON)

-- | Load and parse a game environment from a JSON file
loadGameEnvironmentJSON :: FilePath -> IO (Either String (Metadata, Maybe GameWorldJSON))
loadGameEnvironmentJSON filePath = do
    jsonData <- B.readFile filePath
    case eitherDecode jsonData of
        Left err -> return $ Left $ "Error parsing JSON: " ++ err
        Right (GameEnvironmentJSON env) -> return $ Right env