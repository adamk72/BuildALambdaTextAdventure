{-# LANGUAGE DeriveGeneric #-}

module Core.JSON.GameEnv (GameEnvironmentJSON (..), loadGameEnvironmentJSON) where

import           Core.JSON.Types      (Metadata (..), WorldJSON (..))
import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           GHC.Generics         (Generic)

newtype GameEnvironmentJSON = GameEnvironmentJSON
  { unGameEnvironment :: (Metadata, Maybe WorldJSON)
  }
  deriving (Show, Eq, Generic)

instance FromJSON GameEnvironmentJSON where
  parseJSON = withObject "GameEnvironment" $ \v -> do
    metadata <- v .: "metadata"
    worldJSON <- v .:? "world"
    return $ GameEnvironmentJSON (metadata, worldJSON)

loadGameEnvironmentJSON :: FilePath -> IO (Either String (Metadata, Maybe WorldJSON))
loadGameEnvironmentJSON filePath = do
  jsonData <- B.readFile filePath
  case eitherDecode jsonData of
    Left err                        -> return $ Left $ "Error parsing JSON: " ++ err
    Right (GameEnvironmentJSON env) -> return $ Right env
