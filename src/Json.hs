{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Json (loadGameEnvironmentJSON, JGameEnvironment(..), JMetadata(..)) where
import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           Data.Text            as T
import           GHC.Generics         (Generic)

data JMetadata = JMetadata {
    title       :: Text,
    launchTag   :: Text,
    description :: Text,
    version     :: Text,
    author      :: Text
} deriving (Show, Generic, FromJSON)

data JCharacter = JCharacter {
  tag              :: Text,
  name             :: Text,
  startingLocation :: Text
} deriving (Show, Generic, FromJSON)

data JLocation = JLocation {
  ltag  :: Text,
  lname :: Text
} deriving (Show, Generic, FromJSON)

data JGameWorld = JGameWorld {
  startingCharacter  :: Text,
  playableCharacters :: [JCharacter],
  locations          :: [JLocation]
} deriving (Show, Generic, FromJSON)

data JGameEnvironment = JGameEnvironment {
    metadata :: JMetadata,
    world    :: JGameWorld
} deriving (Show, Generic, FromJSON)

loadGameEnvironmentJSON :: FilePath -> IO (Either String JGameEnvironment)
loadGameEnvironmentJSON filePath = do
  jsonData <- B.readFile filePath
  case eitherDecode jsonData of
        Left err -> return $ Left $ "Error parsing JSON: " ++ err
        Right worldData -> do
            return $ Right worldData

