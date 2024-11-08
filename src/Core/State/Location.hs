{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module Core.State.Location
    ( Location(..)
    ) where

import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics (Generic)

data Location = Location {
    locTag          :: Text,
    locName         :: Text,
    destinationTags :: [Text]
} deriving (Show, Eq, Generic)

instance FromJSON Location where
    parseJSON = withObject "Location" $ \v -> do
        locTag <- v .: "tag"
        locName <- v .: "name"
        destinationTags <- v .: "destinationTags"
        return Location{..}
