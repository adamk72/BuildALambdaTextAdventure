{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveAnyClass  #-}

module Core.State.TaggedEntity
    ( TaggedEntity(TaggedEntity, tag, name, location)  -- Explicitly export the constructor and field names
    , Tagged(..)
    ) where

import           Data.Text            (Text)
import           GHC.Generics         (Generic)
import           Data.Aeson           (FromJSON)
import           Core.State.Location  (Location)

data TaggedEntity = TaggedEntity
    { tag      :: Text
    , name     :: Text
    , location :: Location
    } deriving (Show, Eq, Generic, FromJSON)

class Tagged a where
    getTag :: a -> Text
    getName :: a -> Text
    getLocation :: a -> Location
