{-# LANGUAGE DeriveGeneric #-}

module Entity.Types.Common (module Entity.Types.Common) where

import           Data.Text    (Text)
import           GHC.Generics (Generic)

newtype EntityId = EntityId { unEntityId :: Text }
    deriving (Show, Eq, Ord, Generic)

type LocationId = EntityId
type ActorId = EntityId
type ItemId = EntityId
type MovableId = EntityId
type InventoryId = EntityId
