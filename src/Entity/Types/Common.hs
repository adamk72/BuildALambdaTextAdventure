{-# LANGUAGE DeriveGeneric #-}

module Entity.Types.Common (ActorId, EntityId (..), InventoryId, ItemId, LocationId) where

import           Data.Text    (Text)
import           GHC.Generics (Generic)

newtype EntityId = EntityId {unEntityId :: Text}
  deriving (Show, Eq, Ord, Generic)

type LocationId = EntityId

type ActorId = EntityId

type ItemId = EntityId

type InventoryId = EntityId
