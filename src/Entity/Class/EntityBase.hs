{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
module Entity.Class.EntityBase (module Entity.Class.EntityBase) where

import           Data.Text
import           Entity.Entity
import           Entity.Types.Common

class HasEntityBase (a :: EntityType) where
    getBase :: Entity a -> EntityBase a

instance HasEntityBase 'LocationT where
    getBase (Location base _ _) = base

instance HasEntityBase 'ActorT where
    getBase (Actor base _ _) = base

instance HasEntityBase 'ItemT where
    getBase (Item base _ _) = base

getId :: HasEntityBase a => Entity a -> EntityId
getId = entityId . getBase

getTags :: HasEntityBase a => Entity a -> Maybe [Text]
getTags = entityTags . getBase

getName :: HasEntityBase a => Entity a -> Text
getName = entityName . getBase
