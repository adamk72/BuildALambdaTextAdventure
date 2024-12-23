{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}

module Entity.Class.EntityBase (HasEntityBase (..), getId, getName, getTags, isOfType) where

import           Data.Text           hiding (elem)
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

getId :: (HasEntityBase a) => Entity a -> EntityId
getId = entityId . getBase

getTags :: (HasEntityBase a) => Entity a -> Maybe [Text]
getTags = entityTags . getBase

getName :: (HasEntityBase a) => Entity a -> Text
getName = entityName . getBase

isOfType :: (HasEntityBase a) => Entity a -> Text -> Bool
isOfType entity targetTag = case getTags entity of
  Nothing    -> False
  Just types -> targetTag `elem` types
