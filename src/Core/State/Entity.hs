{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE PatternSynonyms #-}

module Core.State.Entity (module Core.State.Entity) where

import           Core.State.TaggedEntity
import           GHC.Generics            (Generic)

data Entity = Entity {
    entityTag  :: TaggedEntity,
    entityType :: EntityType
} deriving (Show, Eq, Generic)

data EntityType = ActorType | ItemType
    deriving (Show, Eq, Generic)

type Actor = Entity
type Item = Entity

pattern Actor :: TaggedEntity -> Entity
pattern Actor t <- Entity t ActorType
    where Actor t = Entity t ActorType

pattern Item :: TaggedEntity -> Entity
pattern Item t <- Entity t ItemType
    where Item t = Entity t ItemType

mkActor :: TaggedEntity -> Entity
mkActor t = Entity t ActorType

mkItem :: TaggedEntity -> Entity
mkItem t = Entity t ItemType

isActor :: Entity -> Bool
isActor (Entity _ ActorType) = True
isActor _                    = False

isItem :: Entity -> Bool
isItem (Entity _ ItemType) = True
isItem _                   = False

-- Helper functions to filter by type
filterActors :: [Entity] -> [Actor]
filterActors = filter isActor

filterItems :: [Entity] -> [Item]
filterItems = filter isItem

instance Tagged Entity where
    getTag = tag . entityTag
    getName = name . entityTag
    getLocation = location . entityTag
    getInventory = inventory . entityTag
