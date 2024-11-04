{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE PatternSynonyms #-}

module Core.State.Entity (module Core.State.Entity) where

import           GHC.Generics         (Generic)
import           Core.State.TaggedEntity


data Entity = Entity {
    entityTag :: TaggedEntity,
    entityType :: EntityType
} deriving (Show, Eq, Generic)

data EntityType = CharacterType | ItemType
    deriving (Show, Eq, Generic)

type Character = Entity
type Item = Entity

pattern Character :: TaggedEntity -> Entity
pattern Character t <- Entity t CharacterType
    where Character t = Entity t CharacterType

pattern Item :: TaggedEntity -> Entity
pattern Item t <- Entity t ItemType
    where Item t = Entity t ItemType

mkCharacter :: TaggedEntity -> Entity
mkCharacter t = Entity t CharacterType

mkItem :: TaggedEntity -> Entity
mkItem t = Entity t ItemType

isCharacter :: Entity -> Bool
isCharacter (Entity _ CharacterType) = True
isCharacter _ = False

isItem :: Entity -> Bool
isItem (Entity _ ItemType) = True
isItem _ = False

instance Tagged Entity where
    getTag = tag . entityTag
    getName = name . entityTag
    getLocation = location . entityTag
    getInventory = inventory . entityTag