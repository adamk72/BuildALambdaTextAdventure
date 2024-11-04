{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE PatternSynonyms #-}

module Core.State.Entity (module Core.State.Entity) where

import           GHC.Generics         (Generic)
import           Core.State.TaggedEntity


data Entity = Entity {
    entityTag :: TaggedEntity,
    entityType :: EntityType
} deriving (Show, Eq, Generic)

data EntityType = CharacterType | InteractableType
    deriving (Show, Eq, Generic)

type Character = Entity
type Interactable = Entity

pattern Character :: TaggedEntity -> Entity
pattern Character t <- Entity t CharacterType
    where Character t = Entity t CharacterType

pattern Interactable :: TaggedEntity -> Entity
pattern Interactable t <- Entity t InteractableType
    where Interactable t = Entity t InteractableType

mkCharacter :: TaggedEntity -> Entity
mkCharacter t = Entity t CharacterType

mkInteractable :: TaggedEntity -> Entity
mkInteractable t = Entity t InteractableType

isCharacter :: Entity -> Bool
isCharacter (Entity _ CharacterType) = True
isCharacter _ = False

isInteractable :: Entity -> Bool
isInteractable (Entity _ InteractableType) = True
isInteractable _ = False

instance Tagged Entity where
    getTag = tag . entityTag
    getName = name . entityTag
    getLocation = location . entityTag
    getInventory = inventory . entityTag