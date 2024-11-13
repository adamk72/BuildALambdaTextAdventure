{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module Core.State.Operations
    ( module Core.Ops.Inventory
    , module Core.Ops.Location
    , module Core.State.EntityInfo
    , module Core.State.Operations
    ) where

import           Core.Ops.EntityInfo
import           Core.Ops.Inventory
import           Core.Ops.Location
import qualified Data.Map            as Map
import           Data.Maybe          (isJust)
import           Data.Text           (Text)
import           Entity.Entity
import           Prelude             hiding (getContents)



-- | Move an item into a container
moveItemToContainer :: Tagged a => Entity 'ItemT -> Entity a -> World -> Either Text World
moveItemToContainer item container world
    | isContainer container =
        Right $ updateLocation (getId container) item world
    | otherwise =
        Left $ "The " <> getName container <> " is not a container."

-- | Helper function to remove duplicates while preserving order
-- nub :: Eq a => [a] -> [a]
-- nub = List.foldr (\x acc -> if x `elem` acc then acc else x : acc) []
