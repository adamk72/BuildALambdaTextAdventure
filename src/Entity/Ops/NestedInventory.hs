{-# LANGUAGE DataKinds #-}
module Entity.Ops.NestedInventory (
    getNestedInventoryList,
    formatNestedInventory,
    InventoryPath
) where

import Data.Text (Text)
import qualified Data.Text as T
import Entity.Entity
import Entity.Class.EntityBase
import Entity.Class.Capacity
import Utils (oxfordComma)

-- | Represents the path to an item through containers
-- e.g. "bag > pouch > coin" would show how to reach the coin
type InventoryPath = [Text]

getNestedInventoryList :: HasEntityBase a => Entity a -> World -> [(Entity 'ItemT, InventoryPath)]
getNestedInventoryList container world =
    let directItems = getItemList (getId container) world
    in concatMap (getNestedItem [getName container]) directItems
  where
    getNestedItem :: InventoryPath -> Entity 'ItemT -> [(Entity 'ItemT, InventoryPath)]
    getNestedItem path item =
        let itemPath = path ++ [getName item]
            -- Get items in this container (if it is one)
            nestedItems = case itemCapacity item of
                Just _ -> getItemList (getId item) world
                Nothing -> []
            -- Recursively get items in sub-containers
            nestedResults = concatMap (getNestedItem itemPath) nestedItems
        in (item, itemPath) : nestedResults

formatNestedInventory :: [(Entity 'ItemT, InventoryPath)] -> Text
formatNestedInventory [] = "nothing"
formatNestedInventory items =
    let formatItem (item, path) = getName item <>
            if length path <= 2
                then ""
                else " (in " <> T.intercalate " > " (tail . init $ path) <> ")"
    in oxfordComma (map formatItem items)