{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module Core.Operations
    ( module Core.Ops.Inventory
    , module Core.Ops.Location
    , module Core.Ops.EntityInfo
    ) where

import           Core.Ops.EntityInfo
import           Core.Ops.Inventory
import           Core.Ops.Location

-- | Helper function to remove duplicates while preserving order
-- nub :: Eq a => [a] -> [a]
-- nub = List.foldr (\x acc -> if x `elem` acc then acc else x : acc) []
