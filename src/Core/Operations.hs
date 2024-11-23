{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module Core.Operations
    ( module Entity.Ops.Inventory
    , module Entity.Ops.Location
    ) where

import           Entity.Ops.Inventory
import           Entity.Ops.Location

-- | Helper function to remove duplicates while preserving order
-- nub :: Eq a => [a] -> [a]
-- nub = List.foldr (\x acc -> if x `elem` acc then acc else x : acc) []
