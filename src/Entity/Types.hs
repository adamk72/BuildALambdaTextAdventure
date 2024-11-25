module Entity.Types (module Entity.Types) where

-- | Represents capacity constraints for containers
data Capacity =
    None                    -- ^ Cannot hold anything
    | Limited Int           -- ^ Can hold up to n items
    | Unlimited              -- ^ No limit on items
    deriving (Show, Eq)
