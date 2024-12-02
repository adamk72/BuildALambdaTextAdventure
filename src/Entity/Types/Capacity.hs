module Entity.Types.Capacity (Capacity (..)) where

data Capacity =
    None
    | Limited Int
    | Unlimited
    deriving (Show, Eq)
