module Core.Distance (module Core.Distance) where
import           Core.State.Entity
import           Core.State.Operations
import           Core.State.TaggedEntity
import           Data.Maybe

data Distance =
  DistanceSelf |
  DistanceHeld |
  DistanceHeldContained | -- need tests and new function, "put"
  DistanceHere |
  DistanceHereContained | -- need tests
  DistanceOverThere | -- need tests
  DistanceNotHere
  deriving (Show, Eq, Ord)


getDistance :: Entity -> Entity -> Distance
getDistance e1 e2
  | e1 == e2 = DistanceSelf
  | getLocation e1 == getLocation e2 = DistanceHere
  | fromJust (getInventory e1) == getLocation e2 = DistanceHeld
  | isNothing (getInventory e1) = DistanceNotHere
