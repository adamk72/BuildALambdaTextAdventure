module Command.Actor (module Command.Actor) where

import           Command.CommandExecutor
import           Core.GameMonad
import           Core.State.Operations
import           Entity.Entity
import           Utils

-- Todo: This can call the Look command with a Binary Expression instead ("look inventory").
executeInventory :: CommandExecutor
executeInventory _ = undefined

{- OLD WAY
executeInventory :: CommandExecutor
executeInventory _ = do
  gw <- getWorld
  return $ "Your inventory is: " <> getInventory gw
    where
      getInventory gw =
        case getActorInventory gw of
          Left _ -> "empty"
          Right eId -> do
            let invIdM = findEntityById eId
            case findEntityById invIdM gw of
                Just invM -> case invM of
                  AnyActor inv -> oxfordEntityNames (getContainerContents inv gw)
                  _ -> "empty"
                Nothing -> "empty"
-}
