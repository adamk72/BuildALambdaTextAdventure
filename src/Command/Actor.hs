module Command.Actor (module Command.Actor) where
import           Command.CommandExecutor
import           Core.GameMonad
import           Utils
import Core.State.Operations
import Entity.Entity

-- Todo: This can call the Look command with a Binary Expression instead ("look inventory").
executeInventory :: CommandExecutor
executeInventory _ = do
  gw <- getWorld
  case getActorInventory gw of
    Left _ -> pure "Your inventory is empty."
    Right eId -> do
      let invIdM = findEntityById eId
      case findEntityByTag invIdM gw of
        Just invM -> case invM of
          AnyActor inv -> pure $ "Your inventory is: " <> oxfordEntityNames (getContainerContents inv gw)
          _ -> pure "Your inventory is empty."
        Nothing -> pure "Your inventory is empty."

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
            case findEntityByTag invIdM gw of
                Just invM -> case invM of
                  AnyActor inv -> oxfordEntityNames (getContainerContents inv gw)
                  _ -> "empty"
                Nothing -> "empty"
-}