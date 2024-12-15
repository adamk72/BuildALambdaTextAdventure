module Core.JSON.ScenarioConverter (ScenarioJSON, convertToScenario) where

import           Data.Aeson
import           Data.Text           (Text)
import           Entity.Types.Common
import           Parser.Parser       (parseCmdPhrase, parseCondPhrase, renderExpressionError)
import           Parser.Types        (ParseError)
import           Scenario.Types

data ScenarioJSON = ScenarioJSON
  { sTag             :: Text,
    sName            :: Text,
    sStartConditions :: [ConditionGroupJSON],
    sEndConditions   :: [ConditionGroupJSON],
    sActionsOnceTrue :: Maybe [Text]
  }
  deriving (Show, Eq)

data ConditionGroupJSON = ConditionGroupJSON
  { cgAll        :: Maybe [Text],
    cgAny        :: Maybe [Text],
    cgWhileFalse :: Maybe [ScenarioResponseJSON],
    cgWhileTrue  :: Maybe [ScenarioResponseJSON]
  }
  deriving (Show, Eq)

data ScenarioResponseJSON = ScenarioResponseJSON
  { srActions  :: [Text],
    srResponse :: Text
  }
  deriving (Show, Eq)

instance FromJSON ScenarioJSON where
  parseJSON = withObject "Scenario" $ \v -> do
    tag <- v .: "tag"
    name <- v .: "name"
    startConds <- v .: "startConditions"
    endConds <- v .: "endConditions"
    actions <- v .:? "actionsOnceTrue"
    return $
      ScenarioJSON
        { sTag = tag,
          sName = name,
          sStartConditions = startConds,
          sEndConditions = endConds,
          sActionsOnceTrue = actions
        }

instance FromJSON ConditionGroupJSON where
  parseJSON = withObject "ConditionGroup" $ \v -> do
    allConds <- v .:? "all"
    anyConds <- v .:? "any"
    whileFalse <- v .:? "whileFalse"
    whileTrue <- v .:? "whileTrue"
    return $
      ConditionGroupJSON
        { cgAll = allConds,
          cgAny = anyConds,
          cgWhileFalse = whileFalse,
          cgWhileTrue = whileTrue
        }

instance FromJSON ScenarioResponseJSON where
  parseJSON = withObject "ScenarioResponse" $ \v -> do
    acts <- v .: "actions"
    resp <- v .: "response"
    return $
      ScenarioResponseJSON
        { srActions = acts,
          srResponse = resp
        }

convertParseError :: Either ParseError a -> Either Text a
convertParseError = either (Left . renderExpressionError) Right

convertToScenario :: ScenarioJSON -> Either Text Scenario
convertToScenario ScenarioJSON {..} = do
  startConds <- traverse convertConditionGroup sStartConditions
  endConds <- traverse convertConditionGroup sEndConditions
  actions <- mapM (convertParseError . parseCmdPhrase) `traverse` sActionsOnceTrue

  Right
    Scenario
      { tag = EntityId sTag,
        name = sName,
        startConditions = startConds,
        endConditions = endConds,
        actionsOnceTrue = actions
      }

convertConditionGroup :: ConditionGroupJSON -> Either Text ConditionGroup
convertConditionGroup ConditionGroupJSON {..} = do
  let condType = case (cgAll, cgAny) of
        (Just allConds, Nothing) ->
          traverse (convertParseError . parseCondPhrase) allConds >>= Right . All
        (Nothing, Just anyConds) ->
          traverse (convertParseError . parseCondPhrase) anyConds >>= Right . Any
        _ ->
          Left "Must specify either 'all' or 'any' conditions, but not both"

  whileFalseResps <- traverse convertScenarioResponse `traverse` cgWhileFalse
  whileTrueResps <- traverse convertScenarioResponse `traverse` cgWhileTrue

  conds <- condType
  Right
    ConditionGroup
      { conditions = conds,
        whileFalse = whileFalseResps,
        whileTrue = whileTrueResps
      }

convertScenarioResponse :: ScenarioResponseJSON -> Either Text ScenarioResponse
convertScenarioResponse ScenarioResponseJSON {..} = do
  actions <- traverse (convertParseError . parseCmdPhrase) srActions
  Right
    ScenarioResponse
      { actions = actions,
        response = srResponse
      }
