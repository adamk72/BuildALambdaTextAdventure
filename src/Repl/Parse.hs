module Repl.Parse (parse) where
-- import           Command.Go   (executeGo)
import           Command.Go          (executeGo)
import           Command.Look        (executeLook)
import           Control.Monad.State
import           Core.State          (GameWorld)
import           Data.Text           (Text, isPrefixOf, strip, stripPrefix,
                                      toLower)

parse :: Text -> State GameWorld Text
parse input = do
  let lowerCase = toLower input
  case lowerCase of
    _ | "look" `isPrefixOf` lowerCase -> executeLook (strip <$> stripPrefix "look" input)
    _ | "go" `isPrefixOf` lowerCase -> executeGo(strip <$> stripPrefix "go" input)
    _ -> return $ "Don't know how to " <> lowerCase <> "."
