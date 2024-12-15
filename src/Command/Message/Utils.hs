module Command.Message.Utils (msg, msg2, msgGameWordError, msgs) where

import           Command.Message.Common (MessageRenderer (..))
import           Core.GameState
import           Data.Text              (Text, intercalate, unpack)

msg :: (MessageRenderer a) => a -> GameStateText
msg = return . renderMessage

msgs :: (MessageRenderer a) => [a] -> Text -> GameStateText
msgs messages separator = return $ intercalate separator (map renderMessage messages)

msg2 :: (MessageRenderer a, MessageRenderer b) => a -> b -> GameStateText
msg2 m1 m2 = return $ renderMessage m1 <> " " <> renderMessage m2

msgGameWordError :: (MessageRenderer a) => a -> GameStateText
msgGameWordError msgE = error $ unpack $ renderMessage msgE
