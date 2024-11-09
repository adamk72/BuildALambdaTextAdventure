module Core.Message.Utils (msg, msg2, msgGameWordError, msgs) where

import           Control.Monad.State  (State)
import           Core.Message.Common  (MessageRenderer (..))
import           Core.State.GameState (GameWorld)
import           Data.Text            (Text, intercalate, unpack)

msg :: MessageRenderer a => a -> State GameWorld Text
msg = return . renderMessage

msgs :: MessageRenderer a => [a] -> Text -> State GameWorld Text
msgs messages separator = return $ intercalate separator (map renderMessage messages)

msg2 :: (MessageRenderer a, MessageRenderer b) => a -> b -> State GameWorld Text
msg2 m1 m2 = return $ renderMessage m1 <> " " <> renderMessage m2

msgGameWordError :: MessageRenderer a => a -> State GameWorld Text
msgGameWordError msgE = error $ unpack $ renderMessage msgE
