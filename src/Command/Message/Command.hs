module Command.Message.Command (CommandMessage (..)) where

import           Command.Message.Common (MessageRenderer (..))
import           Data.Text              (Text)

data CommandMessage
    = DontKnowWhere Text
    | DropWhat
    | GetWhat
    | GoWhere
    | NotSure
    | PutWhat
    | PutWhere Text
    deriving (Eq, Show)

instance MessageRenderer CommandMessage where
    renderMessage = \case
        DontKnowWhere item -> "Don't know where to put " <> item <> "."
        DropWhat -> "What needs to be dropped?"
        GetWhat -> "What are you trying to get?"
        GoWhere -> "Where do you want to go?"
        NotSure -> "Not sure how to do that."
        PutWhat -> "What needs to be put?"
        PutWhere item -> "Put " <> item <> " where?"
