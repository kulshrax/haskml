module Html where

import qualified Data.Text as T
import qualified Data.Map as M


newtype Html = Html { getNodes :: [Node] } deriving (Eq)

type TagName = T.Text
type Attribute = (T.Text, T.Text)
type Attributes = M.Map T.Text T.Text

data Content = Void | InnerHtml Html deriving (Eq)
data Node = Element { tag    :: TagName
                    , attributes :: Attributes
                    , content :: Content
                    }
          | Text T.Text
          | Comment T.Text
          deriving (Eq)


instance Monoid Html where
    mempty = Html []
    mappend a b = Html $ getNodes a ++ getNodes b


fromNode :: Node -> Html
fromNode = Html . return

emptyElem :: TagName -> Node 
emptyElem t = Element t M.empty (InnerHtml mempty)

voidElem :: TagName -> Node
voidElem t = Element t M.empty Void