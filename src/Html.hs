module Html where

import qualified Data.Text as T
import qualified Data.Map as M

newtype Html = Html { getNodes :: [Node] }

type Attributes = M.Map T.Text T.Text

data Content = InnerHtml Html
             | Void

data Node = Element { tag    :: T.Text
                    , attributes :: Attributes
                    , content :: Content
                    }
          | Text T.Text
          | Comment T.Text


instance Monoid Html where
    mempty = Html []
    mappend a b = Html $ getNodes a ++ getNodes b