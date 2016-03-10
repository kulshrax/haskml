module Html where

--import Data.List
--import Data.Char
--import Data.String

import qualified Data.Text as T
import qualified Data.Map as M

newtype Html = Html { getNodes :: [Node] } deriving Show

type Attributes = M.Map T.Text T.Text

data Node = Element { _tagName    :: T.Text
                    , _attributes :: Attributes
                    , _innerHtml  :: Html
                    }
          | Text T.Text
          | Comment T.Text
          deriving Show


instance Monoid Html where
    mempty = Html []
    mappend a b = Html $ getNodes a ++ getNodes b