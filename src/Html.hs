{-# LANGUAGE OverloadedStrings #-}

module Html
    ( Html (..)
    , TagName
    , Attribute
    , Attributes
    , Content (..)
    , Node (..)
    , fromNode
    , emptyElem
    , voidElem
    , htmlSpecialChars
    ) where

import qualified Data.Text as T
import qualified Data.Map as M


-- | A data type to represent chunks of HTML. While HTML can normally
-- be represented as a tree, in this case we might be dealing with 
-- "chunks" of an HTML DOM tree -- for example, the contents of a tag.
-- This means that we may be dealing with several HTML elements that
-- (currently) have no parent. To accomodate this, we define a wrapper
-- around a list of Nodes, and use that as our primary type for HTML trees.
newtype Html = Html { getNodes :: [Node] } deriving (Eq)

type TagName = T.Text
type Attribute = (T.Text, T.Text)
type Attributes = M.Map T.Text T.Text

data Content = Void | InnerHtml Html deriving (Eq)

-- | An individual HTML element, representing a single tag (and its contents),
-- a text node, or a comment.
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


-- | Some utility functions for working with above data types:

fromNode :: Node -> Html
fromNode = Html . return

emptyElem :: TagName -> Node 
emptyElem t = Element t M.empty (InnerHtml mempty)

voidElem :: TagName -> Node
voidElem t = Element t M.empty Void


-- | Escape special characters from a Text string that have special
-- significance in HTML. Behaves similar to the htmlspecialchars()
-- function in PHP. Placed here so it is broadly available where needed.
htmlSpecialChars :: T.Text -> T.Text
htmlSpecialChars = T.replace "<"  "&lt;"
                 . T.replace ">"  "$gt;"
                 . T.replace "\"" "&quot;"
                 . T.replace "'"  "&#039;"
                 . T.replace "&"  "&amp;"