{-# LANGUAGE OverloadedStrings #-}

module Selectors where

-- This module is currently incomplete. The intention was to create a module
-- for traversing HTML DOM trees and querying them using CSS-like syntax.
-- So far, there are some basic zippers for traversing Html structures,
-- but the CSS selectors are not yet implemeneted.

import Html
import Combinator

import Data.Text as T
import Data.Map as M
import Data.List as L


data HtmlHole = HH { parentInfo :: (TagName, Attributes)
                   , before :: [Node]
                   , after :: [Node]
                   } deriving (Show)

data HtmlZipper = HZ { focused :: Node
                     , holes :: [HtmlHole]
                     } deriving (Show)

data Selector = TagName T.Text
              | Attributes Attributes
              | And Selector Selector 
              | Or Selector Selector
              | DescOf Selector Selector
              | ChildOf Selector Selector
              | NthChild Int
              | Wildcard
              deriving (Show)



toZipper :: Node -> HtmlZipper
toZipper = flip HZ []

fromZipper :: HtmlZipper -> Node
fromZipper = focused . root


view :: HtmlZipper -> Node
view (HZ n _) = n

over :: (Node -> Node) -> HtmlZipper -> HtmlZipper
over f (HZ n hs) = HZ (f n) hs


makeParent :: Node -> HtmlHole -> Node
makeParent n (HH (t,a) xs ys) = Element t a (InnerHtml . Html $ xs ++ n:ys)

parent :: HtmlZipper -> Maybe HtmlZipper
parent (HZ _ []) = Nothing
parent (HZ n (h:hs)) = Just $ HZ (makeParent n h) hs

root :: HtmlZipper -> HtmlZipper
root z = case parent z of
            Just p  -> root p
            Nothing -> z

nth :: Int -> HtmlZipper -> HtmlZipper
nth i (HZ (Element t a (InnerHtml (Html c))) hs) = HZ n (h:hs) 
    where h = HH (t,a) before after
          (before, n:after) = L.splitAt i c

first :: HtmlZipper -> HtmlZipper
first = nth 0

prev :: HtmlZipper -> Maybe HtmlZipper
prev (HZ _ ((HH _ [] _):_)) = Nothing
prev (HZ n ((HH p bs as):hs)) = Just $ 
    HZ (L.last bs) ((HH p (L.init bs) (n:as)):hs)

next :: HtmlZipper -> Maybe HtmlZipper
next (HZ _ ((HH _ _ []):_)) = Nothing
next (HZ n ((HH p b (a:as)):hs)) = Just $ HZ a ((HH p (b ++ [n]) as):hs)


select :: Selector -> Html -> HtmlZipper
select = undefined

isSubset :: Ord k => M.Map k a -> M.Map k a -> Bool
isSubset sub = M.null . M.difference sub
