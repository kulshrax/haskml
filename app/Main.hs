{-# LANGUAGE OverloadedStrings, RankNTypes #-}

{- A simple example program showcasing usage of HTML combinators. -}


module Main where

import qualified Tags as H
import qualified Attributes as A
import RenderHtml
import Combinator

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Monoid


my_name     = "Joe Bloggs"
my_content  = "My <i>amazing</i> page."
my_url      = "haskell.org"
my_items    = ["Thing 1", "Thing 2", "Thing <b>3</b>"]


template name content url items = 
    let title = name <> "'s Page" in
        H.html $ do
            H.head $ do
                H.meta ! A.charset "UTF-8"
                H.title title
            H.body $ do
                "<h1></h1>" $ title
                H.div $ do
                    "<a>A link.</a>" ! A.href url
                    H.ul $ mapM H.li items
                    "<p>Content appears below:\n</p>" $ do
                        text content


main :: IO ()
main = TIO.putStrLn . renderPage . toHtml 
    $ template my_name my_content my_url my_items
