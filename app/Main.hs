{-# LANGUAGE OverloadedStrings, RankNTypes #-}

{- A simple example program showcasing usage of HTML combinators. -}


module Main where

import Tags as H
import Attributes as A
import RenderHtml
import Combinator

import Data.Text as T
import Data.Text.IO as TIO
import Data.Monoid

template = H.html $ do
    H.head $ do
        H.meta ! A.charset "utf-8"
        H.title $ "'s Page"
    H.body $ do
        "<a href=\"foo\">Test</a>" ! A.href "bar" $ do
            text "foobar"


main :: IO ()
main = TIO.putStrLn . renderPage . toHtml $ template
