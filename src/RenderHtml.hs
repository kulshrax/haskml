{-# LANGUAGE OverloadedStrings #-}

module RenderHtml (render) where

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B
import Data.Monoid

import Html
import ParseHtml

instance Show Html where
    show = T.unpack . render


render :: Html -> T.Text
render = L.toStrict . B.toLazyText . renderHtml

renderHtml :: Html -> B.Builder
renderHtml = mconcat . fmap renderNode . getNodes

renderNode :: Node -> B.Builder
renderNode (Text t) = B.fromText t
renderNode (Comment c ) = B.fromText "<!--" <> B.fromText c <> B.fromText "-->"
renderNode elem = renderElem elem

renderElem :: Node -> B.Builder
renderElem e@(Element t _ c) = 
    case c of 
        Void -> renderStartTag e
        InnerHtml h -> renderStartTag e <> renderHtml h <> renderEndTag t

renderStartTag :: Node -> B.Builder
renderStartTag (Element t a _) = B.singleton '<' 
                               <> B.fromText t 
                               <> renderAttrList a 
                               <> B.singleton '>'

renderAttrList :: Attributes -> B.Builder
renderAttrList = M.foldrWithKey renderAttr mempty

renderAttr :: T.Text -> T.Text -> B.Builder -> B.Builder
renderAttr k "" acc = acc <> B.singleton ' ' <> B.fromText k
renderAttr k v  acc = acc <> B.singleton ' ' <> B.fromText k
                          <> B.fromText "=\"" <> B.fromText v 
                          <> B.singleton '"'

renderEndTag :: T.Text -> B.Builder
renderEndTag t = B.fromText "</" <> B.fromText t <> B.singleton '>'
