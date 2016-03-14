{-# LANGUAGE TupleSections, FlexibleInstances, RankNTypes #-}

module Combinator where

import ParseHtml
import RenderHtml
import Html

import qualified Data.Text as T
import qualified Data.Map as M
import Data.Monoid
import Data.String
import Unsafe.Coerce


newtype HtmlM a = HtmlM { getHtml :: Html }

type HtmlParent = forall a b. HtmlM a -> HtmlM b
type HtmlLeaf = forall a. HtmlM a


instance IsString (HtmlM a) where
    fromString = HtmlM . fromString

instance Show (HtmlM a) where
    show = show . getHtml


instance Monoid (HtmlM a) where
    mempty = HtmlM mempty
    mappend = append

instance Functor HtmlM where
    fmap _ = unsafeCoerce  

instance Applicative HtmlM where
    pure = const mempty
    (<*>) = append

instance Monad HtmlM where
    return = pure
    m >>= f = m >> f undefined
    (>>) = append


class HasAttributes a where
    (!) :: a -> Attribute -> a

instance HasAttributes (HtmlM a) where
    (!) = setAttr

instance HasAttributes (HtmlM a -> HtmlM b) where
    c ! a = flip setAttr a . c 


append :: HtmlM a -> HtmlM b -> HtmlM c
append (HtmlM x) (HtmlM y) = HtmlM $ x <> y

newElem :: TagName -> HtmlParent
newElem t c = HtmlM . fromNode $ 
    (emptyElem t) { content = InnerHtml . getHtml $ c }

newVoid :: TagName -> HtmlLeaf
newVoid = HtmlM . fromNode . voidElem

newAttr :: T.Text -> T.Text -> Attribute
newAttr a = (a,)

setAttr :: HtmlM a -> Attribute -> HtmlM b
setAttr (HtmlM h) (k,v) = HtmlM . Html $ n':ns
    where n'     = n { attributes = attr' }
          attr'  = M.insert k v $ attributes n
          (n:ns) = getNodes h
