{-# LANGUAGE TupleSections #-}

module Combinator where

import Prelude hiding (span)

import qualified Data.Text as T
import qualified Data.Map as M
import Data.Monoid
import Data.String
import Unsafe.Coerce

import ParseHtml
import RenderHtml
import Html


newtype HtmlM a = HtmlM { getHtml :: Html }
type HtmlC = HtmlM ()

append :: HtmlM a -> HtmlM b -> HtmlM c
append (HtmlM x) (HtmlM y) = HtmlM $ x <> y

instance IsString (HtmlM a) where
    fromString = HtmlM . fromString

instance Show (HtmlM a) where
    show = show . getHtml

instance Monoid (HtmlM a) where
    mempty  = HtmlM mempty
    mappend = append

instance Functor HtmlM where
    fmap _ = unsafeCoerce  

instance Applicative HtmlM where
    pure  = const mempty
    (<*>) = append

instance Monad HtmlM where
    return = pure
    m >>= f  = m >> f undefined
    (>>)     = append


customElem :: TagName -> HtmlC -> HtmlC
customElem t c = HtmlM . fromNode $ 
                    (emptyElem t) { content = InnerHtml . getHtml $ c }

customVoid :: TagName -> HtmlC
customVoid t = customElem t mempty

customAttr :: T.Text -> T.Text -> Attribute
customAttr a = (a,)

setAttr :: HtmlC -> Attribute -> HtmlC
setAttr (HtmlM h) (k,v) = HtmlM . Html $ n':ns
                      where n'     = n { attributes = attr' }
                            attr'  = M.insert k v $ attributes n
                            (n:ns) = getNodes h

(!) :: HtmlC -> Attribute -> HtmlC
(!) = setAttr
infixl 5 !