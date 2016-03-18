{-# LANGUAGE TupleSections, FlexibleInstances, RankNTypes #-}

module Combinator
    ( HtmlM (..)
    , HtmlParent
    , HtmlLeaf
    , HasAttributes
    , (!)
    , toHtml
    , newElem
    , newVoid
    , newAttr
    ) where

import ParseHtml
import RenderHtml
import Html

import qualified Data.Text as T
import qualified Data.Map as M
import Data.Monoid
import Data.String
import Unsafe.Coerce


newtype HtmlM a = HtmlM { getHtml :: Html } deriving (Eq)

type HtmlParent = forall a b. HtmlM a -> HtmlM b
type HtmlLeaf = forall a. HtmlM a


instance IsString (HtmlM a) where
    fromString = HtmlM . fromString

instance Show (HtmlM a) where
    show = show . getHtml


instance IsString (HtmlM a -> HtmlM b) where
    fromString = insert . HtmlM . fromString

instance Show (HtmlM a -> HtmlM b) where
    show = show . getHtml . ($ mempty)


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
    m >>= f = m >> f (error "HaskML: Unsupported use of bind.")
    (>>) = append


-- | Define an operator for adding attributes to our combinators.
-- This needs to be a typeclass so that it can work with both parent
-- combiantors as well as leaf combinators.

class HasAttributes a where
    (!) :: a -> Attribute -> a
infixl 5 !

instance HasAttributes (HtmlM a) where
    (!) = setAttr

instance HasAttributes (HtmlM a -> HtmlM b) where
    c ! a = flip setAttr a . c 


-- | Typeclass allowing easy conversion of the various combinator types
-- to the basic Html type. Useful for rendering HTML output once one
-- has constructed an HTML template with the combinators.

class ToHtml a where
    toHtml :: a -> Html

instance ToHtml Html where
    toHtml = id

instance ToHtml (HtmlM a) where
    toHtml = getHtml

instance ToHtml (HtmlM a -> HtmlM b) where
    toHtml f = getHtml $ f mempty


-- | Various utility functions for working with HtmlM combinators. 
-- End users should not ordinarily need to use these, but they are used
-- in other parts of the library to 

append :: HtmlM a -> HtmlM b -> HtmlM c
append (HtmlM x) (HtmlM y) = HtmlM $ x <> y

insert :: HtmlM a -> HtmlM b -> HtmlM c
insert (HtmlM (Html (x@(Element _ _ (InnerHtml c)) : xs))) (HtmlM y) =
    HtmlM . Html $ x { content = InnerHtml $ c <> y } : xs

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
