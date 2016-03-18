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
    , text
    ) where

import ParseHtml
import RenderHtml
import Html

import qualified Data.Text as T
import qualified Data.Map as M
import Data.Monoid
import Data.String
import Unsafe.Coerce


-- | Monadic wrapper for Html objects, allowing the use of do notation
-- to concatenate them. In order to be a monad, this type has kind * -> *,
-- but the extra type parameter doesn't actually do anything. 
-- 
-- This type is mostly an abuse of the type system in order to get nice
-- syntactic sugar. It does not obey the monad laws, for example, and
-- should not be treated like a proper monad for most purposes.
--
-- This syntax hack was heavily inspired by BlazeHTML, which does something
-- very similar to a clean-looking API.

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


-- | An abuse of the type system. We need an instance of Function in order
-- to have an instance of Monad. We can implement fmap using unsafeCoerce 
-- since the HtmlM object doesn't actually contain anything, and the type
-- parameter is essentially meaningless anyway.
--
-- This was originally implemented as undefined, but unsafeCoerce will at 
-- least not crash the program if an fmap happens.
instance Functor HtmlM where
    fmap _ = unsafeCoerce  

instance Applicative HtmlM where
    pure = const mempty
    (<*>) = append

-- Again, abuse of the type system. Note that monadic binds only work
-- if the function passed to >>= does not attempt to evaulate its argument.
-- Doing that (or the corresponding <- notation in a do block) will result
-- in a runtime error. This behavior is similar to what BlazeHTML does in 
-- practice.
instance Monad HtmlM where
    return = pure
    m >>= f = m >> f (error "HaskML: Unsupported use of bind.")
    (>>) = append


-- | Define an operator for adding attributes to our combinators.
-- This needs to be a typeclass so that it can work with both parent
-- combiantors as well as leaf combinators.

class HasAttributes a where
    (!) :: a -> Attribute -> a

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

text :: T.Text -> HtmlM a
text = HtmlM . textToHtml
