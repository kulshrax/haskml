{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Attributes where

import qualified Data.Text as T
import Html


src :: T.Text -> Attribute
src = ("src",)

href :: T.Text -> Attribute
href = ("href",)