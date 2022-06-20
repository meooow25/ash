{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module TestUtil where

import qualified Data.Text as T
import qualified GHC.Exts as Exts

import SValOverloads ()
import Types

mkB :: Bool -> SVal
mkB = Boolean

q :: SVal -> SVal
q e = Symbol "quote" :. e :. Nil

instance Num SVal where
    fromInteger = Number
    negate (Number x) = Number (-x)
    negate _          = error "not supported"
    (+)    = error "not supported"
    (*)    = error "not supported"
    abs    = error "not supported"
    signum = error "not supported"
