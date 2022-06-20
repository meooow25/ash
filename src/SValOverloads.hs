{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Some overloads to make it easier to write Scheme expressions as valid Haskell code.
module SValOverloads () where

import qualified Data.Text as T
import qualified GHC.Exts as Exts

import Types

-- | @fromString@ creates a 'Symbol'.
instance Exts.IsString SVal where
    fromString = Symbol . T.pack

-- | @fromList = 'mkSList'@, @toList@ errors.
instance Exts.IsList SVal where
    type Item SVal = SVal
    fromList = mkSList
    toList   = error "not supported"
