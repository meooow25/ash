{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

-- | Convert from Scheme values.
module Conv
    ( Conv
    , ConvFail(..)
    , runConv
    , convs
      -- * Conversions
    , asBool
    , cPair
    , cNum
    , cSymbol
    , cChar
    , cString
    , cVector
    , cInPort
    , cOutPort
    , cProcedure
    , cNil
    , cPromise
    , cEnvSpec
    , cList
    , cNonEmpty
    , cNonEmpty2
    , cImproperList
    , Count(..)
    , cListWithLen
    , cList1
    , cList2
    , cList3
    ) where

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.Maybe
import System.IO
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Vector as V

import Types
import Util

-- | A converter from 'SVal' to @a@.
data Conv a = Conv
    { getExpected :: Expected   -- ^ Expected types
    , run :: SVal -> Maybe a
    } deriving Functor

-- | Succeeds if either converter succeeds.
instance Semigroup (Conv a) where
    a <> b = Conv (getExpected a <> getExpected b) ((<|>) <$> run a <*> run b)

type Expected = NonEmpty T.Text

expect :: T.Text -> Expected
expect = (:| [])

-- | A conversion failure.
newtype ConvFail = ConvFail { convFailMsg :: T.Text }

-- | Runs the converter on a value.
runConv :: Conv a -> SVal -> Either ConvFail a
runConv p v = maybe (Left $ ConvFail msg) Right $ run p v
  where
    msg = T.concat $
        ["expected "] ++ NE.toList (NE.intersperse " or " (getExpected p)) ++ [", got ", writeVal v]

-- | Converts and throws away the result, returning only whether the conversion succeeded.
convs :: Conv a -> SVal -> Bool
convs = isJust .: run

----------------
-- Conversions

asBool :: SVal -> Bool
asBool = \case Boolean False -> False; _ -> True

cPair :: Conv (SVal, SVal)
cPair = Conv (expect "pair") $ \case Pair x y -> Just (x, y); _ -> Nothing

cNum :: Conv Integer
cNum = Conv (expect "number") $ \case Number x -> Just x; _ -> Nothing

cSymbol :: Conv T.Text
cSymbol = Conv (expect "symbol") $ \case Symbol x -> Just x; _ -> Nothing

cChar :: Conv Char
cChar = Conv (expect "char") $ \case Char x -> Just x; _ -> Nothing

cString :: Conv T.Text
cString = Conv (expect "string") $ \case String x -> Just x; _ -> Nothing

cVector :: Conv (V.Vector SVal)
cVector = Conv (expect "vector") $ \case Vector v -> Just v; _ -> Nothing

cInPort :: Conv Handle
cInPort = Conv (expect "input port") $ \case Port (InPort h) -> Just h; _ -> Nothing

cOutPort :: Conv Handle
cOutPort = Conv (expect "output port") $ \case Port (OutPort h) -> Just h; _ -> Nothing

cProcedure :: Conv ([SVal] -> IO SVal)
cProcedure = Conv (expect "procedure") $ \case Procedure (MkProcedure f) -> Just f; _ -> Nothing

cPromise :: Conv Prom
cPromise = Conv (expect "promise") $ \case Promise p -> Just p; _ -> Nothing

cEnvSpec :: Conv EnvSpec
cEnvSpec = Conv (expect "environment specifier") $ \case EnvSpec s -> Just s; _ -> Nothing

cList :: Conv [SVal]
cList = Conv (expect "list") $ \case
    Nil     -> Just []
    a :. xs -> (a:) <$> run cList xs
    _       -> Nothing

cNonEmpty :: Conv (NonEmpty SVal)
cNonEmpty = Conv (expect "non-empty list") $ \case
    a :. xs -> (a :|) <$> run cList xs
    _       -> Nothing

cNonEmpty2 :: Conv (SVal, NonEmpty SVal)
cNonEmpty2 = Conv (expectCount (AtLeast 2)) $ \case
    a :. b :. xs -> (a,) . (b :|) <$> run cList xs
    _            -> Nothing

cImproperList :: Conv (NonEmpty SVal, SVal)
cImproperList = Conv (expect "improper list") $ \case
    x :. xs -> go x xs
    _       -> Nothing
  where
    go _ Nil        = Nothing
    go x (h :. t)   = first (x <|) <$> go h t
    go x y          = Just (x :| [], y)


data Count = Exactly Int | AtLeast Int

expectCount :: Count -> Expected
expectCount = expect . ("list of length " <>) . \case
    Exactly n -> T.pack (show n)
    AtLeast n -> "at least " <> T.pack (show n)

checkLen :: Count -> [a] -> Maybe [a]
checkLen c xs = case c of
    Exactly n | n' == n -> Just xs
    AtLeast n | n' >= n -> Just xs
    _                   -> Nothing
  where
    n' = length xs

cNil :: Conv [SVal]
cNil = Conv (expect "nil") $ \case Nil -> Just []; _ -> Nothing

cListWithLen :: Count -> Conv [SVal]
cListWithLen (Exactly 0) = cNil
cListWithLen c           = Conv (expectCount c) $ run cList >=> checkLen c

cList1 :: Conv SVal
cList1 = Conv (expectCount (Exactly 1)) $ \case
    a :. Nil -> Just a
    _        -> Nothing

cList2 :: Conv (SVal, SVal)
cList2 = Conv (expectCount (Exactly 2)) $ \case
    a :. b :. Nil -> Just (a, b)
    _             -> Nothing

cList3 :: Conv (SVal, SVal, SVal)
cList3 = Conv (expectCount (Exactly 3)) $ \case
    a :. b :. c :. Nil -> Just (a, b, c)
    _                  -> Nothing
