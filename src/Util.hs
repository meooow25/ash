-- | Miscellaneous utilities.
module Util
    ( traverse1
    , distinct
    , (.:)
    , Either3(..)
    , fst3
    , snd3
    , thd3
    ) where

import Data.Hashable
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.HashSet as S

-- | Like @traverse@, only returns the last result.
traverse1 :: Applicative f => (a -> f b) -> NonEmpty a -> f b
traverse1 f (x :| xs) = go x xs
  where
    go y []      = f y
    go y (y':ys) = f y *> go y' ys

-- | Whether the values in the list are distinct.
distinct :: Hashable a => [a] -> Bool
distinct = (==) <$> S.size . S.fromList <*> length

-- | Composes a 1-arg function after a 2-arg function.
infixr 8 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

-- | Like @Either@, but with three alternatives.
data Either3 a b c = E3A a | E3B b | E3C c

-- | The first element of a 3-tuple.
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

-- | The second element of a 3-tuple.
snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

-- | The third element of a 3-tuple.
thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c
