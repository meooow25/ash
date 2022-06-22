{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}

-- | All significant data types used throughout are defined here.
module Types
    ( -- * Scheme values
      SVal(..)
    , pattern (:.)
    , Port(..)
    , Procedure(..)
    , Prom
    , mkProm
    , unProm
    , EnvSpec(..)
      -- * Conversions to string
    , writeVal
    , displayVal
      -- * Constructions of lists
    , mkSList
    , mkImproperList
      -- * AST
    , Expr(..)
    , Body(..)
    , TopLevel(..)
    , Formals(..)
    , VarInit
      -- * Exceptions
    , SException(..)
      -- * The interpreter
    , Env
    , Interp
    , runInterp
    , InterpS
    , runInterpS
    ) where

import Control.Monad.Catch
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.IORef
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text.Lazy (toStrict)
import System.IO
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as B

import Util

------------------
-- Scheme values

-- | Scheme data types.
data SVal
    = Boolean Bool
    | Pair SVal SVal
    | Symbol T.Text
    | Number Integer
    | Char Char
    | String T.Text
    | Vector (V.Vector SVal)
    | Port Port
    | Procedure Procedure
    | Nil
    | Promise Prom
    | EnvSpec EnvSpec
    | EOF
    | Unspec
    deriving Show

-- | Synonym for 'Pair'.
infixr 5 :.
pattern (:.) :: SVal -> SVal -> SVal
pattern l :. r = Pair l r

-- | A Scheme port.
data Port
    = InPort Handle
    | OutPort Handle
    deriving (Eq, Show)

-- | A Scheme procedure.
newtype Procedure = MkProcedure ([SVal] -> IO SVal)

instance Show Procedure where
    show _ = "MkProcedure <procedure>"

-- | A Scheme promise.
newtype Prom = Prom
    { unProm :: IO SVal  -- ^ Returns an action which when executed forces the promise.
    }

-- | Takes an IO action and creates a Scheme promise. The promise is an IO action that memoizes the
-- given IO action when first executed.
mkProm :: IO SVal -> IO Prom
mkProm m = do
    ref <- newIORef Nothing
    pure $ Prom $ readIORef ref >>= \case
        Just res -> pure res
        Nothing -> do
            res <- m
            res <$ writeIORef ref (Just res)

instance Show Prom where
    show _ = "Prom <promise>"

-- | A Scheme environment specifier.
data EnvSpec
    = ReportEnv5  -- ^ Scheme report environment
    | NullEnv5    -- ^ Null environment
    deriving (Eq, Show)

instance Eq SVal where
    Boolean x   == Boolean y   = x == y
    Pair x1 x2  == Pair y1 y2  = x1 == y1 && x2 == y2
    Symbol x    == Symbol y    = x == y
    Number x    == Number y    = x == y
    Char x      == Char y      = x == y
    String x    == String y    = x == y
    Vector x    == Vector y    = x == y
    Port x      == Port y      = x == y
    Procedure _ == Procedure _ = False   -- cannot compare procedures
    Nil         == Nil         = True
    Promise _   == Promise _   = False   -- cannot compare promises
    EnvSpec x   == EnvSpec y   = x == y
    EOF         == EOF         = True
    Unspec      == Unspec      = True
    _           == _           = False

--------------------------
-- Conversions to string

-- | Converts an 'SVal' to its string representation.
writeVal :: SVal -> T.Text
writeVal = toStrict . B.toLazyText . writesVal

writesVal :: SVal -> B.Builder
writesVal = \case
    Boolean b   -> if b then "#t" else "#f"
    Pair x y    -> dispsList writesVal x y
    Symbol s    -> B.fromText s
    Number n    -> B.fromString $ show n
    Char c      -> writesChar c
    String s    -> writesString s
    Vector v    -> dispsVector writesVal v
    Port p      -> writesPort p
    Procedure _ -> "#<procedure>"
    Nil         -> "()"
    Promise _   -> "#<promise>"
    EnvSpec _   -> "#<environment>"
    EOF         -> "#<eof>"
    Unspec      -> "#<unspecified>"

writesChar :: Char -> B.Builder
writesChar = \case
    ' '  -> "#\\space"
    '\n' -> "#\\newline"
    c    -> "#\\" <> B.singleton c

writesString :: T.Text -> B.Builder
writesString = ("\"" <>) . (<> "\"") . T.foldr ((<>) . escape) mempty
  where
    escape = \case
        '"'  -> "\\\""
        '\\' -> "\\\\"
        '\t' -> "\\t"  -- not specified by the standard
        '\n' -> "\\n"  -- not specified by the standard
        c    -> B.singleton c

writesPort :: Port -> B.Builder
writesPort = \case
    InPort _  -> B.fromText "#<input port>"
    OutPort _ -> B.fromText "#<output port>"

-- | Like 'writeVal' but directly shows chars and strings, without quoting or escaping characters.
displayVal :: SVal -> T.Text
displayVal = toStrict . B.toLazyText . displaysVal

displaysVal :: SVal -> B.Builder
displaysVal = \case
    Pair x y -> dispsList displaysVal x y
    Char c   -> B.singleton c
    String s -> B.fromText s
    Vector v -> dispsVector displaysVal v
    x        -> writesVal x  -- for everything else it's the same for write and display

dispsList :: (SVal -> B.Builder) -> SVal -> SVal -> B.Builder
dispsList dispf = ("(" <>) .: go
  where
    go x (y :. z) = dispf x <> " " <> go y z
    go x Nil      = dispf x <> ")"
    go x y        = dispf x <> " . " <> dispf y <> ")"

dispsVector :: (SVal -> B.Builder) -> V.Vector SVal -> B.Builder
dispsVector dispf = ("#(" <>) . (<> ")") . mconcat . intersperse " " . map dispf . V.toList

--------------------------
-- Construction of lists

-- | Creates a Scheme list.
mkSList :: [SVal] -> SVal
mkSList = foldr Pair Nil

-- | Creates an improper Scheme list.
mkImproperList :: NonEmpty SVal -> SVal -> SVal
mkImproperList = flip (foldr Pair)

--------
-- AST

-- | Primitive Scheme expressions.
--
-- @letrec@ and @delay@ when derived depend on assignment, so they are implemented as primitives
-- here.
data Expr
    = Ref T.Text                 -- ^ Variable references
    | Lit SVal                   -- ^ Constants or quoted expressions
    | Call Expr [Expr]           -- ^ Procedure calls
    | Lambda Formals Body        -- ^ Lambdas
    | If Expr Expr (Maybe Expr)  -- ^ Conditionals
    | Letrec [VarInit] Body      -- ^ Recursive bindings
    | Delay Expr                 -- ^ Delayed evaluation
    deriving (Eq, Show)

-- | A @\<body>@, which consists of definitions followed by at least one expression.
data Body = Body [VarInit] (NonEmpty Expr) deriving (Eq, Show)

-- | Same as a @\<body>@ but allows zero expressions after defines. This differs from the spec.
data TopLevel = TopLevel [VarInit] [Expr] deriving (Eq, Show)

-- | The formal arguments of a procedure.
data Formals = Formals
    [T.Text]        -- ^ Required argument names
    (Maybe T.Text)  -- ^ Optional rest argument name
    deriving (Eq, Show)

-- | A definition or @(\<variable> \<init>)@ pair.
type VarInit = (T.Text, Expr)

---------------
-- Exceptions

newtype SException = SException { exMsg :: T.Text } deriving Show

instance Exception SException where
    displayException = T.unpack . exMsg

--------------------
-- The interpreter

-- | A map of names bound to values.
type Env = M.HashMap T.Text SVal

-- | The interpreter monad.
--
-- @Interp@ is strict when putting values into it, so that pure errors are not ignored. All of the
-- following throw when run.
-- 
-- > pure undefined                   :: Interp ()
-- > fmap (const undefined) (pure ()) :: Interp ()
-- > liftIO (pure undefined)          :: Interp ()
newtype Interp a = Interp
    { runInterp :: Env -> IO a  -- ^ Runs the action with the given bindings.
    } deriving (MonadReader Env, MonadThrow, MonadFix) via ReaderT Env IO

instance Functor Interp where
    fmap = liftM

instance Applicative Interp where
    pure  = Interp . const . (pure $!)
    (<*>) = ap

instance Monad Interp where
    m >>= f = Interp $ \env -> runInterp m env >>= flip runInterp env . f

instance MonadIO Interp where
    liftIO = Interp . const . (id <$!>)

-- | The interpreter monad for running 'TopLevel' values.
newtype InterpS a = InterpS
    { unInterpS :: StateT Env IO a
    } deriving
        (Functor, Applicative, Monad, MonadState Env, MonadIO, MonadThrow, MonadCatch, MonadMask)

-- | Runs the action with the given bindings.
runInterpS :: InterpS a -> Env -> IO a
runInterpS = evalStateT . unInterpS
