{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Native implementations of standard Scheme procedures.
--
-- The procedures here are those that can only be implemented natively or are simply easier to
-- implement natively. Other standard procedures are written in Scheme, see @lib-scm/lib.scm@.
--
-- The @eval@ procedure is native but defined in the "Run" module.
module Procedures
    ( nativeProcedures
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Data.Bifunctor
import Data.Char
import Data.List.NonEmpty (NonEmpty(..))
import Data.Foldable
import Data.Function
import Data.Text.Lazy (toStrict)
import Data.Void
import System.IO
import System.IO.Error
import qualified Data.HashMap.Strict as M
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Vector as V

import Conv
import Parse
import Types
import Util

-- | An environment with natively implemented standard procedures.
nativeProcedures :: Env
nativeProcedures = M.fromList $ second (Procedure . MkProcedure) <$>
    [ ("equal?", equalq)

    , ("number?",   numberq)
    , ("=",         eq)
    , ("<",         lt)
    , (">",         gt)
    , ("<=",        le)
    , (">=",        ge)
    , ("max",       max')
    , ("min",       min')
    , ("+",         plus)
    , ("*",         mult)
    , ("-",         minus)
    , ("quotient",  quotient)
    , ("remainder", remainder)
    , ("modulo",    modulo)
    , ("gcd",       gcd')
    , ("lcm",       lcm')

    , ("number->string", numberToString)
    , ("string->number", stringToNumber)

    , ("boolean?", booleanq)

    , ("pair?", pairq)
    , ("cons",  cons)
    , ("car",   car)
    , ("cdr",   cdr)
    , ("null?", nullq)
    , ("list?", listq)

    , ("char?",            charq)
    , ("char=?",           charEq)
    , ("char<?",           charLt)
    , ("char>?",           charGt)
    , ("char<=?",          charLe)
    , ("char>=?",          charGe)
    , ("char-ci=?",        charCiEq)
    , ("char-ci<?",        charCiLt)
    , ("char-ci>?",        charCiGt)
    , ("char-ci<=?",       charCiLe)
    , ("char-ci>=?",       charCiGe)
    , ("char-alphabetic?", charAlphabetic)
    , ("char-numeric?",    charNumeric)
    , ("char-whitespace?", charWhitespace)
    , ("char-upper-case?", charUpperCase)
    , ("char-lower-case?", charLowerCase)
    , ("char->integer",    charToInteger)
    , ("integer->char",    integerToChar)
    , ("char-upcase",      charUpcase)
    , ("char-downcase",    charDowncase)

    , ("symbol?",        symbolq)
    , ("symbol->string", symbolToString)
    , ("string->symbol", stringToSymbol)

    , ("string?",       stringq)
    , ("make-string",   makeString)
    , ("string-length", stringLength)
    , ("string-ref",    stringRef)
    , ("string=?",      stringEq)
    , ("string-ci=?",   stringCiEq)
    , ("string<?",      stringLt)
    , ("string>?",      stringGt)
    , ("string<=?",     stringLe)
    , ("string>=?",     stringGe)
    , ("string-ci<?",   stringCiLt)
    , ("string-ci>?",   stringCiGt)
    , ("string-ci<=?",  stringCiLe)
    , ("string-ci>=?",  stringCiGe)
    , ("substring",     substring)
    , ("string-append", stringAppend)
    , ("string->list",  stringToList)
    , ("list->string",  listToString)

    , ("vector?",       vectorq)
    , ("make-vector",   makeVector)
    , ("vector-length", vectorLength)
    , ("vector-ref",    vectorRef)
    , ("vector->list",  vectorToList)
    , ("list->vector",  listToVector)

    , ("procedure?", procedureq)
    , ("apply",      apply)
    , ("force",      force)

    , ("scheme-report-environment", schemeReportEnvironment)
    , ("null-environment",          nullEnvironment)

    , ("call-with-input-file",  callWithInputFile)
    , ("call-with-output-file", callWithOutputFile)
    , ("input-port?",           inputPortq)
    , ("output-port?",          outputPortq)
    , ("current-input-port",    currentInputPort)
    , ("current-output-port",   currentOutputPort)
    , ("open-input-file",       openInputFile)
    , ("open-output-file",      openOutputFile)
    , ("close-input-port",      closeInputPort)
    , ("close-output-port",     closeOutputPort)

    , ("read-char",   readChar)
    , ("eof-object?", eofObjectq)
    , ("char-ready?", charReadyq)

    , ("write",      write)
    , ("display",    display)
    , ("newline",    newline)
    , ("write-char", writeChar)

    , ("read-line",        readLine)
    , ("read-from-string", readFromString)
    ]

type Proc = [SVal] -> IO SVal

convM :: Conv a -> SVal -> IO a
convM = either (throwM . SException . convFailMsg) pure .: runConv

mk0 :: IO SVal -> Proc
mk0 f = convM cNil . mkSList >=> const f

mk1 :: (SVal -> IO SVal) -> Proc
mk1 f = convM cList1 . mkSList >=> f

mk2 :: (SVal -> SVal -> IO SVal) -> Proc
mk2 f = convM cList2 . mkSList >=> uncurry f

mk3 :: (SVal -> SVal -> SVal -> IO SVal) -> Proc
mk3 f = convM cList3 . mkSList >=> \(x, y, z) -> f x y z

mk0or1 :: IO SVal -> (SVal -> IO SVal) -> Proc
mk0or1 f0 f1 = convM ((Nothing <$ cNil) <> (Just <$> cList1)) . mkSList >=> maybe f0 f1

mk1or2 :: (SVal -> IO SVal) -> (SVal -> SVal -> IO SVal) -> Proc
mk1or2 f1 f2 = convM ((Left <$> cList1) <> (Right <$> cList2)) . mkSList >=> either f1 (uncurry f2)

mk1' :: (NonEmpty SVal -> IO SVal) -> Proc
mk1' f = convM cNonEmpty . mkSList >=> f

mk2' :: ((SVal, NonEmpty SVal) -> IO SVal) -> Proc
mk2' f = convM cNonEmpty2 . mkSList >=> f

--------------------------------
-- 6.1  Equivalence predicates

equalq :: Proc
equalq = mk2 $ pure . Boolean .: (==)

--------------------------------
-- 6.2.5  Numerical operations

numberq :: Proc
numberq = mk1 $ pure . Boolean . convs cNum

-- The standard defines these as taking >=2 arguments, but it's >=0 here because why not.
eq, lt, gt, le, ge :: Proc
eq = numCompareOp (==)
lt = numCompareOp (<)
gt = numCompareOp (>)
le = numCompareOp (<=)
ge = numCompareOp (>=)

max', min', plus, mult :: Proc
max' = numOp1' maximum
min' = numOp1' minimum
plus = numOp0' sum
mult = numOp0' product

minus :: Proc
minus = numOp1' $ \case
    x :| [] -> -x
    x :| xs -> foldl' (-) x xs

quotient, remainder, modulo :: Proc
quotient  = numOpBin quot
remainder = numOpBin rem
modulo    = numOpBin mod

gcd', lcm' :: Proc
gcd' = numOp0' (foldl' gcd 0)
lcm' = numOp0' (foldl' lcm 1)

numCompareOp :: (Integer -> Integer -> Bool) -> Proc
numCompareOp f = fmap (Boolean . go) . traverse (convM cNum)
  where
    go (x:xs@(y:_)) = f x y && go xs
    go _            = True

numOp0' :: ([Integer] -> Integer) -> Proc
numOp0' f = fmap (Number . f) . traverse (convM cNum)

numOp1' :: (NonEmpty Integer -> Integer) -> Proc
numOp1' f = mk1' $ fmap (Number . f) . traverse (convM cNum)

numOpBin :: (Integer -> Integer -> Integer) -> Proc
numOpBin f = mk2 $ \x y -> Number .: f <$> convM cNum x <*> convM cNum y

--------------------------------------
-- 6.2.6  Numerical input and output

numberToString :: Proc
numberToString = mk1or2 f1 f2
  where
    f1 z       = join $ go <$> convM cNum z <*> pure 10
    f2 z radix = join $ go <$> convM cNum z <*> convM cNum radix
    go :: Integer -> Integer -> IO SVal
    go z = \case
        2  -> pure $ mk $ intToText 2 z
        8  -> pure $ mk $ intToText 8 z
        10 -> pure $ mk $ intToText 10 z
        16 -> pure $ mk $ intToText 16 z
        _  -> throwM $ SException "bad radix"
    mk = String . toStrict . B.toLazyText
    intToText r n
        | n > 0     = goPos n
        | n < 0     = B.singleton '-' <> goPos (-n)
        | otherwise = B.singleton '0'
      where
        goPos :: Integer -> B.Builder
        goPos 0 = mempty
        goPos x = let (x', y) = quotRem x r
                  in goPos x' <> B.singleton (intToDigit $ fromIntegral y)

stringToNumber :: Proc
stringToNumber = mk1or2 f1 f2
  where
    f1 string       = join $ go <$> convM cString string <*> pure 10
    f2 string radix = join $ go <$> convM cString string <*> convM cNum radix
    go :: T.Text -> Integer -> IO SVal
    go s r = do
        defp <- case r of
            2  -> pure L.binary
            8  -> pure L.octal
            10 -> pure L.decimal
            16 -> pure L.hexadecimal
            _  -> throwM $ SException "bad radix"
        let p :: P.Parsec Void T.Text Integer
            p = P.choice
                [ P.string "#b" *> signed L.binary
                , P.string "#o" *> signed L.octal
                , P.string "#d" *> signed L.decimal
                , P.string "#x" *> signed L.hexadecimal
                , signed defp
                ]
        maybe (throwM $ SException "bad number") (pure . Number) $ P.parseMaybe p s
    signed = L.signed (pure ())

--------------------
-- 6.3.1  Booleans

booleanq :: Proc
booleanq = mk1 $ pure . Boolean . \case Boolean _ -> True; _ -> False

---------------------------
-- 6.3.2  Pairs and lists

pairq :: Proc
pairq = mk1 $ pure . Boolean . convs cPair

cons, car, cdr :: Proc
cons = mk2 $ pure .: Pair
car  = mk1 $ fmap fst . convM cPair
cdr  = mk1 $ fmap snd . convM cPair

nullq, listq :: Proc
nullq = mk1 $ pure . Boolean . convs cNil
listq = mk1 $ pure . Boolean . convs cList

-------------------
-- 6.3.3  Symbols

symbolq :: Proc
symbolq = mk1 $ pure . Boolean . convs cSymbol

symbolToString, stringToSymbol :: Proc
symbolToString = mk1 $ fmap String . convM cSymbol
stringToSymbol = mk1 $ fmap Symbol . convM cString

----------------------
-- 6.3.4  Characters

charq :: Proc
charq = mk1 $ pure . Boolean . convs cChar

charEq, charLt, charGt, charLe, charGe :: Proc
charEq = charCompareOp (==)
charLt = charCompareOp (<)
charGt = charCompareOp (>)
charLe = charCompareOp (<=)
charGe = charCompareOp (>=)

charCiEq, charCiLt, charCiGt, charCiLe, charCiGe :: Proc
charCiEq = charCompareOp ((==) `on` toLower)
charCiLt = charCompareOp ((<)  `on` toLower)
charCiGt = charCompareOp ((>)  `on` toLower)
charCiLe = charCompareOp ((<=) `on` toLower)
charCiGe = charCompareOp ((>=) `on` toLower)

charCompareOp :: (Char -> Char -> Bool) -> Proc
charCompareOp f = mk2 (liftA2 (Boolean .: f) `on` convM cChar)

charAlphabetic, charNumeric, charWhitespace, charUpperCase, charLowerCase :: Proc
charAlphabetic = charPredicate isAlpha
charNumeric    = charPredicate isNumber
charWhitespace = charPredicate isSpace
charUpperCase  = charPredicate isUpper
charLowerCase  = charPredicate isLower

charPredicate :: (Char -> Bool) -> Proc
charPredicate f = mk1 $ fmap (Boolean . f) . convM cChar

charToInteger, integerToChar :: Proc
charToInteger = mk1 $ fmap (Number . fromIntegral . ord) . convM cChar
integerToChar = mk1 $ fmap (Char . chr . fromIntegral) . convM cNum

charUpcase, charDowncase :: Proc
charUpcase   = mk1 $ fmap (Char . toUpper) . convM cChar
charDowncase = mk1 $ fmap (Char . toLower) . convM cChar

-------------------
-- 6.3.5  Strings

stringq :: Proc
stringq = mk1 $ pure . Boolean . convs cString

makeString :: Proc
makeString = mk2 $ \k char ->
    String .: T.replicate <$> (fromIntegral <$> convM cNum k) <*> (T.singleton <$> convM cChar char)

stringLength, stringRef :: Proc
stringLength = mk1 $ fmap (Number . fromIntegral . T.length) . convM cString
stringRef = mk2 $
    \s i -> Char .: T.index <$> convM cString s <*> (fromIntegral <$> convM cNum i)

stringEq,   stringLt,   stringGt,   stringLe,   stringGe   :: Proc
stringCiEq, stringCiLt, stringCiGt, stringCiLe, stringCiGe :: Proc
stringEq   = strCompareOp (==)
stringCiEq = strCompareOp ((==) `on` T.toLower)
stringLt   = strCompareOp (<)
stringGt   = strCompareOp (>)
stringLe   = strCompareOp (<=)
stringGe   = strCompareOp (>=)
stringCiLt = strCompareOp ((<)  `on` T.toLower)
stringCiGt = strCompareOp ((>)  `on` T.toLower)
stringCiLe = strCompareOp ((<=) `on` T.toLower)
stringCiGe = strCompareOp ((>=) `on` T.toLower)

strCompareOp :: (T.Text -> T.Text -> Bool) -> Proc
strCompareOp f = mk2 (liftA2 (Boolean .: f) `on` convM cString)

substring :: Proc
substring = mk3 $
    \string start end -> join $ go <$>
        convM cString string <*>
        (fromIntegral <$> convM cNum start) <*>
        (fromIntegral <$> convM cNum end)
  where
    go str s e
        | 0 <= s && s <= e && e <= T.length str = pure $ String $ T.take (e - s) (T.drop s str)
        | otherwise                             = throwM $ SException "bad substring range"

stringAppend :: Proc
stringAppend = fmap (String . T.concat) . traverse (convM cString)

stringToList, listToString :: Proc
stringToList = mk1 $ fmap (mkSList . map Char . T.unpack) . convM cString
listToString = mk1 $ convM cList >=> fmap (String . T.pack) . traverse (convM cChar)

-------------------
-- 6.3.6  Vectors

vectorq :: Proc
vectorq = mk1 $ pure . Boolean . convs cVector

makeVector :: Proc
makeVector = mk1or2 f1 f2
  where
    f1 k   = go Unspec . fromIntegral <$> convM cNum k
    f2 k x = go x . fromIntegral <$> convM cNum k
    go     = Vector .: flip V.replicate

vectorLength :: Proc
vectorLength = mk1 $ fmap (Number . fromIntegral . V.length) . convM cVector

vectorRef :: Proc
vectorRef = mk2 $ \v k -> (V.!) <$> convM cVector v <*> (fromIntegral <$> convM cNum k)

vectorToList, listToVector :: Proc
vectorToList = mk1 $ fmap (foldr Pair Nil) . convM cVector
listToVector = mk1 $ fmap (Vector . V.fromList) . convM cList

-------------------------
-- 6.4 Control features

procedureq :: Proc
procedureq = mk1 $ pure . Boolean . convs cProcedure

apply :: Proc
apply = mk2' $ \(proc', arg :| args) -> join $ convM cProcedure proc' <*> cat arg args
  where
    cat x []      = convM cList x
    cat x (x':xs) = (x:) <$> cat x' xs

force :: Proc
force = mk1 $ convM cPromise >=> unProm

--------------
-- 6.5  Eval

schemeReportEnvironment, nullEnvironment :: Proc
schemeReportEnvironment = envProc ReportEnv5
nullEnvironment         = envProc NullEnv5

envProc :: EnvSpec -> Proc
envProc spec = mk1 $ convM cNum >=> \case
    5 -> pure $ EnvSpec spec
    _ -> throwM $ SException "version must be 5"

-----------------
-- 6.6.1  Ports

callWithInputFile :: Proc
callWithInputFile = mk2 $ \filename proc' -> do
    f <- convM cString filename
    p <- convM cProcedure proc'
    withFile (T.unpack f) ReadMode $ \h -> p [Port (InPort h)]

callWithOutputFile :: Proc
callWithOutputFile = mk2 $ \filename proc' -> do
    f <- convM cString filename
    p <- convM cProcedure proc'
    withFile (T.unpack f) WriteMode $ \h -> p [Port (OutPort h)]

inputPortq, outputPortq :: Proc
inputPortq  = mk1 $ pure . Boolean . convs cInPort
outputPortq = mk1 $ pure . Boolean . convs cOutPort

currentInputPort, currentOutputPort :: Proc
currentInputPort  = mk0 $ pure $ Port (InPort stdin)
currentOutputPort = mk0 $ pure $ Port (OutPort stdout)

openInputFile, openOutputFile :: Proc
openInputFile = mk1 $
    convM cString >=> fmap (Port . InPort) . flip openFile ReadMode . T.unpack
openOutputFile = mk1 $
    convM cString >=> fmap (Port . OutPort) . flip openFile WriteMode . T.unpack

closeInputPort, closeOutputPort :: Proc
closeInputPort  = mk1 $ convM cInPort >=> (Unspec <$) . hClose
closeOutputPort = mk1 $ convM cOutPort >=> (Unspec <$) . hClose

-----------------
-- 6.6.3  Input

readChar :: Proc
readChar = inPort0or1 $ \h -> catchIf isEOFError (Char <$> hGetChar h) (const (pure EOF))

eofObjectq :: Proc
eofObjectq = mk1 $ pure . Boolean . \case EOF -> True; _ -> False

charReadyq :: Proc
charReadyq = inPort0or1 $ \h ->
    catchIf isEOFError (Boolean <$> hReady h) (const (pure $ Boolean True))

inPort0or1 :: (Handle -> IO SVal) -> Proc
inPort0or1 f = mk0or1 (f stdin) (convM cInPort >=> f)

------------------
-- 6.6.3  Output

write, display :: Proc
write   = writeOrDisplay writeVal
display = writeOrDisplay displayVal

writeOrDisplay :: (SVal -> T.Text) -> Proc
writeOrDisplay dispf = mk1or2 f1 f2
  where
    f1 x   = go x stdout
    f2 x p = convM cOutPort p >>= go x
    go x h = Unspec <$ TIO.hPutStr h (dispf x)

newline :: Proc
newline = mk0or1 (go stdout) (convM cOutPort >=> go)
  where
    go h = Unspec <$ hPutStrLn h ""

writeChar :: Proc
writeChar = mk1or2 f1 f2
  where
    f1 c   = join $ go <$> convM cChar c <*> pure stdout
    f2 c p = join $ go <$> convM cChar c <*> convM cOutPort p
    go c h = Unspec <$ hPutChar h c

-----------------
-- Non-standard

-- The standard describes the procedure "read" which parses SVals from a port.
-- This is troublesome to add because we would need an incremental parser, which our parser is not.
-- It might be possible to use lazy IO, but it'll complicate things.
--
-- Instead of "read", the procedures "read-line" and "read-from-string" are provided as substitute,
-- sort of.

-- | Reads a single line of input from a port.
readLine :: Proc
readLine = inPort0or1 $ \h -> catchIf isEOFError (String . T.pack <$> hGetLine h) (const (pure EOF))

-- | Parses a string into a list of Scheme values.
readFromString :: Proc
readFromString = mk1 $
    convM cString >=>
    either (const $ throwM e) (pure . mkSList) . readManyPrettyErr "<string>"
  where
    e = SException "parse failed"
