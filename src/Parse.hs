{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Parse text into Scheme values.
module Parse
    ( readManyPrettyErr
    ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty(..))
import Data.Void
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Text.Megaparsec.Char.Lexer as L

import Types
import Util

type Parser = Parsec Void T.Text

-- | Parses text into a list of 'SVal's.
readManyPrettyErr
    :: String                -- ^ The file name
    -> T.Text                -- ^ The source text
    -> Either String [SVal]  -- ^ The result, with errors pretty formatted
readManyPrettyErr = first errorBundlePretty .: runParser (spaceConsumer *> many pVal <* eof)

-------------------
-- Simple parsers

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: T.Text -> Parser T.Text
symbol = L.symbol spaceConsumer

ident :: Parser T.Text
ident = T.pack <$> ((:) <$> initial <*> many subsequent) <|> peculiar
  where
    initial           = letterChar <|> specialInitial
    specialInitial    = oneOf ("!$%&*/:<=>?^_~" :: String)
    subsequent        = initial <|> digitChar <|> specialSubsequent
    specialSubsequent = oneOf ("+-.@" :: String)
    peculiar          = T.singleton <$> oneOf ("+-" :: String)

-----------------
-- SVal parsers

pVal :: Parser SVal
pVal = lexeme $ choice
    [ pBool
    , pNumber
    , pChar
    , pString
    , pSymbol
    , pQuote
    , pVector
    , pList
    ]

pBool :: Parser SVal
pBool = label "boolean" $ Boolean False <$ string "#f" <|> Boolean True <$ string "#t"

pNumber :: Parser SVal
pNumber = label "number" $ Number <$> choice
    [ string "#b" *> signed L.binary
    , string "#o" *> signed L.octal
    , string "#d" *> signed L.decimal
    , string "#x" *> signed L.hexadecimal
    , try (signed L.decimal)
    ]
  where
    signed = L.signed (pure ())

pChar :: Parser SVal
pChar = label "character" $ string "#\\" *> choice
    [ Char ' '  <$ string "space"
    , Char '\n' <$ string "newline"
    , Char      <$> anySingle
    ]

pString :: Parser SVal
pString = label "string" $ between (char '"') (char '"') $ String . T.pack <$> many ch
  where
    ch = anySingleBut '"' >>= \case
        '\\' -> flip fmap anySingle $ \case
            '"'  -> '"'
            '\\' -> '\\'
            't'  -> '\t'  -- not specified by the standard
            'n'  -> '\n'  -- not specified by the standard
            c    -> c     -- not specified by the standard
        c -> pure c

pSymbol :: Parser SVal
pSymbol = label "symbol" $ Symbol . T.toLower <$> ident

pQuote :: Parser SVal
pQuote = label "quote" $ symbol "'" *> ((Symbol "quote" :.) . (:. Nil) <$> pVal)

pVector :: Parser SVal
pVector = label "vector" $ between (symbol "#(") (symbol ")") $ Vector . V.fromList <$> many pVal

pList :: Parser SVal
pList = label "list" $ between (symbol "(") (symbol ")") $ many pVal >>= \case
    []         -> pure Nil
    xs@(x:xs') -> mkImproperList (x :| xs') <$> (symbol "." *> pVal) <|> pure (mkSList xs)
