module P0 where

import Data.Char (isDigit, isAsciiLower, isAsciiUpper, ord, chr)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import Control.Applicative
import Data.Void (Void)

import L0
import Shared

-- | l0p Process l0-depend data
l0p :: (a -> Sexp b) -> Sexp a -> Sexp b
l0p f (EsLiteral l)                          = f                              l
l0p f (EsList xs)                            = EsList          (map (l0p f) xs)
l0p f (EsSeq xs)                             = EsSeq           (map (l0p f) xs)
l0p f (EsLoc i x)                            = EsLoc                i (l0p f x)
l0p _ (EsErr e)                              = EsErr                          e

l0Tol0p :: L0 -> L0p
l0Tol0p = l0p parse

parse :: Lexeme -> Sexp Literal
parse (LaSymbol                        name) = EsLiteral (LiSymbol        name)
parse (LaSyntaxSym                     name) = EsLiteral (LiSyntaxSym     name)
parse (LaDecimal      sign                i) = int                    10 sign i
parse (LaOctal        sign                i) = int                    8  sign i
parse (LaHexadecimal  sign                i) = int                    16 sign i
parse (LaBinary       sign                i) = int                    2  sign i
parse (LaFloat        sign     integer rest) = float    sign       integer rest
parse (LaRational     sign divisor dividend) = rational sign   divisor dividend
parse (LaStandard     sign     base se expo) = standard sign       base se expo
parse (LaBoolean                          b) = case b of
  "#true"                                   -> EsLiteral (LiBoolean       True)
  "#false"                                  -> EsLiteral (LiBoolean      False)
  _                                         -> EsErr EeInvalidBoolean
parse (LaCharacter                       ch) = str2char                      ch
parse (LaEscaped                       name) = escape2char                 name
parse (LaUnicode                       code) = unicode2char                code
parse (LaString                         str) = strstr str
parse (LaRawString                      str) = EsLiteral (LiString         str)

int :: Integer -> Maybe Char -> String -> Sexp Literal
int radix sgn s = case str2int radix '\'' s of
  Nothing -> EsErr EeParseError
  Just i  -> sign sgn i
  where
    sign (Just '+') v                        = EsLiteral (LiInteger v)
    sign (Just '-') v                        = EsLiteral (LiInteger (-v))
    sign Nothing    v                        = EsLiteral (LiInteger v)
    sign _          _                        = EsErr EeParseError

f3 :: (Integer -> Integer -> String -> String -> Double) -> Maybe Char -> String -> String -> Sexp Literal
f3 f sgn i r = case (str2int 10 '\'' i, str2int 10 '\'' r) of
  (Nothing, _)       -> EsErr EeParseError
  (_, Nothing)       -> EsErr EeParseError
  (Just i', Just r') -> sign sgn (f i' r' i r)
  where
    sign (Just '+') v = EsLiteral (LiReal v)
    sign (Just '-') v = EsLiteral (LiReal (-v))
    sign Nothing    v = EsLiteral (LiReal v)
    sign _   _        = EsErr EeParseError

float :: Maybe Char -> String -> String -> Sexp Literal
float = f3 (\x y _ y' -> fromInteger x + fromInteger y / 10 ^ length (filter (/= '\'') y'))

rational :: Maybe Char -> String -> String -> Sexp Literal
rational = f3 (\x y _ _-> fromInteger x / fromInteger y)

standard :: Maybe Char -> String -> Maybe Char -> String -> Sexp Literal
standard s b se e = case (str2int 10 '\'' b, str2int 10 '\'' e) of
  (Nothing, _)       -> EsErr EeParseError
  (_, Nothing)       -> EsErr EeParseError
  (Just i', Just r') -> case se of
    Just '+'         -> sign s (fromInteger i' * 10 ** fromInteger r')
    Just '-'         -> sign s (fromInteger i' * 10 ** fromInteger (-r'))
    Nothing          -> sign s (fromInteger i' * 10 ** fromInteger r')
    _                -> EsErr EeParseError
  where
    sign (Just '+') v = EsLiteral (LiReal v)
    sign (Just '-') v = EsLiteral (LiReal (-v))
    sign Nothing    v = EsLiteral (LiReal v)
    sign _   _        = EsErr EeParseError

str2int :: Integer -> Char -> String -> Maybe Integer
str2int radix sep s
  | radix <= 0 || radix > 36 = Nothing
  | otherwise                = do
      s' <- validate s
      digits <- traverse (check . digit) s'
      pure $ foldl step 0 digits
  where
    validate :: String -> Maybe String
    validate [] = Nothing
    validate (x:xs)
      | x == sep  = Nothing
      | otherwise = sequence (go False (x:xs))
      where
        go :: Bool -> String -> [Maybe Char]
        go prev (y:ys)
          | y == sep  = if prev || null ys then [Nothing] else go True ys
          | otherwise = Just y : go False ys
        go _ []       = []

    digit :: Char -> Maybe Integer
    digit c
      | isDigit c      = Just (fromIntegral (ord c - ord '0'))
      | isAsciiLower c = Just (fromIntegral (ord c - ord 'a') + 10)
      | isAsciiUpper c = Just (fromIntegral (ord c - ord 'A') + 10)
      | otherwise      = Nothing

    check :: Maybe Integer -> Maybe Integer
    check Nothing      = Nothing
    check (Just d)     = if d < radix then Just d else Nothing

    step :: Integer -> Integer -> Integer
    step n d           = n * radix + d

type Parser = P.Parsec Void String

strstr :: String -> Sexp Literal
strstr s =
  case P.runParser (P.many (unicode <|> regular <|> plain) <* P.eof) "" s of
    Left  _  -> EsErr EeInvalidEscape
    Right cs -> maybe (EsErr EeInvalidEscape) (EsLiteral . LiString) (sequence cs)
  where
    plain :: Parser (Maybe Char)
    plain = Just <$> P.satisfy (/= '\\')

    regular :: Parser (Maybe Char)
    regular = flip lookup table <$> (C.string "\\" *> C.asciiChar)
      where
        table =
          [ ('n', '\n')
          , ('r', '\r')
          , ('t', '\t')
          , ('b', '\b')
          , ('f', '\f')
          , ('v', '\v')
          , ('a', '\a')
          , ('\\', '\\')
          , ('\'', '\'')
          , ('"', '"')
          , ('0', '\0')
          ]

    unicode :: Parser (Maybe Char)
    unicode = do
      hex <- C.string "\\u{" *> P.some C.hexDigitChar <* C.char '}'
      return $ case str2int 16 ' ' hex of
        Just i | validate i -> Just (chr (fromInteger i))
        _                   -> Nothing
      where
        validate n = 0 <= n && n <= 0x10FFFF && not (0xD800 <= n && n <= 0xDFFF)

str2char :: String -> Sexp Literal
str2char [c] = EsLiteral (LiCharacter c)
str2char _   = EsErr EeParseError

escape2char :: String -> Sexp Literal
escape2char = maybe (EsErr EeInvalidEscape) (EsLiteral . LiCharacter) . flip lookup table
  where
    table =
      [ ("n",          '\n')
      , ("newline",    '\n')
      , ("linefeed",   '\n')
      , ("lf",         '\n')
      , ("return",     '\r')
      , ("cr",         '\r')
      , ("t",          '\t')
      , ("tab",        '\t')
      , ("space",       ' ')
      , ("b",          '\b')
      , ("backspace",  '\b')
      , ("f",          '\f')
      , ("v",          '\v')
      , ("a",          '\a')
      , ("\\",         '\\')
      , ("backslash",  '\\')
      , ("\"",          '"')
      , ("dquote",      '"')
      , ("\'",         '\'')
      , ("quote",      '\'')
      , ("0",          '\0')
      , ("NULL",       '\0')
      , ("NUL",        '\0')
      , ("null",       '\0')
      ]


unicode2char :: String -> Sexp Literal
unicode2char s = case str2int 16 ' ' s of
    Just i | validate i -> EsLiteral (LiCharacter (chr (fromInteger i)))
    _                   -> EsErr EeInvalidUnicode
  where
    validate n = 0 <= n && n <= 0x10FFFF && not (0xD800 <= n && n <= 0xDFFF)
