{-# LANGUAGE ScopedTypeVariables #-}

module Reader
  ( ToLoc (..)
  , Site (..)
  , delimited
  , l0
  , l0With
  ) where

import Control.Applicative (Alternative, optional, (<|>))
import Control.Monad (void)
import Data.Char (isSpace)
import Data.List (intercalate)
import Data.Maybe (catMaybes, isJust)
import Data.Void (Void)

import qualified L0 as N
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C

import Shared

type Parser = P.Parsec Void String

-- | An L0 node located by @i@.
type Node i = N.Sexp i N.Lexeme

-- | What 'Reader' knows about a node's position and its attached comments,
--   before it is turned into the caller's own location type.
data Site = Site
  { siteOffset   :: Int      -- ^ 0-based character offset of the node
  , siteColumn   :: Int      -- ^ 1-based column of the node
  , siteLength   :: Int      -- ^ length of the node in characters
  , siteSlice    :: String   -- ^ the node's own source text
  , siteLine     :: String   -- ^ the whole source line the node starts on
  , siteComments :: [String] -- ^ comments attached to the node
  , siteSource   :: String   -- ^ source name
  }
  deriving (Show, Eq)

-- | Turn a 'Site' into the caller's location type @i@.  Provide an instance to
--   substitute a location type of your own; 'Span' is the built-in default.
class ToLoc i where
  toLoc :: Site -> i

instance ToLoc Span where
  toLoc s =
    Span
      { loc    = fromIntegral (siteOffset s)
      , col    = fromIntegral (siteColumn s)
      , len    = fromIntegral (siteLength s)
      , slice  = siteSlice s
      , line   = siteLine s
      , info   = intercalate "; " (siteComments s)
      , source = siteSource s
      }

-- | @delimited open close body@ parses @open body close@.  A missing closer is
--   returned as 'Nothing' instead of failing, so that an unterminated group
--   still produces a node.
--
--   The recursive knot belongs to the caller: @body@ is handed a parser that
--   stops at the closer, which is how nesting is expressed.  The earlier
--   @(m a -> m a) -> m a@ shape could only ever describe a one-element group,
--   since @f = many@ would need @a ~ [a]@.
delimited :: (Monad m, Alternative m)
          => m trigger -> m delimiter -> m body -> m (body, Maybe delimiter)
delimited trigger delimiter body = do
  _ <- trigger
  b <- body
  d <- optional delimiter
  pure (b, d)

-- | Lex and parse a whole source unit into L0 with 'Span' locations.
l0 :: String -> String -> Node Span
l0 = l0With

-- | Lex and parse a whole source unit into L0, with the location type chosen by
--   the caller.
--
--   Total: no input makes this fail.  Every position where no lexeme can be
--   formed becomes an 'N.EsErr' placeholder wrapped in an 'N.EsLoc', so the
--   result is always a best-effort tree whose shape mirrors the input.
l0With :: forall i. ToLoc i => String -> String -> Node i
l0With name src =
  case P.runParser (top <* P.eof) name src of
    Left  _        -> N.EsLoc (toLoc (fileSite [])) (N.EsErr EeParseError)
    Right (cs, xs) -> N.EsLoc (toLoc (fileSite cs)) (N.EsSeq xs)
  where
    top :: Parser ([String], [Node i])
    top = do
      cs <- layout
      xs <- seqUntil topEnd
      pure (cs, xs)

    ---------------------------------------------------------------- layout

    -- | Skip layout, returning the comments found (inner text, trimmed).
    layout :: Parser [String]
    layout = filter (not . null) . catMaybes <$> P.many piece
      where
        piece :: Parser (Maybe String)
        piece = (Nothing <$ void whitespace) <|> (Just . trim <$> comment)

    whitespace :: Parser Char
    whitespace = P.satisfy isSpace

    comment :: Parser String
    comment = lineComment <|> blockComment

    -- | @; ...@ to the end of the line.
    lineComment :: Parser String
    lineComment = do
      _  <- C.char ';'
      cs <- P.many (P.satisfy (/= '\n'))
      _  <- optional (C.char '\n')
      pure (trim cs)

    -- | A nestable @#| ... |#@ block.  An unterminated block is taken to run to
    --   the end of the input rather than failing.
    blockComment :: Parser String
    blockComment = C.string "#|" *> go (1 :: Int) ""
      where
        go :: Int -> String -> Parser String
        go depth acc
          | depth <= 0 = pure (reverse acc)
          | otherwise  = do
              eof <- P.atEnd
              if eof
                then pure (reverse acc)
                else do
                  closer <- optional (P.try (C.string "|#"))
                  case closer of
                    Just _  -> go (depth - 1) acc
                    Nothing -> do
                      opener <- optional (P.try (C.string "#|"))
                      case opener of
                        Just _  -> go (depth + 1) ('|' : '#' : acc)
                        Nothing -> do
                          c <- P.anySingle
                          go depth (c : acc)

    trim :: String -> String
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

    ---------------------------------------------------------------- nodes

    -- | Whether the input is exhausted.  'P.atEnd' already answers 'Bool'.
    topEnd :: Parser Bool
    topEnd = P.atEnd

    -- | Stop at a closing parenthesis, or at the end of the input.  Not
    --   @'P.atEnd' <|> ...@: 'P.atEnd' *succeeds* with 'False', so @<|>@ would
    --   never reach the parenthesis test.
    listEnd :: Parser Bool
    listEnd = do
      eof <- P.atEnd
      if eof
        then pure True
        else isJust <$> optional (P.lookAhead (C.char ')'))

    -- | Parse nodes until @end@ says stop.  The caller guarantees the position
    --   is free of layout, and the last 'node' leaves it free of layout again.
    seqUntil :: Parser Bool -> Parser [Node i]
    seqUntil end = do
      stop <- end
      if stop
        then pure []
        else do
          x  <- node
          xs <- seqUntil end
          pure (x : xs)

    -- | One node, wrapped in 'N.EsLoc'.  The location is built last, so that
    --   the comments trailing the node are already known when 'toLoc' runs.
    node :: Parser (Node i)
    node = do
      o0 <- P.getOffset
      c0 <- column
      x  <- body
      o1 <- P.getOffset
      cs <- layout
      pure (N.EsLoc (toLoc (site o0 c0 (o1 - o0) cs)) x)

    column :: Parser Int
    column = P.unPos . P.sourceColumn <$> P.getSourcePos

    -- | A node without its location.  The alternatives are tried in the order
    --   that resolves the prefix overlaps, and every risky one is 'P.try' so
    --   that a partial match rewinds and 'recover' stays reachable.
    body :: Parser (Node i)
    body = P.try list
       <|> P.try rawString
       <|> P.try boolean
       <|> P.try syntaxSym
       <|> P.try hashChar
       <|> P.try string
       <|> P.try number
       <|> symbol
       <|> P.try badBoolean
       <|> recover

    -- | Anything unlexable.  Each branch consumes at least one character, so
    --   the item loops always make progress, and the last one is total.
    recover :: Parser (Node i)
    recover = unmatchedClose <|> unknownSigil <|> invalidChar

    -- | A @)@ with no opener.
    unmatchedClose :: Parser (Node i)
    unmatchedClose = N.EsErr EeUnmatchedClose <$ C.char ')'

    -- | A @#@ that starts none of the forms above, taken together with the name
    --   it introduces so that a single node covers @#z@ rather than leaving a
    --   stray @z@ behind.
    unknownSigil :: Parser (Node i)
    unknownSigil = do
      _ <- C.char '#'
      _ <- P.many symbolChar
      pure (N.EsErr EeUnknownSigil)

    -- | A character that begins no lexeme.  Unreachable for the lexemes above,
    --   but kept as the total fallback that guarantees progress.
    invalidChar :: Parser (Node i)
    invalidChar = N.EsErr EeInvalidChar <$ P.anySingle

    list :: Parser (Node i)
    list = do
      (xs, closed) <- delimited (C.char '(') (C.char ')') (seqUntil listEnd)
      case closed of
        Just _  -> pure (N.EsList xs)
        Nothing -> do
          o <- P.getOffset
          c <- column
          pure (N.EsList (xs ++ [N.EsLoc (toLoc (site o c 0 [])) (N.EsErr EeUnclosedList)]))

    ------------------------------------------------------------- symbols

    symbolChar :: Parser Char
    symbolChar = P.satisfy $ \c -> not (isSpace c) && c `notElem` "()\";"

    -- | An ordinary identifier.  Guarded so that it never takes over a number
    --   or one of the @#@ forms.
    symbol :: Parser (Node i)
    symbol = do
      P.notFollowedBy (C.char '#')
      P.notFollowedBy C.digitChar
      P.notFollowedBy (P.try (P.oneOf "+-" *> C.digitChar))
      cs <- P.some symbolChar
      pure (N.EsLiteral (N.LaSymbol cs))

    ------------------------------------------------------------- sigils

    boolean :: Parser (Node i)
    boolean = do
      s <- C.string "#true" <|> C.string "#false"
      P.notFollowedBy symbolChar
      pure (N.EsLiteral (N.LaBoolean s))

    -- | @#t@ or @#f@ followed by more name: a misspelled boolean.
    badBoolean :: Parser (Node i)
    badBoolean = do
      _ <- C.char '#'
      _ <- P.oneOf "tf"
      _ <- P.many symbolChar
      pure (N.EsErr EeInvalidBoolean)

    syntaxSym :: Parser (Node i)
    syntaxSym = do
      _  <- C.string "#:"
      cs <- P.many symbolChar
      pure $ case cs of
        [] -> N.EsErr EeMissingName
        _  -> N.EsLiteral (N.LaSyntaxSym cs)

    rawString :: Parser (Node i)
    rawString = do
      _      <- C.string "#r\""
      rs     <- P.many (P.satisfy (/= '"'))
      closed <- optional (C.char '"')
      pure $ case closed of
        Just _  -> N.EsLiteral (N.LaRawString rs)
        Nothing -> N.EsErr EeUnclosedString

    -- | The body is kept exactly as written, escapes included; 'P0.strstr' is
    --   what interprets them.  All this needs to know is that a backslash binds
    --   the character after it, so that @\\"@ does not close the string.
    string :: Parser (Node i)
    string = do
      _      <- C.char '"'
      rs     <- concat <$> P.many (P.try escaped <|> plain)
      closed <- optional (C.char '"')
      pure $ case closed of
        Nothing -> N.EsErr EeUnclosedString
        Just _  -> N.EsLiteral (N.LaString rs)

    escaped :: Parser String
    escaped = do
      b <- C.char '\\'
      c <- P.anySingle
      pure [b, c]

    plain :: Parser String
    plain = (: []) <$> P.satisfy (/= '"')

    -- | @#\\u{...}@, @#\\'<char>@ and @#\\<name>@.  Longest form first: a bare
    --   @#\\u@ is the name @u@, and @#\\'@ starts a character.
    --
    --   Only the framing is checked here -- that the delimiter is present and
    --   the payload is non-empty.  Whether the code point is in range or the
    --   name is one 'P0' knows is 'P0'\'s judgement, so the text is passed on
    --   untouched and 'P0' reports 'EeInvalidUnicode' / 'EeInvalidEscape'.
    hashChar :: Parser (Node i)
    hashChar = do
      _ <- C.string "#\\"
      P.try unicodeChar <|> P.try quoteChar <|> escapedChar

    unicodeChar :: Parser (Node i)
    unicodeChar = do
      _      <- C.string "u{"
      hex    <- P.many (P.satisfy (/= '}'))
      closed <- optional (C.char '}')
      pure $ case closed of
        Nothing -> N.EsErr EeUnclosedEscape
        Just _  -> N.EsLiteral (N.LaUnicode hex)

    quoteChar :: Parser (Node i)
    quoteChar = do
      _  <- C.char '\''
      mc <- optional P.anySingle
      pure $ case mc of
        Just c  -> N.EsLiteral (N.LaCharacter [c])
        Nothing -> N.EsErr EeMissingChar

    escapedChar :: Parser (Node i)
    escapedChar = do
      cs <- P.many nameChar
      pure $ case cs of
        [] -> N.EsErr EeMissingName
        _  -> N.EsLiteral (N.LaEscaped cs)

    nameChar :: Parser Char
    nameChar = P.satisfy $ \c -> not (isSpace c) && c `notElem` "();"

    ------------------------------------------------------------- numbers

    -- | @[sign]@ then one of the seven numeric forms.  The sign only counts
    --   when a digit follows it, so a lone @-@ stays a symbol.
    number :: Parser (Node i)
    number = do
      sg <- optional (P.oneOf "+-")
      mk <- P.try numberBody
      pure (mk sg)

    numberBody :: Parser (Maybe Char -> Node i)
    numberBody = P.try radixBody <|> decimalBody

    -- | @0o@, @0x@ and @0b@, tried before the decimal form so that a bare
    --   @0x@ falls back to the decimal @0@ followed by the symbol @x@.
    radixBody :: Parser (Maybe Char -> Node i)
    radixBody = do
      _  <- C.char '0'
      k  <- P.oneOf "oxb"
      ds <- digits (radixDigit k)
      pure $ \sg -> N.EsLiteral $ case k of
        'o' -> N.LaOctal       sg ds
        'x' -> N.LaHexadecimal sg ds
        _   -> N.LaBinary      sg ds

    radixDigit :: Char -> Parser Char
    radixDigit 'o' = C.octDigitChar
    radixDigit 'x' = C.hexDigitChar
    radixDigit _   = C.binDigitChar

    -- | A run of digits with the group separator allowed in between.  Where the
    --   separator may sit is 'P0'\'s business, so the run is taken greedily.
    digits :: Parser Char -> Parser String
    digits d = P.some (d <|> C.char '\'')

    decimalBody :: Parser (Maybe Char -> Node i)
    decimalBody = do
      a <- digits C.digitChar
      P.try (floatBody a)
        <|> P.try (rationalBody a)
        <|> P.try (standardBody a)
        <|> pure (\sg -> N.EsLiteral (N.LaDecimal sg a))

    floatBody :: String -> Parser (Maybe Char -> Node i)
    floatBody a = do
      _ <- C.char '.'
      b <- digits C.digitChar
      pure $ \sg -> N.EsLiteral (N.LaFloat sg a b)

    rationalBody :: String -> Parser (Maybe Char -> Node i)
    rationalBody a = do
      _ <- C.char '/'
      b <- digits C.digitChar
      pure $ \sg -> N.EsLiteral (N.LaRational sg a b)

    standardBody :: String -> Parser (Maybe Char -> Node i)
    standardBody a = do
      _  <- C.char 'e'
      se <- optional (P.oneOf "+-")
      b  <- digits C.digitChar
      pure $ \sg -> N.EsLiteral (N.LaStandard sg a se b)

    ------------------------------------------------------------- spans

    site :: Int -> Int -> Int -> [String] -> Site
    site o c n cs =
      Site
        { siteOffset   = o
        , siteColumn   = c
        , siteLength   = n
        , siteSlice    = take n (drop o src)
        , siteLine     = lineAt o
        , siteComments = cs
        , siteSource   = name
        }

    fileSite :: [String] -> Site
    fileSite = site 0 1 (length src)

    -- | The whole source line containing the given offset.
    lineAt :: Int -> String
    lineAt off =
      case takeWhile ((<= off) . fst) (zip lineStarts (lines src)) of
        [] -> ""
        ls -> snd (last ls)

    lineStarts :: [Int]
    lineStarts = 0 : [ i + 1 | (i, ch) <- zip [0 ..] src, ch == '\n' ]

