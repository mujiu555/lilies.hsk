module Shared where

data Span = Span
  { loc :: Integer
  , col :: Integer
  , len :: Integer
  , slice :: String
  , line :: String
  , info :: String
  , source :: String
  }
  deriving (Show, Eq)

-- | Serr, Ee short for Error expression
--
-- The first group is raised when decoding a lexeme into a value; the second
-- when reading source text into lexemes.
data Serr
  = EeInvalidEscape    -- ^ @#\\name@ or @\\\\name@: no such escape name
  | EeInvalidUnicode   -- ^ @#\\u{...}@: not a code point
  | EeInvalidBoolean   -- ^ @#t...@ or @#f...@: not @#true@ or @#false@
  | EeParseError       -- ^ a numeric payload was rejected
  | EeUnclosedList     -- ^ @(@ with no matching @)@
  | EeUnclosedString   -- ^ @\"@ with no closing quote
  | EeUnclosedEscape   -- ^ @#\\u{@ with no closing brace
  | EeMissingChar      -- ^ @#\\'@ with nothing after it
  | EeMissingName      -- ^ @#\\@ or @#:@ with an empty name
  | EeUnmatchedClose   -- ^ a @)@ with no @(@ to close
  | EeUnknownSigil     -- ^ @#@ beginning none of the known forms
  | EeInvalidChar      -- ^ a character that begins no lexeme
  deriving (Show, Eq)

newtype ErrorList
  = ErrorList [(Span, Serr)]
