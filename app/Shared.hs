module Shared where

-- | Sexp, Es short for Expression S-Expression
-- The parse information a node carries is a 'Span'.
data Sexp l
  = EsLiteral      l
  | EsList         [Sexp l]                                -- ^ @(...)@               Variable-Length   Heterogeneous
  | EsSeq          [Sexp l]                                -- ^                       A Sequence of Expressions, Currently AST Internal Sequence of expressions
  | EsLoc          Span (Sexp l)                           -- ^                       A Node contains Parse Information, AST Internal Information Provider
  | EsErr          Serr                                    -- ^                       A Node contains Parse Error, AST Internal Information Provider
  deriving (Show, Eq)

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
