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
  , ln  :: Integer
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
-- when reading source text into lexemes; the third when resolving a parsed unit
-- into L1.
data Serr
  = EeInvalidEscape    -- ^ @#\\name@ or @\\\\name@: no such escape name
  | EeInvalidUnicode   -- ^ @#\\u{...}@: not a code point
  | EeInvalidBoolean   -- ^ a boolean payload that is not @#true@ or @#false@
  | EeParseError       -- ^ a numeric payload was rejected
  | EeUnclosedList     -- ^ @(@ with no matching @)@
  | EeUnclosedString   -- ^ @\"@ with no closing quote
  | EeUnclosedEscape   -- ^ @#\\u{@ with no closing brace
  | EeMissingChar      -- ^ @#\\'@ with nothing after it
  | EeMissingName      -- ^ @#\\@ or @#:@ with an empty name
  | EeUnmatchedClose   -- ^ a @)@ with no @(@ to close
  | EeInvalidChar      -- ^ a character that begins no lexeme
  -- The third group is raised when resolving a parsed unit into L1.
  | EeUnexpectedSynSym -- ^ a @#:name@ outside an argument list: nothing for it to name
  | EeExpectArgument   -- ^ a @#:name@ whose argument is missing, or is itself a @#:name@
  | EeNoSuchParameter  -- ^ a named argument with no parameter of that name on the callee
  | EeInvalidForm      -- ^ a form whose shape does not match the form it heads
  deriving (Show, Eq)

newtype ErrorList
  = ErrorList [(Span, Serr)]
