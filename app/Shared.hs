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

-- | Serr, Ee short for Error expression
data Serr
  = EeMissing
  | EeInvalidEscape
  | EeInvalidUnicode
  | EeInvalidBoolean
  | EeParseError
