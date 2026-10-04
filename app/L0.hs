{-# LANGUAGE ScopedTypeVariables #-}
module L0 where

import Shared
-- | Lexeme, La short for Lexeme and Atom
-- Anything within angle brackets are text provided by reader
-- square brackets are optional,
-- If and only if write angle brackets twice, it means there is a angle
-- A.K.A., do not include trigger or prefix/suffix
-- NOTE:
-- 1.  <sym> is just a sequence of valid characters, (text)
-- 2.  [sign] is a optional `+` or `-`
-- 3.  <int> means decimal numbers literal, possible `1234567890` and `'`
-- 4.  <oct> means octal numbers literal, possible `12345670` and `'`
-- 5.  <hex> means hexadecimal numbers literal, possible `123456789abcdef0` and `'`
-- 6.  <bin> means binary numbers literal, possible `10` and `'`
-- 7.  <n> and <m> are decimal numbers
-- 8.  <base> and <exp> are decimal numbers
-- 9.  for boolean, the provided string is either "#true" or "#false"
-- 10. <char> is any single unicode character, treat as coded in UTF-8
-- 11. <name> is ASCII escaped character name, or short name like C-Style escape set
-- 12. <unicode> is a sequence of available Unicode Code Point ID
-- 13. <str> is a continuous escape uninterpreted text that delimited by double quote `"`
-- 14. within escape interpreting string, there are three method for escape:
--   - @\\<short-name>@: C-Style escape, e.g., @\\r@ for return, @\\n@ for newline (linefeed)
--   - @\\u{<unicode>}@: unicode code point, decoded into utf-8 automatically
data Lexeme
  = LaSymbol       String                                  -- ^ @<sym>@                     Ordinary Identifier
  | LaSyntaxSym    String                                  -- ^ @#:<sym>@                   Literal           Symbol
  | LaDecimal      (Maybe Char) String                     -- ^ @[sign]<int>@               Decimal Form      Integer
  | LaOctal        (Maybe Char) String                     -- ^ @[sign]0o<oct>@             Octal Form        Integer
  | LaHexadecimal  (Maybe Char) String                     -- ^ @[sign]0x<hex>@             Hexadecimal Form  Integer
  | LaBinary       (Maybe Char) String                     -- ^ @[sign]0b<bin>@             Binary Form       Integer
  | LaFloat        (Maybe Char) String String              -- ^ @[sign]<n>.<m>@             Float-Point       Real Number
  | LaRational     (Maybe Char) String String              -- ^ @[sign]<n>/<m>@             Rational Form     Real Number
  | LaStandard     (Maybe Char) String (Maybe Char) String -- ^ @[sign]<base>e[sign]<exp>@  Standard Form     Real Number
  | LaBoolean      String                                  -- ^ @#true@/@#false@                              Boolean
  | LaCharacter    String                                  -- ^ @#\\'<char>@                                  Character
  | LaEscaped      String                                  -- ^ @#\\<name>@                 Named             Character
  | LaUnicode      String                                  -- ^ @#\\u{<unicode>}@           Unicode Form      Character
  | LaString       String                                  -- ^ @"<str>"@                   Regular           String, escapes uninterpreted
  | LaRawString    String                                  -- ^ @#r"<str>"@                 Raw               String, taken verbatim
  deriving (Show, Eq)

-- | Literal, Li short for Literal
-- This defines all literal values
-- 1. Symbol do not have semantic information, thus cannot tell if it is value or identifier
-- 2. SyntaxSym is a kind of marker for named argument in function/special form call
-- 3. Unlike Symbol, SyntaxSym across one file unit shares one naming space
-- 4. Integer is always signed, assign semantic information after L1, tell the real runtime type information
-- 5. Real is IEEE 754 64-bit Binary Float-Point Real Number, assign semantic information after L1
-- 6. Boolean, algebric type, interpreted boolean immediate
-- 7. Character, algebric type, interpreted character immediate
-- 8. String, interpreted array of character
data Literal
  = LiSymbol       String                                  -- ^                       Ordinary Identifier, resolved by context
  | LiSyntaxSym    String                                  -- ^                       Literal           Symbol, if they have same name, they are same
  | LiInteger      Integer                                 -- ^                                         Signed 2's Completion Integer
  | LiReal         Double                                  -- ^                                         IEEE 754 Binary Float-Point Real
  | LiBoolean      Bool                                    -- ^                                         Boolean
  | LiCharacter    Char                                    -- ^                                         Character
  | LiString       String                                  -- ^                                         String, escape interpreted
  deriving (Show, Eq)

-- | Sexp, Es short for Expression S-Expression
data Sexp i l
  = EsLiteral      l
  | EsList         [Sexp i l]                              -- ^ @(...)@               Variable-Length   Heterogeneous
  | EsSeq          [Sexp i l]                              -- ^                       A Sequence of Expressions, Currently AST Internal Sequence of expressions
  | EsLoc          i (Sexp i l)                            -- ^                       A Node contains Parse Information, AST Internal Information Provider
  | EsErr          Serr                                    -- ^                       A Node contains Parse Error, AST Internal Information Provider
  deriving (Show, Eq)

