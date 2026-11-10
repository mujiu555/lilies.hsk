{-# LANGUAGE GADTs #-}
module L1Sexpr where

data Span
  = Span FilePath Int Int

data Atom i where
  SexprInt  :: Integer  -> Atom Integer
  SexprRat  :: Rational  -> Atom Rational
  SexprReal :: Double  -> Atom Double
  SexprBool :: Bool  -> Atom Bool
  SexprChar :: Char  -> Atom Char
  SexprStr  :: String  -> Atom String

data Sexpr where
  SexprCons :: Sexpr -> Sexpr -> Sexpr
  SexprNil  :: Sexpr
  SexprAtom :: (Atom a) -> Sexpr
  SexprSym  :: String -> Sexpr
  SexprSeq  :: [Sexpr] -> Sexpr

data Environment
  = Identifier Integer String
  | Value      Sexpr
  | Lambda     [Environment] Sexpr
  | None

data SynObj where
  SynCons :: SynObj -> SynObj -> SynObj
  SynNil  :: SynObj
  SynAtom :: (Atom a) -> SynObj
  SynSym  :: Environment -> SynObj
  SynSeq  :: [SynObj] -> SynObj
  SynObj  :: [Environment] -> SynObj -> SynObj
