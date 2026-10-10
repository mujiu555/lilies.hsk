
module T01 where

import L0
import L1
import Shared

namingResolving :: L0p -> L1Form
namingResolving (EsLiteral      l            ) =                         --
namingResolving (EsList         [Sexp l]     ) =                         -- ^ @(...)@               Variable-Length   Heterogeneous
namingResolving (EsSeq          [Sexp l]     ) =                         -- ^                       A Sequence of Expressions, Currently AST Internal Sequence of expressions
namingResolving (EsLoc          Span (Sexp l)) =                         -- ^                       A Node contains Parse Information, AST Internal Information Provider
namingResolving (EsErr          Serr         ) =                         -- ^                       A Node contains Parse Error, AST Internal Information Provider

  = Fl1Definition  (L1Define     L1Form)
  | Fl1Signature   (L1Sig        L1Form)
  | Fl1Types                     L1Types
  | Fl1Construct   (L1Construct  L1Form)
  | Fl1IfThenElse  (L1IfThenElse L1Form)
  | Fl1Application (L1Apply      L1Form)
  | Fl1Match       (L1Match      L1Form)
  | Fl1Cont1       (L1DlCont1    L1Form)
  | Fl1Common      (L1Common     L1Form)
