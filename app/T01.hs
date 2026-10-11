module T01 where

import L0
import L1
import Shared

resolve :: L0p -> L1Form
resolve (EsLiteral (LiInteger                  i)) = Fl1Common (L1Literal   (ViInteger i))
resolve (EsLiteral (LiReal                     r)) = Fl1Common (L1Literal      (ViReal r))
resolve (EsLiteral (LiBoolean                  b)) = Fl1Common (L1Literal   (ViBoolean b))
resolve (EsLiteral (LiCharacter                c)) = Fl1Common (L1Literal (ViCharacter c))
resolve (EsLiteral (LiString                   s)) = Fl1Common (L1Literal    (ViString s))
resolve (EsList [EsLiteral (LiSymbol "quote"), l]) = Fl1Construct (L1Quote (resolve l))
resolve (EsList                     (fun: params)) = Fl1Apply  (L1Apply (resolve fun) (map resolve params))
resolve (EsSeq                                  l) = Fl1Common (L1Program (map resolve l))
resolve (EsLoc                                s e) = Fl1Common (L1Loc       (resolve e) s)
resolve (EsErr                                  e) = Fl1Common (L1Err                   e)
resolve _                                          = Fl1Common (L1Err       EeMissingName)
