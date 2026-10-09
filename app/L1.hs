module L1 where

import Shared

data Value
  = ViSymbol     String  Integer   -- ^                                                                                   Symbol value
  | ViInteger    Integer           -- ^                                                                                   Signed 2's Complement Integer
  | ViReal       Double            -- ^                                                                                   IEEE 754 Binary Floating-Point Real
  | ViBoolean    Bool              -- ^                                                                                   Boolean
  | ViCharacter  Char              -- ^                                                                                   Character
  | ViString     String            -- ^                                                                                   String, escape interpreted
  | ViUnit                         -- ^                                                                                   Unit

data L1LetFamily  a
  = L1Let         [a] a            -- ^ @(let ((: <name> <type> <val>)...) <body>)@                                       Let
  | L1LetSeq      [a] a            -- ^ @(let:seq ((: <name> <type> <val>)...) <body>)@       Sequential                  Let
  | L1LetRec      [a] a            -- ^ @(let:rec ((: <name> <type> <val>)...) <body>)@       Recursive                   Let
  | L1LetSeqRec   [a] a            -- ^ @(let:seqrec ((: <name> <type> <val>)...) <body>)@    Sequential Recursive        Let

data L1LoopFamily a
  = L1Loop        a   a            -- ^ @(loop <label> <body>)@                                                           Loop
  | L1While       a   a   a        -- ^ @(while <label> <cond> <body>)@                       While                       Loop
  | L1Until       a   a   a        -- ^ @(until <label> <cond> <body>)@                       Until                       Loop, sugar
  | L1For         a   [a] a        -- ^ @(for <label> ((: <name> <type> <val>)...) <body>)@   For                         Loop, named let, sugar for unnamed lambda
  | L1ForEach     a   [a] a        -- ^ @(foreach <label> ((<name> <type> <exp>)...) <body>)@ Foreach                     Loop, sugar, sugar for map function
  | L1Break       a   a            -- ^ @(:break <label> <ret-val>)@                          Loop                        Break
  | L1Continue    a                -- ^ @(:continue <label>)@                                 Loop                        Continue

data L1Branching  a
  = L1Conditional [a]              -- ^ @(cond (<cond> <then>)...)@                           Conditional                 Branching
  | L1When        a   a            -- ^ @(when <cond> <then>)@                                When                        Branching, not allowed to return non-unit
  | L1Unless      a   a            -- ^ @(unless <cond> <else>)@                              Unless                      Branching, not allowed to return non-unit
  | L1CondArm     a   a            -- ^ @(<cond> <then>)@                                     Arm                         Conditional Branching

data L1Pipe       a
  = L1Then        a   [a]          -- ^ @(then <seed> <funcall>...)@                          Then                        Pipe, first arguement, lower to let
  | L1Pipe        a   [a]          -- ^ @(pipe <seed> <funcall> ...)@                                                     Pipe, with place holder, lower to let
  | L1PipePlace   Integer          -- ^ @:<int>@                                              Pipe                        Place Holder

data L1Sequence   a
  = L1Sequence    a   [a]          -- ^ @(sequence <label> <body>...)@                        Code Block                  Sequence
  | L1Return      a   a            -- ^ @(:return <label> <val>)@                             Sequence                    Return

data BindingForm  a
  = L1ValBinding  a   a   a        -- ^ @(: <name> <type> <val>)@                             Name Type Value             Binding
  | L1TypeBinding a   a            -- ^ @(: <name/variant> <type>)@                           Name Type                   Binding
  | L1EnumBinding a   a   a        -- ^ @(<name> <type> <val>)@                               Enumerator List             Binding

data L1Match      a
  = L1Match       a   [a]          -- ^ @(match <scrutinee> (<pattern> <body>)...)@                                       Pattern Match
  | L1PmArm       a   a            -- ^ @(<pattern> <body>)@                                  Arm                         Pattern match
  | L1PmOr        [a]              -- ^ @(:or <pattern> ...)@                                 OR                          Pattern match Arm
  | L1PmAnd       [a]              -- ^ @(:and <pattern> ...)@                                AND                         Pattern match Arm
  | L1PmLit       Value            -- ^ @<lit>@                                               Literal                     Pattern match Arm
  | L1PmWildcard                   -- ^ @_@                                                   Wildcard, ignore            Pattern match Arm
  | L1PmBinder    a                -- ^ @<sym>@                                               Binder                      Pattern match Arm
  | L1PmCtor      a   [a]          -- ^ @(<ctor> <pattern>...)@                               Constructor Extraction      Pattern match Arm
  | L1PmGuard     a   a            -- ^ @(:guard <pattern> <cond>)                            Guard                       Pattern match Arm
  | L1PmAs        a   a            -- ^ @(:as <sym> <pattern>)                                Whole Alias                 Pattern match Arm
  | L1PmTuple     [a]              -- ^ @(...)@                                               Tuple                       Pattern match Arm

data L1DlCont     a
  = L1Reset       a                -- ^ @(reset <body>)@                                      Reset                       Delimited Continuation
  | L1Shift       a                -- ^ @(shift <lambda>)@                                    Shift                       Delimited Continuation

data L1Define     a
  = L1Define      (L1Define a) a a-- ^ @(define <definition-form> <type> <val>)@                                         Definition
  | L1Binding     a   a   a       -- ^ @(<binding-type> <name> <type>)@                      Type-Constructor            Definition Binding
  | L1Name        a               -- ^ @<id>@                                                Name                        Definition Binding

data L1IfThenElse a
  = L1IfThenElse  a   a   a       -- ^ @(if <cond> <then> <else>)@                           If Then Else                Branching

data L1Apply      a
  = L1Apply       a   [a]         -- ^ @(<forms> {[synsym] <argument>}...)@                                              Application
  | L1Argument    (Maybe a) a     -- ^ @[named] <argument>@                                  Argument                    Binding

-- | L1Sig
-- All the same,
-- Since we now have only two level of type,
-- It still behaviour like val-type-kind
--
-- All things within Sig, is a surface syntax for (->) in type theory, for compatitable with type universe,
-- though we do not support levels beyond kind...
--
-- @(define * (#type 1) (#type 0))@
-- For Poly Type Class
--   @(define Monad (class ((type (*) *)) *) (lambda (m) (record (:include (Applicative m)) (: >>= (forall (x y) (function ((m x) (function (x) (m y))) (m y)))))))@
-- For Poly Type
--   @(define List (type (*) *) (lambda (a) (mu L (enum (: list (record (: car a) (: cdr L))) (: nil ())))))@
-- For Poly Function
--   @(define sum (forall (a) (constraint ((Sum a)) (function (a a) a))) (lambda (dict) (lambda (x y) ((invoke dict sum) x y))))@
-- For implement a type class
--   @(define monad (include ((Applicative Maybe)) (Monad Maybe)) (lambda (cst) (record (: return (invoke cst pure)) (: >>= (lambda (a b) ...)))))@
-- For instance a enum...
--   @(define bar (List #int) (enum (: list (record (: car 1) (: cdr (enum (: nil #unit)))))))
data L1Sig        a
  = L1Function    [a] a            -- ^ @(function <param> <return>)@                         Function                    Signature
  | L1Macro       [a] a            -- ^ @(macro <param> <syntax>)@                            Macro                       Signature
  | L1Type        [a] a            -- ^ @(type <param> <type-sig>)@                           Type Constructor            Signature
  | L1Class       [a] a            -- ^ @(class <param> <type-sig>)@                          Type Class                  Signature
  | L1Forall      [a] a            -- ^ @(forall <param> <body>)@                             Typed                       Constructor
  | L1Constraint  [a] a            -- ^ @(constraint (<type-class-constraint>...) <body>)@    Type                        Constraint
  | L1Include     [a] a            -- ^ @(include (<super-class-constraint>...) <body>)@      Super                       Constraint
  | L1CstForm     a   [a]          -- ^ @(<constraint> <type>...)@                            Constraint                  Form

data L1Types      a
  = L1SynObj      a                -- ^ @(#syn <type>)@                                       Type Syntax Object          Constructor
  | L1TypeKind    Integer          -- ^ @(#type <level>)@                                     Type Type/Universe          Constructor
  | L1Integer                      -- ^ @#int@                                                Type Int                    Constructor
  | L1Real                         -- ^ @#real@                                               Type Real                   Constructor
  | L1Char                         -- ^ @#char@                                               Type Char                   Constructor
  | L1Boolean                      -- ^ @#bool@                                               Type Bool                   Constructor
  | L1Symbol                       -- ^ @#sym@                                                Type Symbol                 Constructor
  | L1Unit                         -- ^ @#unit@                                               Type Unit                   Constructor

data L1Construct  a
  = L1Lambda      [a] a            -- ^ @(lambda (<name>...) <body>)@                         Lambda                      Expression
  | L1Record      [a]              -- ^ @(record (: <name> <type>)...)@                       Product Type                Constructor
  | L1Enumerator  [a]              -- ^ @(enum (: <variant> <type>)...)@                      Sum Type                    Constructor
  | L1Mu          a   a            -- ^ @(mu <name> <type-construct-body>)@                   Recursive Type              Binder
  | L1RecInc      a                -- ^ @(:include <record>)@                                 Structure                   Inheritance

data L1Common     a
  = L1Identifier  String Integer   -- ^ @<id>@                                                                            Identifier
  | L1Literal     Value            -- ^                                                       Literal Values
  | L1FormSeq     [a]              -- ^                                                                                   Sequence
  | L1Annotation  [a] a            -- ^ @(annotation (<meta>...) <body>)@                                                 Annotation
  | L1Loc         a Span           -- ^                                                                                   Loc
  | L1Err                          -- ^                                                                                   Error, resolved from l0

-- | Forms, Fl short for Forms Language Forms
-- This is the form data enum that defines all forms, waiting for desugar
-- Anything within angle brackets (`<` and `>`) are text provided by reader
-- square brackets (`[` and `]`) are optional,
-- Brackets(`{` and `}`) meaning they are together,
-- If and only if write angle brackets twice, it means there is a angle
-- A.K.A., do not include trigger or prefix/suffix
-- E.g., @{[opt]<mond>}@ means, opt is optional, mond is mandatory, and all two together to be a form
data L1Form
  = Fl1Definition  (L1Define     L1Form)
  | Fl1Signature   (L1Sig        L1Form)
  | Fl1Types       (L1Types      L1Form)
  | Fl1Construct   (L1Construct  L1Form)
  | Fl1IfThenElse  (L1IfThenElse L1Form)
  | Fl1Branching   (L1Branching  L1Form)
  | Fl1Let         (L1LetFamily  L1Form)
  | Fl1Loop        (L1LoopFamily L1Form)
  | Fl1Sequence    (L1Sequence   L1Form)
  | Fl1Pipe        (L1Pipe       L1Form)
  | Fl1Application (L1Apply      L1Form)
  | Fl1Binding     (BindingForm  L1Form)
  | Fl1Match       (L1Match      L1Form)
  | Fl1Reset       (L1DlCont     L1Form)
  | Fl1Common      (L1Common     L1Form)

-- | L1Norm, desugar Branching sugar, desugar Loop sugar, pipe, sequence
data L1Norm
  = Nl1Definition  (L1Define     L1Norm)
  | Nl1Signature   (L1Sig        L1Norm)
  | Nl1Types       (L1Types      L1Norm)
  | Nl1Construct   (L1Construct  L1Norm)
  | Nl1IfThenElse  (L1IfThenElse L1Norm)
  | Nl1Let         (L1LetFamily  L1Norm)
  | Nl1Application (L1Apply      L1Norm)
  | Nl1Binding     (BindingForm  L1Norm)
  | Nl1Match       (L1Match      L1Norm)
  | Nl1Reset       (L1DlCont     L1Norm)
  | Nl1Common      (L1Common     L1Norm)

-- | L1Dmth, desugar match
data L1Dmth
  = Dm1Definition  (L1Define     L1Dmth)
  | Dm1Signature   (L1Sig        L1Dmth)
  | Dm1Types       (L1Types      L1Dmth)
  | Dm1Construct   (L1Construct  L1Dmth)
  | Dm1IfThenElse  (L1IfThenElse L1Dmth)
  | Dm1Let         (L1LetFamily  L1Dmth)
  | Dm1Application (L1Apply      L1Dmth)
  | Dm1Binding     (BindingForm  L1Dmth)
  | Dm1Reset       (L1DlCont     L1Dmth)
  | Dm1Common      (L1Common     L1Dmth)

data L1Dlet
  = Dl1Definition  (L1Define     L1Dlet)
  | Dl1Signature   (L1Sig        L1Dlet)
  | Dl1Types       (L1Types      L1Dlet)
  | Dl1Construct   (L1Construct  L1Dlet)
  | Dl1IfThenElse  (L1IfThenElse L1Dlet)
  | Dl1Application (L1Apply      L1Dlet)
  | Dl1Binding     (BindingForm  L1Dlet)
  | Dl1Reset       (L1DlCont     L1Dlet)
  | Dl1Common      (L1Common     L1Dlet)
