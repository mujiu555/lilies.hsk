module L1 where

import Shared

data Value
  = ViSymbol     String  Integer   -- ^                                                                                   Symbol value
  | ViInteger    Integer           -- ^                                                                                   Signed 2's Complement Integer
  | ViReal       Double            -- ^                                                                                   IEEE 754 Binary Floating-Point Real
  | ViBoolean    Bool              -- ^                                                                                   Boolean
  | ViCharacter  Char              -- ^                                                                                   Character
  | ViString     String            -- ^                                                                                   String, escape interpreted
  | ViUnit                         -- ^ @()@                                                                              Unit
  | ViSexp       (Sexp Value)
  deriving (Show, Eq)

data L1LetFamily  a
  = L1Let         [a] a            -- ^ @(let ((: <name> <type> <val>)...) <body>)@                                       Let
  | L1LetSeq      [a] a            -- ^ @(let:seq ((: <name> <type> <val>)...) <body>)@       Sequential                  Let
  | L1LetRec      [a] a            -- ^ @(let:rec ((: <name> <type> <val>)...) <body>)@       Recursive                   Let
  | L1LetSeqRec   [a] a            -- ^ @(let:seqrec ((: <name> <type> <val>)...) <body>)@    Sequential Recursive        Let
  | L1NamBinding  a   a   a        -- ^ @(: <name> <type> <val>)@                             Name Type Value             Binding
  deriving (Show, Eq)

data L1Match      a
  = L1Match       a   [a]          -- ^ @(match <scrutinee> (<pattern> <body>)...)@                                       Pattern Match
  | L1Case        a   [a] [a]      -- ^ @(case <scrutinee> (<lit>...) (<pattern> <body>)...)@ Syntax Object               Pattern Match
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
  deriving (Show, Eq)

data L1DlCont1    a
  = L1Reset1      a                -- ^ @(reset <body>)@                                      Reset                       Delimited Continuation
  | L1Shift1      a                -- ^ @(shift <lambda>)@                                    Shift                       Delimited Continuation
  deriving (Show, Eq)

data L1Define     a
  = L1Define      (L1Define a) a a -- ^ @(define <definition-form> <type> <val>)@                                         Definition
  | L1Binding     a   a   a        -- ^ @(<binding-type> <name> <type>)@                      Type-Constructor            Definition Binding
  | L1Name        a                -- ^ @<id>@                                                Name                        Definition Binding
  deriving (Show, Eq)

data L1IfThenElse a
  = L1IfThenElse  a   a   a        -- ^ @(if <cond> <then> <else>)@                           If Then Else                Branching
  deriving (Show, Eq)

data L1Apply      a
  = L1Apply       a   [a]          -- ^ @(<forms> {[synsym] <argument>}...)@                                              Application
  | L1Argument    (Maybe a) a      -- ^ @[named] <argument>@                                  Argument                    Binding
  deriving (Show, Eq)

-- | L1Sig
-- NOTE: Since we now have only two level of type,
-- It still behaviour like val-type-kind
--
-- NOTE: All things within Sig, is a surface syntax for arrow expression (->) in type theory
-- Including @function@, @macro@, @type@, @class@, @mdoule@
--
-- @(define * (#type 1) (#type 0))@
-- For Poly Type Class
--   @(define Monad (class ((type (*) *)) *) (lambda (m) (record (:include (Applicative m)) (: >>= (forall (x y) (function ((m x) (function (x) (m y))) (m y)))))))@
-- For Poly Type
--   @(define List (type (*) *) (lambda (a) (mu (lambda (L) (enum (: list (record (: car a) (: cdr L))) (: nil ()))))))@
-- For Poly Function
--   @(define foo (forall (a) (constraint ((Sum a)) (function (a a) a))) (lambda (dict) (lambda (x y) ((invoke dict sum) x y))))@
-- For implement a type class
--   @(define monad (include ((Applicative Maybe)) (Monad Maybe)) (lambda (cst) (record (: return (invoke cst pure)) (: >>= (lambda (a b) ...)))))@
-- For instance a enum...
--   @(define bar (List #int) (enum (: list (record (: car 1) (: cdr (enum (: nil #unit)))))))
-- For module:
--   @(define baz * (record (: Monad (class ((type (*) *)) *)) (: List (type (*) *)) (: monad (Monad Maybe)) (: bar (List #int))))@
-- For instance a module:
--   @(define mod-baz baz (record (: Monad Monad) (: List List) (: monad monad) (: bar bar)))@
-- Poly module is also support,
-- For macro:
--   @(define when (macro #syn #syn) (lambda ctx (case ctx ((_ cond then) (syntax (list (quote if) cond then ()))) (:else (error "")))))@
-- For non named signature
--   @(define id (forall (a) (mu (lambda (L) (function (tuple L a) a)))) (lambda f (case ((_ a) a) (:else (error "")))))@
--   It is resonable, but, actually, will never pass the type check, so, type level recursive will not allowed,
--   you can try to letting function to accept function invoke expression itself, but it will never pass the compilication
data L1Sig        a
  = L1Function    a   a            -- ^ @(function <param> <return>)@                         Function                    Signature
  | L1Macro       a   a            -- ^ @(macro <param> <syntax>)@                            Macro                       Signature
  | L1Type        a   a            -- ^ @(type <param> <type-sig>)@                           Type Constructor            Signature
  | L1Class       a   a            -- ^ @(class <param> <type-sig>)@                          Type Class                  Signature
  | L1Module      a   a            -- ^ @(module <param> <module-sig>)@                       Module                      Signature
  | L1Forall      [a] a            -- ^ @(forall (<id>...) <body>)@                           Typed                       Constructor
  | L1Constraint  [a] a            -- ^ @(constraint (<type-class-constraint>...) <body>)@    Type                        Constraint
  | L1Include     [a] a            -- ^ @(include (<super-class-constraint>...) <body>)@      Super                       Constraint
  | L1CstForm     a   [a]          -- ^ @(<constraint> <type>...)@                            Constraint                  Form
  deriving (Show, Eq)

-- | L1Module
--
-- For provide a module:
--   @(provide baz mod-baz baz)@
-- For fetch a module:
--   @(define m (export baz) (require baz))@
-- For import a module
--   @(import m)@
data L1Module     a
  = L1Import      [a]              -- ^ @(import <module>)@                                   Module                      Expander, just put names in a module into environment
  | L1Export      a                -- ^ @(export <identifier>...)@                            Module                      Descriptor, fetch module-sig
  | L1Provide     a   a   a        -- ^ @(provide <identifier> <module> <module-sig>)@        Module                      Provider, by providing module name and values, export them all together
  | L1Require     a                -- ^ @(require <identifier>)@                              Module                      Resolver, by providing module name, fetch all symbol provided by module
  deriving (Show, Eq)

data L1Types
  = L1Integer                      -- ^ @#int@                                                Type Int                    Constructor
  | L1Real                         -- ^ @#real@                                               Type Real                   Constructor
  | L1Char                         -- ^ @#char@                                               Type Char                   Constructor
  | L1Boolean                      -- ^ @#bool@                                               Type Bool                   Constructor
  | L1Unit                         -- ^ @#unit@                                               Type Unit                   Constructor
  | L1SynObj                       -- ^ @#syn@                                                Type Sexp                   Constructor
  | L1TypeKind    Integer          -- ^ @(#type <level>)@                                     Type Type/Universe          Constructor
  deriving (Show, Eq)

data L1Construct  a
  = L1Lambda      a   a            -- ^ @(lambda <param> <body>)@                             Lambda                      Expression
  | L1Record      [a]              -- ^ @(record <binding>...)@                               Product Type                Constructor, if signature is a *, it creates a new type, with binding to be name-type binding, if signature is a product type, it creates instance for that product type
  | L1Enumerator  [a]              -- ^ @(enum <binding>...)@                                 Sum Type                    Constructor, if signature is a *, it creates a new type, with binding to be name-type binding
  | L1Tuple       [a]              -- ^ @(tuple <type>...)@                                   Product Type Tuple          Constructor, if signature is a *, it creates a new type, with binding to be name-type binding
  | L1Mu          a                -- ^ @(mu (lambda (<name>) <body>))@                       Recursive Type              Binder
  | L1Fix         a                -- ^ @(fix (lambda (<name>) <body>))@                      Recursive function          Binder
  | L1RecInc      a                -- ^ @(:include <record>)@                                 Structure                   Inheritance, appear in record only, if signature is a *, means expand definitions into new record, if signature is a product type, and argument is a existed instance, means fill values automaticaly
  | L1TypBinding  a   a            -- ^ @(: <name/variant> <type>)@                           Name Type                   Binding, appear in record or enum only
  | L1ValBinding  a   a            -- ^ @(: <name/variant> <val>)@                            Value Type                  Binding, appear in record or enum only
  | L1Syntax      a                -- ^ @(syntax <expr>)@                                     Syntax                      Expression
  | L1Quote       a                -- ^ @(quote <expr>)@                                      Quote                       Expression
  | L1List        [a]              -- ^ @(list <expr>...)@                                    List                        Expression
  | L1QuasiQuote  a                -- ^ @(quasiquote <expr>)@                                 QuasiQuote                  Expression
  | L1Splice      a                -- ^ @(splice <expr>)@                                     Splice                      Expression
  | L1Unquote     a                -- ^ @(unquote <expr>)@                                    Unquote                     Expression
  deriving (Show, Eq)

data L1Common     a
  = L1Identifier  String Integer   -- ^ @<id>@                                                                            Identifier, holding identifier unique id
  | L1Param       (Either a [a])   -- ^ @(<identifier>...)@ or @<identifier>@                                             Parameter
  | L1Literal     Value            -- ^                                                       Literal Values
  | L1Program     [a]              -- ^                                                       Toplevel                    Sequence
  | L1Annotation  [a] a            -- ^ @(annotation (<meta>...) <body>)@                                                 Annotation
  | L1Loc         a Span           -- ^                                                                                   Loc
  | L1Err         Serr             -- ^                                                                                   Error, resolved from l0
  deriving (Show, Eq)

{- Following to be macro def in environment -}

data L1LoopFamily a
  = L1Loop        a   a            -- ^ @(loop <label> <body>)@                                                           Loop
  | L1While       a   a   a        -- ^ @(while <label> <cond> <body>)@                       While                       Loop
  | L1Until       a   a   a        -- ^ @(until <label> <cond> <body>)@                       Until                       Loop, sugar
  | L1For         a   [a] a        -- ^ @(for <label> ((: <name> <type> <val>)...) <body>)@   For                         Loop, named let, sugar for unnamed lambda
  | L1ForEach     a   [a] a        -- ^ @(foreach <label> ((<name> <type> <exp>)...) <body>)@ Foreach                     Loop, sugar, sugar for map function
  | L1Break       a   a            -- ^ @(:break <label> <ret-val>)@                          Loop                        Break
  | L1Continue    a                -- ^ @(:continue <label>)@                                 Loop                        Continue
  deriving (Show, Eq)

data L1Branching  a
  = L1Conditional [a]              -- ^ @(cond (<cond> <then>)...)@                           Conditional                 Branching
  | L1When        a   a            -- ^ @(when <cond> <then>)@                                When                        Branching, not allowed to return non-unit
  | L1Unless      a   a            -- ^ @(unless <cond> <else>)@                              Unless                      Branching, not allowed to return non-unit
  | L1CondArm     a   a            -- ^ @(<cond> <then>)@                                     Arm                         Conditional Branching
  deriving (Show, Eq)

data L1Pipe       a
  = L1Then        a   [a]          -- ^ @(then <seed> <funcall>...)@                          Then                        Pipe, first arguement, lower to let
  | L1Pipe        a   [a]          -- ^ @(pipe <seed> <funcall> ...)@                                                     Pipe, with place holder, lower to let
  | L1PipePlace   Integer          -- ^ @:<int>@                                              Pipe                        Place Holder
  deriving (Show, Eq)

data L1Sequence   a
  = L1Sequence    a   [a]          -- ^ @(sequence <label> <body>...)@                        Code Block                  Sequence
  | L1Return      a   a            -- ^ @(:return <label> <val>)@                             Sequence                    Return
  deriving (Show, Eq)

{- Following are ASTs -}

-- | Forms, Fl short for Forms Language Forms
-- This is the form data enum that defines all forms, waiting for desugar
-- Anything within angle brackets (`<` and `>`) are text provided by reader
-- square brackets (`[` and `]`) are optional,
-- Brackets(`{` and `}`) meaning they are together,
-- If and only if write angle brackets twice, it means there is a angle
-- A.K.A., do not include trigger or prefix/suffix
-- E.g., @{[opt]<mond>}@ means, opt is optional, mond is mandatory, and all two together to be a form
data L1Form
  = Fl1Define      (L1Define     L1Form)
  | Fl1Signature   (L1Sig        L1Form)
  | Fl1Module      (L1Module     L1Form)
  | Fl1Types                     L1Types
  | Fl1Construct   (L1Construct  L1Form)
  | Fl1IfThenElse  (L1IfThenElse L1Form)
  | Fl1Apply       (L1Apply      L1Form)
  | Fl1Match       (L1Match      L1Form)
  | Fl1Let         (L1LetFamily  L1Form)
  | Fl1Cont1       (L1DlCont1    L1Form)
  | Fl1Common      (L1Common     L1Form)
  deriving (Show, Eq)

-- | L1Res, L1 Named Resolved
data L1Res
  = Rl1Define
