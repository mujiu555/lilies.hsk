module L1 where

import Shared

data L1Identifier
  -- | @<id>@                                                                            Identifier,
  -- holding identifier unique id, all same identifier must have same id, all different identifier must have different id
  = L1Identifier String Integer
  -- | @<id>@                                                                            Symbol value,
  -- datam
  | L1Symbol     String Integer
  deriving (Show, Eq)

data Value
  -- |                                                                                   Symbol
  = ViSymbol     L1Identifier
  -- |                                                                                   Signed 2's Complement Integer
  | ViInteger    Integer         
  -- |                                                                                   IEEE 754 Binary Floating-Point Real
  | ViReal       Double          
  -- |                                                                                   Boolean
  | ViBoolean    Bool            
  -- |                                                                                   Character
  | ViCharacter  Char
  -- |                                                                                   String, escape interpreted
  | ViString     String          
  -- | @()@                                                                              Unit
  | ViUnit                       
  -- |                                                                                   S-Expression
  | ViSexp       (Sexp Value)
  deriving (Show, Eq)

data L1LetBinding a
  -- | @(: <name> <type> <val>)@                             Name Type Value             Binding
  = L1NamBinding  L1Identifier   a   a
  deriving (Show, Eq)

data L1LetFamily  a
  -- | @(let ((: <name> <type> <val>)...) <body>)@                                       Let,
  -- each variable cannot visite others
  = L1Let         [L1LetBinding a] a             
  -- | @(let:seq ((: <name> <type> <val>)...) <body>)@       Sequential                  Let,
  -- later one can visite previous
  | L1LetSeq      [L1LetBinding a] a             
  -- | @(let:rec ((: <name> <type> <val>)...) <body>)@       Recursive                   Let,
  -- each variable can see itself
  | L1LetRec      [L1LetBinding a] a             
  -- | @(let:seqrec ((: <name> <type> <val>)...) <body>)@    Sequential Recursive        Let,
  -- later one can visit previous, and itself
  | L1LetSeqRec   [L1LetBinding a] a             
  deriving (Show, Eq)

data L1MatchPattern a
  -- | @(:or <pattern> ...)@                                 OR                          Pattern match Arm
  = L1PmOr        [a]            
  -- | @(:and <pattern> ...)@                                AND                         Pattern match Arm
  | L1PmAnd       [a]            
  -- | @<lit>@                                               Literal                     Pattern match Arm
  | L1PmLit       Value          
  -- | @_@                                                   Wildcard, ignore            Pattern match Arm
  | L1PmWildcard                 
  -- | @<sym>@                                               Binder                      Pattern match Arm
  | L1PmBinder    a              
  -- | @(<ctor> <pattern>...)@                               Constructor Extraction      Pattern match Arm
  | L1PmCtor      a   [a]        
  -- | @(:guard <pattern> <cond>)                            Guard                       Pattern match Arm
  | L1PmGuard     a   a          
  -- | @(:as <sym> <pattern>)                                Whole Alias                 Pattern match Arm
  | L1PmAs        a   a          
  -- | @(...)@                                               Tuple                       Pattern match Arm
  | L1PmTuple     [a]            
  deriving (Show, Eq)

data L1MatchArm   a
  -- | @(<pattern> <body>)@                                  Arm                         Pattern match
  = L1PmArm       (L1MatchPattern a)   a          
  deriving (Show, Eq)

data L1Match      a
  -- | @(match <scrutinee> (<pattern> <body>)...)@                                       Pattern Match
  = L1Match       a   [L1MatchArm a]        
  -- | @(case <scrutinee> (<lit>...) (<pattern> <body>)...)@ Syntax Object               Pattern Match
  -- lit are symbols that will never be treat as odinary pattern variable
  | L1Case        a   [L1Identifier] [L1MatchArm a]    
  deriving (Show, Eq)

data L1DlCont1    a
  -- | @(reset <body>)@                                      Reset                       Delimited Continuation
  = L1Reset1      a
  -- | @(shift <lambda>)@                                    Shift                       Delimited Continuation
  | L1Shift1      (L1Lambda a)
  deriving (Show, Eq)

data L1Define     a
  -- | @(define <name> <type> <val>)@                                                    Definition
  = L1Define      L1Identifier a a 
  deriving (Show, Eq)

data L1IfThenElse a
  -- | @(if <cond> <then> <else>)@                           If Then Else                Branching
  = L1IfThenElse  a   a   a
  deriving (Show, Eq)

data L1Argument   a
  -- | @{[synsym] <argument>}@                               Argument                    Binding,
  -- parse synsym in l0 to a concrete identifier that corresponding to parameter of the lambda being calling
  = L1Argument    (Maybe L1Identifier) a
  deriving (Show, Eq)

data L1Apply      a
  -- | @(<forms> <argument>...)@                                                         Application
  = L1Apply       a   [L1Argument a]
  deriving (Show, Eq)

data L1LamParam
  -- | @<identifier>@                                                                    Parameter
  = L1ParamMatcher    L1Identifier
  -- | @(<identifier>...)@                                                               Parameter
  | L1ParamList       [L1Identifier]
  deriving (Show, Eq)

data L1SigParam   a
  -- | @<type>@                                                                          Parameter
  = L1SigMatcher  a
  -- | @(:Self <type>...)@                                                               Parameter
  | L1SigList     [a]
  deriving (Show, Eq)

data L1CstBinding a
  -- | @(<type> <type>)@                                     Super Class(Include)        Constraint
  = L1SupBinding a a
  -- | @(<type> <type>)@                                     Type Class(Constraint)      Constraint
  | L1ClsBinding a a
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
--   @(define Monad (class (:Self (type (:Self *) *)) *) (lambda (m) (record (:include (Applicative m)) (: >>= (forall (x y) (function (:Self (m x) (function (:Self x) (m y))) (m y)))))))@
-- For Poly Type
--   @(define List (type (:Self *) *) (lambda (a) (mu (lambda (L) (enum (: list (record (: car a) (: cdr L))) (: nil ()))))))@
-- For Poly Function
--   @(define foo (forall (a) (constraint ((Sum a)) (function (:Self a a) a))) (lambda (dict) (lambda (x y) ((invoke dict sum) x y))))@
-- For implement a type class
--   @(define monad (include ((Applicative Maybe)) (Monad Maybe)) (lambda (cst) (record (: return (invoke cst pure)) (: >>= (lambda (a b) ...)))))@
-- For instance a enum...
--   @(define bar (List #int) (enum (: list (record (: car 1) (: cdr (enum (: nil #unit)))))))
-- For module:
--   @(define baz * (record (: Monad (class (:Self (type (:Self *) *)) *)) (: List (type (:Self *) *)) (: monad (Monad Maybe)) (: bar (List #int))))@
-- For instance a module:
--   @(define mod-baz baz (record (: Monad Monad) (: List List) (: monad monad) (: bar bar)))@
-- Poly module is also support,
-- For macro:
--   @(define when (macro #syn #syn) (lambda ctx (case ctx () ((_ cond then) (syntax (list (quote if) cond then ()))) (:else (error "")))))@
-- For non named signature
--   @(define id (forall (a) (mu (lambda (L) (function (:Self (tuple L a)) a)))) (lambda f (case ((_ a) a) (:else (error "")))))@
--   It is resonable, but, actually, will never pass the type check, so, type level recursive will not allowed,
--   you can try to letting function to accept function invoke expression itself, but it will never pass the compilication
data L1Sig        a
  -- | @(function <param> <return>)@                         Function                    Signature
  = L1Function    (L1SigParam a)   a          
  -- | @(macro <param> <syntax>)@                            Macro                       Signature
  | L1Macro       (L1SigParam a)   a          
  -- | @(type <param> <type-sig>)@                           Type Constructor            Signature
  | L1Type        (L1SigParam a)   a          
  -- | @(class <param> <type-sig>)@                          Type Class                  Signature
  | L1Class       (L1SigParam a)   a          
  -- | @(module <param> <module-sig>)@                       Module                      Signature
  | L1Module      (L1SigParam a)   a          
  -- | @(constraint (<type-class-constraint>...) <body>)@    Type                        Constraint
  | L1Constraint  [L1CstBinding a] a          
  -- | @(include (<super-class-constraint>...) <body>)@      Super                       Constraint
  -- Type Class Inheritance
  | L1Include     [L1CstBinding a] a          
  deriving (Show, Eq)

data L1Lambda     a
  -- | @(lambda <param> <body>)@                             Lambda                      Expression
  -- NOTE: Unlike the lambda appearing in macros, a runtime lambda that wants to
  --   use the matcher form must be able to express the whole call expression,
  --   which contains the function itself.  Doing so constructs a type for that
  --   bound object which needs to encode itself.  Although we have @mu@, this
  --   still fails because of negative-position self-reference, so it can never
  --   pass the type check.  The lambda in macros, by contrast, only receives
  --   the expression itself.
  = L1Lambda      L1LamParam   a          
  -- | @(forall (<id>...) <body>)@                           Typed                       Constructor,
  -- NOTE: The slot is typed as 'L1Param', so a matcher (@L1ParamMatcher@)
  --   is syntactically admissible here.  Logically it is also sound: the
  --   matcher would bind the whole type expression as one object.
  --
  --   However, matching a type-level object requires a universe level higher
  --   than the one @forall@ itself lives in, so that the bound object can be
  --   treated as a whole.  We do not provide such a universe level, and we
  --   do not intend to (that would amount to actual dependent types)
  --
  --   Furhtermore, even if we provide dependent type,
  --   such construct would still be unsound, since the type constructor would appear in negative position,
  --   letting the recursive type encode itself and yielding T = T -> T (Girard's paradox)
  | L1Forall      L1LamParam a      
  deriving (Show, Eq)

data L1TypBinder     a
  -- | @(:include <record>)@                                 Structure                   Inheritance,
  -- appear in record only, if signature is a *, means expand definitions into new record,
  -- if signature is a product type, and argument is a existed instance, means fill values automaticaly
  = L1RecInc      a              
  -- | @(: <name/variant> <type>)@                           Name Type                   Binding,
  -- appear in record or enum only
  | L1TypBinding  L1Identifier   a          
  -- | @(: <name/variant> <val>)@                            Value Type                  Binding,
  -- appear in record or enum only
  | L1ValBinding  L1Identifier   a          
  deriving (Show, Eq)

data L1Construct  a
  -- | @(record <binding>...)@                               Product Type                Constructor,
  -- if signature is a *, it creates a new type, with binding to be name-type binding, if signature is a product type, it creates instance for that product type
  = L1Record      [L1TypBinder a]            
  -- | @(enum <binding>...)@                                 Sum Type                    Constructor,
  -- if signature is a *, it creates a new type, with binding to be name-type binding
  | L1Enumerator  [L1TypBinder a]            
  -- | @(tuple <type>...)@                                   Product Type Tuple          Constructor,
  -- if signature is a *, it creates a new type, with binding to be name-type binding
  | L1Tuple       [a]            
  -- | @(mu (lambda (<name>) <body>))@                       Recursive Type              Binder
  | L1Mu          (L1Lambda a)              
  -- | @(fix (lambda (<name>) <body>))@                      Recursive function          Binder
  | L1Fix         (L1Lambda a)
  -- | @(syntax <expr>)@                                     Syntax                      Expression
  | L1Syntax      a              
  -- | @(quote <expr>)@                                      Quote                       Expression
  | L1Quote       a              
  -- | @(list <expr>...)@                                    List                        Expression
  | L1List        [a]            
  -- | @(quasiquote <expr>)@                                 QuasiQuote                  Expression
  | L1QuasiQuote  a              
  -- | @(splice <expr>)@                                     Splice                      Expression
  | L1Splice      a              
  -- | @(unquote <expr>)@                                    Unquote                     Expression
  | L1Unquote     a              
  -- | @(invoke <instance> <sym>)@                           Field/Method                Visitor
  | L1Invoke      a   a          
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
  -- | @(import <module>)@                                   Module                      Expander, just put names in a module into environment
  = L1Import      a
  -- | @(export <identifier>...)@                            Module                      Descriptor, fetch module-sig
  | L1Export      L1Identifier
  -- | @(provide <identifier> <module> <module-sig>)@        Module                      Provider, by providing module name and values, export them all together
  | L1Provide     L1Identifier L1Identifier L1Identifier
  -- | @(require <identifier>)@                              Module                      Resolver, by providing module name, fetch all symbol provided by module
  | L1Require     L1Identifier
  deriving (Show, Eq)

data L1Types
  -- | @#int@                                                Type Int                    Constructor
  = L1Integer                    
  -- | @#real@                                               Type Real                   Constructor
  | L1Real                       
  -- | @#char@                                               Type Char                   Constructor
  | L1Char                       
  -- | @#bool@                                               Type Bool                   Constructor
  | L1Boolean                    
  -- | @#unit@                                               Type Unit                   Constructor
  | L1Unit                       
  -- | @#syn@                                                Type Sexp                   Constructor
  | L1SynObj                     
  -- | @(#type <level>)@                                     Type Type/Universe          Constructor
  | L1TypeKind    Integer        
  deriving (Show, Eq)

data L1Common     a
  -- |                                                       Literal Values
  = L1Literal     Value          
  -- |                                                       Toplevel                    Sequence
  | L1Program     [a]            
  -- | @(annotation (<meta>...) <body>)@                                                 Annotation
  | L1Annotation  [a] a          
  -- |                                                                                   Loc
  | L1Loc         a Span         
  -- |                                                                                   Error, resolved from l0
  | L1Err         Serr           
  deriving (Show, Eq)

{- Following to be macro def in environment -}

data L1LoopFamily a
  -- | @(loop <label> <body>)@                                                           Loop
  = L1Loop        L1Identifier   a          
  -- | @(while <label> <cond> <body>)@                       While                       Loop
  | L1While       L1Identifier   a   a      
  -- | @(until <label> <cond> <body>)@                       Until                       Loop, sugar
  | L1Until       L1Identifier   a   a      
  -- | @(for <label> ((: <name> <type> <val>)...) <body>)@   For                         Loop, named let, sugar for unnamed lambda
  | L1For         L1Identifier   [a] a      
  -- | @(foreach <label> ((<name> <type> <exp>)...) <body>)@ Foreach                     Loop, sugar, sugar for map function
  | L1ForEach     L1Identifier   [a] a      
  -- | @(:break <label> <ret-val>)@                          Loop                        Break
  | L1Break       L1Identifier   a          
  -- | @(:continue <label>)@                                 Loop                        Continue
  | L1Continue    L1Identifier              
  deriving (Show, Eq)

data L1Branching  a
  -- | @(cond (<cond> <then>)...)@                           Conditional                 Branching
  = L1Conditional [a]            
  -- | @(when <cond> <then>)@                                When                        Branching, not allowed to return non-unit
  | L1When        a   a          
  -- | @(unless <cond> <else>)@                              Unless                      Branching, not allowed to return non-unit
  | L1Unless      a   a          
  -- | @(<cond> <then>)@                                     Arm                         Conditional Branching
  | L1CondArm     a   a          
  deriving (Show, Eq)

data L1Pipe       a
  -- | @(then <seed> <funcall>...)@                          Then                        Pipe, first arguement, lower to let
  = L1Then        a   [a]        
  -- | @(pipe <seed> <funcall> ...)@                                                     Pipe, with place holder, lower to let
  | L1Pipe        a   [a]        
  -- | @:<int>@                                              Pipe                        Place Holder
  | L1PipePlace   Integer        
  deriving (Show, Eq)

data L1Sequence   a
  -- | @(sequence <label> <body>...)@                        Code Block                  Sequence
  = L1Sequence    L1Identifier   [a]        
  -- | @(:return <label> <val>)@                             Sequence                    Return
  | L1Return      L1Identifier   a          
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
  | Fl1Lambda      (L1Lambda     L1Form)
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
