{-| Readable renderings of the two trees the pipeline passes around: the located L0
    tree the reader produces, and the resolved L1 form tree.

    These are for inspection, so they show what the trees carry rather than trying
    to match source syntax exactly.  A name is printed as @name#id@, a name used as
    data as @'name#id@, and a location as a fixed-width @[line:startCol-endCol]@
    tag on the line of the node it belongs to.  @[(a a)]@-style signature
    parameters keep the @(:Self ...)@ spelling their comment gives them.
-}
module Pretty
  ( prettyL0
  , prettyL1
  ) where

import Data.Char (isDigit)

import L0
import L1
import Shared

-- * L0

-- | The L0 tree, one node per line, indented by nesting.
prettyL0 :: Sexp Lexeme -> String
prettyL0 = unlines . align . l0node 0

l0node :: Int -> Sexp Lexeme -> [String]
l0node d (EsLoc sp x)  = tag sp (l0node d x)
l0node d (EsLiteral l) = [pad d ++ lexemeText l]
l0node d (EsList xs)   = group d "(" l0node xs
l0node d (EsSeq xs)    = bracket d "<seq" ">" l0node xs
l0node d (EsErr e)     = [pad d ++ "#err " ++ show e]

-- | One lexeme, spelled as the reader's own documentation spells it.
lexemeText :: Lexeme -> String
lexemeText (LaSymbol s)          = "Sym " ++ show s
lexemeText (LaSyntaxSym s)       = "SynSym " ++ show s
lexemeText (LaDecimal sg ds)     = "Int " ++ signed sg ds
lexemeText (LaOctal sg ds)       = "Oct " ++ signed sg ds
lexemeText (LaHexadecimal sg ds) = "Hex " ++ signed sg ds
lexemeText (LaBinary sg ds)      = "Bin " ++ signed sg ds
lexemeText (LaFloat sg a b)      = "Float " ++ signed sg (a ++ "." ++ b)
lexemeText (LaRational sg a b)   = "Ratio " ++ signed sg (a ++ "/" ++ b)
lexemeText (LaStandard sg b se e) = "Std " ++ signed sg (b ++ "e" ++ maybe "" (: []) se ++ e)
lexemeText (LaBoolean s)         = s
lexemeText (LaCharacter s)       = "Char " ++ show s
lexemeText (LaEscaped s)         = "Esc " ++ show s
lexemeText (LaUnicode s)         = "Uni " ++ show s
lexemeText (LaString s)          = "Str " ++ show s
lexemeText (LaRawString s)       = "Raw " ++ show s

signed :: Maybe Char -> String -> String
signed (Just c) s = c : s
signed Nothing  s = s

-- * L1

-- | The resolved form tree, one form per line, indented by nesting.
prettyL1 :: L1Form -> String
prettyL1 = unlines . align . l1 0

l1 :: Int -> L1Form -> [String]
l1 d fm = case fm of
  Fl1Common (L1Loc x sp)      -> tag sp (l1 d x)
  Fl1Common (L1Literal v)     -> [pad d ++ valueText v]
  Fl1Common (L1Program xs)    -> group d "(program" l1 xs
  Fl1Common (L1Annotation ms b) -> group d "(annotation" l1 (ms ++ [b])
  Fl1Common (L1Err e)         -> [pad d ++ "#err " ++ show e]
  Fl1Define (L1Define i ty v) ->
    [pad d ++ "(define " ++ nameText i] ++ l1 (d + 1) ty ++ l1 (d + 1) v ++ [pad d ++ ")"]
  Fl1Lambda x                 -> lambda d x
  Fl1Signature s              -> signature d s
  Fl1Module m                 -> moduleForm d m
  Fl1Types t                  -> [pad d ++ typesText t]
  Fl1Construct c              -> construct d c
  Fl1IfThenElse (L1IfThenElse c t e) -> group d "(if" l1 [c, t, e]
  Fl1Apply (L1Apply h as)     ->
    [pad d ++ "(apply"] ++ l1 (d + 1) h ++ concatMap (arg (d + 1)) as ++ [pad d ++ ")"]
  Fl1Match m                  -> matchForm d m
  Fl1Let l                    -> letFamily d l
  Fl1Cont1 c                  -> cont d c

-- | The same form on one line, for a slot that has to stay inline.  It is the
--   multi-line rendering with its line breaks turned back into spaces, so the two
--   can never disagree about a shape, and with the location tags dropped, which
--   would only crowd a line that is already inside another one.
flatL1 :: L1Form -> String
flatL1 = unwords . map (dropTag . dropWhile (== ' ')) . l1 0

-- | Drop a leading location tag, if there is one.
dropTag :: String -> String
dropTag s = case s of
  ('[' : rest) | (_, ']' : more) <- break (== ']') rest -> dropWhile (== ' ') more
  _                                                     -> s

-- | @(lambda <param> <body>)@ and @(forall (<id>...) <body>)@.
lambda :: Int -> L1Lambda L1Form -> [String]
lambda d l = case l of
  L1Lambda p b -> block "lambda" (lamParamText p) b
  L1Forall p b -> block "forall" (lamParamText p) b
  where
    block w ps b = [pad d ++ "(" ++ w ++ " " ++ ps] ++ l1 (d + 1) b ++ [pad d ++ ")"]

lamParamText :: L1LamParam -> String
lamParamText (L1ParamMatcher i) = nameText i
lamParamText (L1ParamList   is) = "(" ++ unwords (map nameText is) ++ ")"

-- | The signatures, all of which are @(<word> <param> <body>)@ except the two
--   constraint forms, whose group comes first.
signature :: Int -> L1Sig L1Form -> [String]
signature d s = case s of
  L1Function sp b   -> sigWord "function" sp b
  L1Macro sp b      -> sigWord "macro" sp b
  L1Type sp b       -> sigWord "type" sp b
  L1Class sp b      -> sigWord "class" sp b
  L1Module sp b     -> sigWord "module" sp b
  L1Constraint bs b -> cstWord "constraint" bs b
  L1Include bs b    -> cstWord "include" bs b
  where
    sigWord w sp b = [pad d ++ "(" ++ w ++ " " ++ sigParamText sp]
                     ++ l1 (d + 1) b ++ [pad d ++ ")"]
    cstWord w bs b = [pad d ++ "(" ++ w]
                     ++ concatMap (cstBinding (d + 1)) bs ++ l1 (d + 1) b ++ [pad d ++ ")"]

-- | A signature parameter: a matcher is the type itself, a list the documented
--   @(:Self ...)@ spelling.
sigParamText :: L1SigParam L1Form -> String
sigParamText (L1SigMatcher x) = flatL1 x
sigParamText (L1SigList   xs) = "(:Self " ++ unwords (map flatL1 xs) ++ ")"

-- | One constraint of a group: @(sup a b)@ for @include@, @(cls a b)@ for
--   @constraint@, which is where the two differ.
cstBinding :: Int -> L1CstBinding L1Form -> [String]
cstBinding d cb = case cb of
  L1SupBinding a b -> group d "(sup" l1 [a, b]
  L1ClsBinding a b -> group d "(cls" l1 [a, b]

moduleForm :: Int -> L1Module L1Form -> [String]
moduleForm d m = case m of
  L1Import x      -> [pad d ++ "(import " ++ flatL1 x ++ ")"]
  L1Export i      -> [pad d ++ "(export " ++ nameText i ++ ")"]
  L1Provide a b c -> [pad d ++ "(provide " ++ unwords (map nameText [a, b, c]) ++ ")"]
  L1Require i     -> [pad d ++ "(require " ++ nameText i ++ ")"]

construct :: Int -> L1Construct L1Form -> [String]
construct d c = case c of
  L1Record bs       -> group d "(record" typBinder bs
  L1Enumerator bs   -> group d "(enum" typBinder bs
  L1Tuple xs        -> group d "(tuple" l1 xs
  L1Mu l            -> [pad d ++ "(mu"] ++ lambda (d + 1) l ++ [pad d ++ ")"]
  L1Fix l           -> [pad d ++ "(fix"] ++ lambda (d + 1) l ++ [pad d ++ ")"]
  L1Syntax x        -> group d "(syntax" l1 [x]
  L1Quote x         -> group d "(quote" l1 [x]
  L1List xs         -> group d "(list" l1 xs
  L1QuasiQuote x    -> group d "(quasiquote" l1 [x]
  L1Splice x        -> group d "(splice" l1 [x]
  L1Unquote x       -> group d "(unquote" l1 [x]
  L1Invoke a b      -> group d "(invoke" l1 [a, b]

-- | One @(: name type)@, @(:= name value)@ or @(:include record)@ entry.
typBinder :: Int -> L1TypBinder L1Form -> [String]
typBinder d b = case b of
  L1RecInc x        -> group d "(:include" l1 [x]
  L1TypBinding i t  -> [pad d ++ "(: " ++ nameText i] ++ l1 (d + 1) t ++ [pad d ++ ")"]
  L1ValBinding i v  -> [pad d ++ "(:= " ++ nameText i] ++ l1 (d + 1) v ++ [pad d ++ ")"]

matchForm :: Int -> L1Match L1Form -> [String]
matchForm d m = case m of
  L1Match s arms ->
    [pad d ++ "(match"] ++ l1 (d + 1) s ++ concatMap (arm (d + 1)) arms ++ [pad d ++ ")"]
  L1Case s lits arms ->
    [pad d ++ "(case"] ++ l1 (d + 1) s
      ++ [pad (d + 1) ++ "(" ++ unwords (map nameText lits) ++ ")"]
      ++ concatMap (arm (d + 1)) arms ++ [pad d ++ ")"]

-- | @(<pattern> <body>)@.
arm :: Int -> L1MatchArm L1Form -> [String]
arm d (L1PmArm p b) = [pad d ++ "("] ++ pat (d + 1) p ++ l1 (d + 1) b ++ [pad d ++ ")"]

-- | The pattern of an arm.  Its child slots are typed as forms rather than as
--   patterns, so a nested pattern is shown the way it is stored: as a form.
pat :: Int -> L1MatchPattern L1Form -> [String]
pat d p = case p of
  L1PmOr xs        -> group d "(:or" l1 xs
  L1PmAnd xs       -> group d "(:and" l1 xs
  L1PmLit v        -> [pad d ++ valueText v]
  L1PmWildcard     -> [pad d ++ "_"]
  L1PmBinder x     -> l1 d x
  L1PmCtor h ps    -> [pad d ++ "(" ++ flatL1 h] ++ concatMap (l1 (d + 1)) ps ++ [pad d ++ ")"]
  L1PmGuard p' c   -> [pad d ++ "(:guard"] ++ l1 (d + 1) p' ++ l1 (d + 1) c ++ [pad d ++ ")"]
  L1PmAs n p'      -> [pad d ++ "(:as " ++ flatL1 n] ++ l1 (d + 1) p' ++ [pad d ++ ")"]
  L1PmTuple xs     -> group d "(" l1 xs

letFamily :: Int -> L1LetFamily L1Form -> [String]
letFamily d fam = case fam of
  L1Let       bs b -> block "let" bs b
  L1LetSeq    bs b -> block "let:seq" bs b
  L1LetRec    bs b -> block "let:rec" bs b
  L1LetSeqRec bs b -> block "let:seqrec" bs b
  where
    block w bs b = [pad d ++ "(" ++ w]
                   ++ concatMap (letBinding (d + 1)) bs ++ l1 (d + 1) b ++ [pad d ++ ")"]

-- | @(: name type value)@.
letBinding :: Int -> L1LetBinding L1Form -> [String]
letBinding d (L1NamBinding i ty v) =
  [pad d ++ "(: " ++ nameText i] ++ l1 (d + 1) ty ++ l1 (d + 1) v ++ [pad d ++ ")"]

cont :: Int -> L1DlCont1 L1Form -> [String]
cont d c = case c of
  L1Reset1 x -> group d "(reset" l1 [x]
  L1Shift1 l -> [pad d ++ "(shift"] ++ lambda (d + 1) l ++ [pad d ++ ")"]

-- | @(#:name value)@, or the value alone when the argument is unnamed.
arg :: Int -> L1Argument L1Form -> [String]
arg d (L1Argument mi x) =
  [pad d ++ "(" ++ maybe "" (("#:" ++) . nameText) mi] ++ l1 (d + 1) x ++ [pad d ++ ")"]

-- * Leaves

-- | A name with its identity: @n#3@ for a reference, @'n#3@ for a name as data.
nameText :: L1Identifier -> String
nameText (L1Identifier n i) = n ++ "#" ++ show i
nameText (L1Symbol     n i) = "'" ++ n ++ "#" ++ show i

valueText :: Value -> String
valueText v = case v of
  ViSymbol i      -> nameText i
  ViInteger n     -> show n
  ViReal r        -> show r
  ViBoolean b     -> if b then "#true" else "#false"
  ViCharacter c   -> "#\\" ++ show c
  ViString s      -> show s
  ViUnit          -> "()"
  ViSexp s        -> sexpText s

-- | A quoted datum on one line.  The location nodes a datum carries are dropped
--   here: they are what the enclosing rendering shows everywhere else, and they
--   would drown a datum this small.
sexpText :: Sexp Value -> String
sexpText s = case s of
  EsLiteral v -> valueText v
  EsList xs   -> "(" ++ unwords (map sexpText xs) ++ ")"
  EsSeq xs    -> "<seq " ++ unwords (map sexpText xs) ++ ">"
  EsLoc _ x   -> sexpText x
  EsErr e     -> "#err " ++ show e

typesText :: L1Types -> String
typesText t = case t of
  L1Integer     -> "#int"
  L1Real        -> "#real"
  L1Char        -> "#char"
  L1Boolean     -> "#bool"
  L1Unit        -> "#unit"
  L1SynObj      -> "#syn"
  L1TypeKind n  -> "(#type " ++ show n ++ ")"

-- * Layout

-- | An indented group with both delimiters spelled out.
bracket :: Int -> String -> String -> (Int -> a -> [String]) -> [a] -> [String]
bracket d open close f xs = [pad d ++ open] ++ concatMap (f (d + 1)) xs ++ [pad d ++ close]

-- | An indented group: its opening line, its children, its closing line.
group :: Int -> String -> (Int -> a -> [String]) -> [a] -> [String]
group d w = bracket d w ")"

pad :: Int -> String
pad d = replicate (2 * d) ' '

-- | Put a location tag on the first line of a rendered node.  The tag is the
--   bracketed @[<line>:<startCol>-<endCol>]@ field; it is emitted unpadded here
--   and widened to a common width by 'align' once the whole rendering is known.
tag :: Span -> [String] -> [String]
tag sp ls = case ls of
  (l : rest) -> (tagText sp ++ " " ++ l) : rest
  []         -> [tagText sp ++ " "]

-- | The bracketless tag of a location.
tagContent :: Span -> String
tagContent sp = show (ln sp) ++ ":" ++ show (col sp) ++ "-" ++ show endCol
  where
    -- The last column the node covers, @col + len - 1@.  A zero-length node --
    -- the synthetic missing-closer marker -- keeps its start column.
    endCol = max (col sp) (col sp + len sp - 1)

tagText :: Span -> String
tagText sp = "[" ++ tagContent sp ++ "]"

-- | Right-align every leading location tag to one width, so that the tags of a
--   whole rendering line up in a column.  Only line-leading tags are touched --
--   the sole place 'tag' puts one.
align :: [String] -> [String]
align ls = map widen ls
  where
    w = maximum (0 : [length c | Just (c, _) <- map splitTag ls])
    widen l = case splitTag l of
      Just (c, r) -> "[" ++ replicate (w - length c) ' ' ++ c ++ "] " ++ r
      Nothing     -> l

-- | Split a line-leading tag into its content, @<line>:<startCol>-<endCol>@, and
--   the rest of the line.  'Nothing' when the line does not begin with one.
splitTag :: String -> Maybe (String, String)
splitTag ('[' : s) = case break (== ']') s of
  (c, ']' : ' ' : r) | not (null c), all isTagChar c -> Just (c, r)
  _                                                  -> Nothing
splitTag _ = Nothing

isTagChar :: Char -> Bool
isTagChar c = isDigit c || c == ':' || c == '-'
