{-| Layer 0 -> 1: name resolution.

    'resolve' turns a parsed unit ('L0p') into the L1 form language ('L1Form'),
    giving every identifier occurrence the identity of the binding it refers to.
    A declared name -- the binder of a @define@, @let@, @lambda@, @mu@, record
    field or pattern -- becomes an 'L1Identifier' with a fresh identity, and every
    use of a name becomes a @ViSymbol@ carrying that same identity, or, when nothing
    in scope binds it, the identity interned for that name over the whole unit.  Two
    occurrences therefore denote the same thing exactly when their 'Integer's are
    equal, and distinct binders never share one.

    The environment is this pass's own state and is never stored in the tree: the
    tree already carries scoping, since every binder list is a node, so a later pass
    rebuilds the environment by walking the result.

    Total, like the reader: input it could not lex arrives as 'EsErr' and leaves as
    'L1Err', and a form of the wrong shape leaves as 'L1Err' too, so the output
    always mirrors the shape of the input.
-}
module T01 where

import L0
import L1
import Shared

-- * Identities

-- | Which namespace a name lives in.
data Kind
  = KValue
  | KType
  | KCls
  | KMacro
  | KModule
  | KFree     -- ^ a name only ever used, never bound in this unit
  deriving (Show, Eq)

-- | A binding: the identity a binder shares with every use of it.
data Bnd = Bnd
  { bndId     :: Integer
  , bndKind   :: Kind
  , bndParams :: Maybe [L1Identifier]   -- ^ parameters, when the bound value is a lambda
  }

type Frame = [(String, Bnd)]

-- * Pass state

data St = St
  { stNext   :: Integer               -- ^ the one identity supply
  , stFrames :: [Frame]               -- ^ scope stack, innermost first, unit frame last
  , stSyms   :: [(String, Integer)]   -- ^ interned @#:name@ identities
  , stLits   :: [L1Identifier]        -- ^ the enclosing @case@'s literals
  }

newtype Res a = Res { unRes :: St -> (a, St) }

instance Functor Res where
  fmap f (Res g) = Res $ \s -> let (a, s') = g s in (f a, s')

instance Applicative Res where
  pure a = Res $ \s -> (a, s)
  Res f <*> Res g = Res $ \s ->
    let (h, s1) = f s
        (a, s2) = g s1
    in (h a, s2)

instance Monad Res where
  Res g >>= k = Res $ \s -> let (a, s1) = g s in unRes (k a) s1

getSt :: Res St
getSt = Res $ \s -> (s, s)

freshId :: Res Integer
freshId = Res $ \s -> (stNext s, s { stNext = stNext s + 1 })

newBnd :: Kind -> Res Bnd
newBnd kd = Bnd <$> freshId <*> pure kd <*> pure Nothing

-- * Environment

-- | Nearest binding for @n@, innermost frame outward.  First hit winning is what
--   shadowing means here.
lookupName :: String -> Res (Maybe Bnd)
lookupName n = do
  s <- getSt
  pure (nearest n (stFrames s))
  where
    nearest _ []       = Nothing
    nearest m (f : fs) = case lookup m f of
      Just b  -> Just b
      Nothing -> nearest m fs

-- | The binding with the given identity, wherever it sits.
lookupId :: Integer -> Res (Maybe Bnd)
lookupId i = do
  s <- getSt
  pure $ case [b | b <- concatMap (map snd) (stFrames s), bndId b == i] of
    (b : _) -> Just b
    []      -> Nothing

-- | The unit frame, the outermost one, where top-level definitions and interned
--   names live.
lookupUnit :: String -> Res (Maybe Integer)
lookupUnit n = do
  s <- getSt
  pure $ case reverse (stFrames s) of
    []      -> Nothing
    (u : _) -> bndId <$> lookup n u

-- | Run @k@ under one more frame holding @bs@, restoring the stack afterwards.
under :: [(String, Bnd)] -> Res a -> Res a
under bs k = Res $ \s ->
  let (a, s') = unRes k s { stFrames = bs : stFrames s }
  in (a, s' { stFrames = stFrames s })

-- | Run @k@ with @ls@ added to the enclosing @case@'s literals.  A name a case
--   lists as a literal is never a pattern variable there.
withLits :: [L1Identifier] -> Res a -> Res a
withLits ls k = Res $ \s ->
  let (a, s') = unRes k s { stLits = ls ++ stLits s }
  in (a, s' { stLits = stLits s })

-- | The enclosing @case@ literal of that name, if one is listed.
litOf :: String -> Res (Maybe L1Identifier)
litOf n = do
  s <- getSt
  pure $ case [i | i <- stLits s, idName i == n] of
    (i : _) -> Just i
    []      -> Nothing

-- | Declare @n@ in the innermost frame with a fresh identity, shadowing any earlier
--   declaration of that name in the same scope.
bindLocal :: String -> Kind -> Res Bnd
bindLocal n kd = do
  b <- newBnd kd
  pushLocal n b
  pure b

pushLocal :: String -> Bnd -> Res ()
pushLocal n b = Res $ \s -> case stFrames s of
  []       -> ((), s { stFrames = [[(n, b)]] })
  (f : fs) -> ((), s { stFrames = ((n, b) : f) : fs })

-- | Bind @n@ in the innermost frame, reusing a binding already there for that name.
--   Only the pre-collected top-level definitions need the reuse: it is what gives a
--   definition one identity over the whole unit.
declare :: String -> Kind -> Res Bnd
declare n kd = do
  s <- getSt
  case stFrames s of
    (f : _) | Just b <- lookup n f -> pure b
    _                              -> bindLocal n kd

-- | Record parameters on the binding with the given identity.
setParams :: Integer -> [L1Identifier] -> Res ()
setParams i ps = Res $ \s -> ((), s { stFrames = map (map tag) (stFrames s) })
  where
    tag (n, b) = (n, if bndId b == i then b { bndParams = Just ps } else b)

-- | Identity of a name used as a reference: the binder in scope, or the identity
--   interned for that name over the unit when nothing in scope binds it.
internRef :: String -> Res Integer
internRef n = do
  mb <- lookupName n
  case mb of
    Just b  -> pure (bndId b)
    Nothing -> do
      i <- freshId
      addUnit n (Bnd i KFree Nothing)
      pure i

-- | Identity of a name used as data.  No scope is consulted, so a quoted @n@ is a
--   different identifier from a bound @n@.
internData :: String -> Res Integer
internData n = do
  mb <- lookupUnit n
  case mb of
    Just i  -> pure i
    Nothing -> do
      i <- freshId
      addUnit n (Bnd i KFree Nothing)
      pure i

-- | Identity of a @#:name@.  Syntax symbols share one flat namespace over the whole
--   unit: equal names are the same thing.
internSym :: String -> Res Integer
internSym n = Res $ \s -> case lookup n (stSyms s) of
  Just i  -> (i, s)
  Nothing -> (stNext s, s { stNext = stNext s + 1, stSyms = (n, stNext s) : stSyms s })

-- | Add a binding to the unit frame, creating it if there is none.
addUnit :: String -> Bnd -> Res ()
addUnit n b = Res $ \s -> case reverse (stFrames s) of
  []         -> ((), s { stFrames = [[(n, b)]] })
  (u : rest) -> ((), s { stFrames = reverse (((n, b) : u) : rest) })

-- * Entry point

-- | Resolve one source unit.
resolve :: L0p -> L1Form
resolve x = fst (unRes (unit x) (St 0 [] [] []))

-- | Every top-level @define@ is collected before anything else is resolved, so
--   definitions see each other whatever their order, and a definition can call
--   itself.
unit :: L0p -> Res L1Form
unit x = do
  bs <- mapM (\ (n, kd) -> do b <- newBnd kd; pure (n, b)) (topDefs x)
  under bs (form x)

-- | The @(define <name> <type> <value>)@ heads at the top of a unit, with the
--   namespace their signature puts them in.  The first definition of a name wins.
topDefs :: L0p -> [(String, Kind)]
topDefs x = collect (topForms x)
  where
    collect [] = []
    collect (y : ys) =
      let rest = collect ys
      in case defHead y of
        Just (n, kd)
          | n `elem` map fst rest -> rest
          | otherwise             -> (n, kd) : rest
        Nothing                   -> rest

-- | The forms of a unit: the members of its top-level sequence, or the node itself.
topForms :: L0p -> [L0p]
topForms x = case unwrap x of
  EsSeq ys -> ys
  y        -> [y]

defHead :: L0p -> Maybe (String, Kind)
defHead x = case unwrap x of
  EsList (h : n : ty : _) | Just "define" <- symName h
    -> (\ nm -> (nm, sigKind ty)) <$> symName n
  _ -> Nothing

-- | The namespace a @define@ binds in, read off its signature.
sigKind :: L0p -> Kind
sigKind x = case unwrap x of
  EsList (h : _) -> case symName h of
    Just "macro"  -> KMacro
    Just "type"   -> KType
    Just "class"  -> KCls
    Just "module" -> KModule
    _             -> KValue
  _ -> KValue

-- * Nodes

form :: L0p -> Res L1Form
form (EsLiteral l) = literal l
form (EsList xs)   = formList xs
form (EsSeq xs)    = Fl1Common . L1Program <$> mapM form xs
form (EsLoc sp x)  = do x' <- form x; pure (Fl1Common (L1Loc x' sp))
form (EsErr e)     = pure (Fl1Common (L1Err e))

-- | A list is a known form when its head is one of the words in 'forms', and an
--   application otherwise.  @()@ is the unit value.
formList :: [L0p] -> Res L1Form
formList []         = value ViUnit
formList xs@(h : _) = case symName h of
  Just w | Just f <- lookup w forms -> f xs
  _                                 -> application xs

-- * Literals

literal :: Literal -> Res L1Form
literal (LiSymbol n) = case typeKeyword n of
  Just t  -> pure (Fl1Types t)
  Nothing -> do i <- internRef n; pure (symForm n i)
literal (LiSyntaxSym _) = pure (Fl1Common (L1Err EeUnexpectedSynSym))
literal l = case literalValue l of
  Just v  -> value v
  Nothing -> bad

-- | The literals that are values outright.
literalValue :: Literal -> Maybe Value
literalValue (LiInteger i)   = Just (ViInteger i)
literalValue (LiReal r)      = Just (ViReal r)
literalValue (LiBoolean b)   = Just (ViBoolean b)
literalValue (LiCharacter c) = Just (ViCharacter c)
literalValue (LiString s)    = Just (ViString s)
literalValue _               = Nothing

-- | The type constructors of 'L1Types', spelled as 'L1Types' names them.
typeKeyword :: String -> Maybe L1Types
typeKeyword "#int"  = Just L1Integer
typeKeyword "#real" = Just L1Real
typeKeyword "#char" = Just L1Char
typeKeyword "#bool" = Just L1Boolean
typeKeyword "#unit" = Just L1Unit
typeKeyword "#syn"  = Just L1SynObj
typeKeyword _       = Nothing

-- | A literal value in form position.
value :: Value -> Res L1Form
value v = pure (Fl1Common (L1Literal v))

-- | A reference to a name.
symForm :: String -> Integer -> L1Form
symForm n i = Fl1Common (L1Literal (ViSymbol (L1Identifier n i)))

-- | A name as data.
datSym :: String -> Integer -> Value
datSym n i = ViSymbol (L1Symbol n i)

idName :: L1Identifier -> String
idName (L1Identifier n _) = n
idName (L1Symbol     n _) = n

idOf :: L1Identifier -> Integer
idOf (L1Identifier _ i) = i
idOf (L1Symbol     _ i) = i

-- | The error form for a shape or arity failure.
errForm :: L1Form
errForm = Fl1Common (L1Err EeInvalidForm)

bad :: Res L1Form
bad = pure errForm

-- * Application

application :: [L0p] -> Res L1Form
application (f : as) = do
  f'  <- form f
  ps  <- calleeParams f'
  as' <- arguments ps as
  pure (Fl1Apply (L1Apply f' as'))
application [] = value ViUnit

-- | The parameters of the form being called, when the call site alone tells them:
--   a lambda, or a name bound to one.  'Nothing' means the callee is unknown, and
--   then a named argument is left unchecked rather than guessed at.
calleeParams :: L1Form -> Res (Maybe [L1Identifier])
calleeParams f = case unwrapForm f of
  Fl1Lambda (L1Lambda p _)           -> pure (Just (paramIds p))
  Fl1Common (L1Literal (ViSymbol i)) -> maybe Nothing bndParams <$> lookupId (idOf i)
  _                                  -> pure Nothing

-- | The identifiers a parameter form binds.
paramIds :: L1LamParam -> [L1Identifier]
paramIds (L1ParamMatcher i) = [i]
paramIds (L1ParamList   is) = is

-- | The argument list of an application.  A @#:name@ is read only here: it becomes
--   the identifier of the callee's parameter of that name.
arguments :: Maybe [L1Identifier] -> [L0p] -> Res [L1Argument L1Form]
arguments ps (n : v : rest) | Just nm <- synName n, Nothing <- synName v = do
  v'    <- form v
  rest' <- arguments ps rest
  case matchParam ps nm of
    Just p  -> pure (L1Argument (Just p) v' : rest')
    Nothing -> do
      i <- internSym nm
      case ps of
        Nothing -> pure (L1Argument (Just (L1Symbol nm i)) v' : rest')
        Just _  -> pure (L1Argument (Just (L1Symbol nm i))
                                   (Fl1Common (L1Err EeNoSuchParameter)) : rest')
arguments ps (n : rest) | Just nm <- synName n = do
  i     <- internSym nm
  rest' <- arguments ps rest
  pure (L1Argument (Just (L1Symbol nm i))
                   (Fl1Common (L1Err EeExpectArgument)) : rest')
arguments ps (v : rest) = do
  v'    <- form v
  rest' <- arguments ps rest
  pure (L1Argument Nothing v' : rest')
arguments _ [] = pure []

-- | The callee's parameter of that name, when the callee's parameters are known.
matchParam :: Maybe [L1Identifier] -> String -> Maybe L1Identifier
matchParam Nothing   _  = Nothing
matchParam (Just ps) nm = case [p | p <- ps, idName p == nm] of
  (p : _) -> Just p
  []      -> Nothing

-- | The name a @#:name@ spells.
synName :: L0p -> Maybe String
synName (EsLoc _ x)                 = synName x
synName (EsLiteral (LiSyntaxSym n)) = Just n
synName _                           = Nothing

-- * Definition

defineForm :: [L0p] -> Res L1Form
defineForm args = case args of
  [_, n, ty, val] -> case symName n of
    Nothing -> bad
    Just nm -> do
      b    <- declare nm (sigKind ty)
      ty'  <- form ty
      val' <- under [(nm, b)] (form val)
      recordParams (bndId b) val'
      pure (Fl1Define (L1Define (L1Identifier nm (bndId b)) ty' val'))
  _ -> bad

-- | Remember the parameters of a bound value when it is a lambda, so that a later
--   @(f #:x ...)@ can be checked against them.
recordParams :: Integer -> L1Form -> Res ()
recordParams i v = case unwrapForm v of
  Fl1Lambda (L1Lambda p _) -> setParams i (paramIds p)
  _                        -> pure ()

-- * Let

-- | Which names a @let@ variant makes visible to the binding being resolved.
data Vis = VLet | VSeq | VRec | VSeqRec

letForm :: Vis -> [L0p] -> Res L1Form
letForm vis args = case args of
  [_, def, body] -> case unwrap def of
    EsList bs -> case mapM binderParts bs of
      Nothing    -> bad
      Just parts -> do
        ds <- mapM (\ (nm, _, _) -> do d <- newBnd KValue; pure (nm, d)) parts
        vs <- sequence [ bind1 vis ds i (d, ty, val)
                       | (i, (d, (_, ty, val))) <- zip [0 ..] (zip ds parts) ]
        body' <- under ds (form body)
        pure (Fl1Let (family vis vs body'))
    _ -> bad
  _ -> bad

-- | The declared name and the two expressions of a @(: <name> <type> <value>)@
--   @let@ binder.
binderParts :: L0p -> Maybe (String, L0p, L0p)
binderParts b = case unwrap b of
  EsList [k, n, ty, val] | Just ":" <- symName k, Just nm <- symName n
    -> Just (nm, ty, val)
  _ -> Nothing

-- | One binding, resolved under the names its variant makes visible.
bind1 :: Vis -> [(String, Bnd)] -> Int -> ((String, Bnd), L0p, L0p)
      -> Res (L1LetBinding L1Form)
bind1 vis ds i ((nm, d), ty, val) = do
  ty'  <- under (visible vis ds i) (form ty)
  val' <- under (visible vis ds i) (form val)
  recordParams (bndId d) val'
  pure (L1NamBinding (L1Identifier nm (bndId d)) ty' val')

-- | The names a binding can see: none, the earlier ones, every one, or the earlier
--   ones and itself.
visible :: Vis -> [(String, Bnd)] -> Int -> [(String, Bnd)]
visible vis ds i = case vis of
  VLet    -> []
  VSeq    -> take i ds
  VRec    -> ds
  VSeqRec -> take (i + 1) ds

family :: Vis -> [L1LetBinding L1Form] -> L1Form -> L1LetFamily L1Form
family vis vs body = case vis of
  VLet    -> L1Let       vs body
  VSeq    -> L1LetSeq    vs body
  VRec    -> L1LetRec    vs body
  VSeqRec -> L1LetSeqRec vs body

-- * Lambda and forall

lambdaForm :: [L0p] -> Res L1Form
lambdaForm args = case args of
  _ : ps : body : _ -> Fl1Lambda <$> paramBody KValue ps body
  _                 -> bad

forallForm :: [L0p] -> Res L1Form
forallForm args = case args of
  _ : vs : body : _ -> do
    (p, bs) <- param KType vs
    body'   <- under bs (form body)
    pure (Fl1Lambda (L1Forall p body'))
  _ -> bad

-- | A lambda as a value, from the whole @(lambda <param> <body>)@ form.
lambdaOf :: Kind -> L0p -> Res (L1Lambda L1Form)
lambdaOf kd x = case unwrap x of
  EsList (w : ps : body : _) | Just "lambda" <- symName w -> paramBody kd ps body
  _ -> pure (L1Lambda (L1ParamList []) errForm)

paramBody :: Kind -> L0p -> L0p -> Res (L1Lambda L1Form)
paramBody kd ps body = do
  (p, bs) <- param kd ps
  L1Lambda p <$> under bs (form body)

-- | A parameter form: one identifier, which 'L1Lambda''s NOTE calls a matcher
--   because it receives the whole call expression, or a list of identifiers.
param :: Kind -> L0p -> Res (L1LamParam, [(String, Bnd)])
param kd x = case unwrap x of
  EsLiteral (LiSymbol n) -> do
    b <- newBnd kd
    pure (L1ParamMatcher (L1Identifier n (bndId b)), [(n, b)])
  EsList ys -> case mapM symName ys of
    Just ns -> do
      bs <- mapM (\ n -> do b <- newBnd kd; pure (n, b)) (dedup ns)
      pure (L1ParamList [L1Identifier n (bndId b) | (n, b) <- bs], bs)
    Nothing -> pure (L1ParamList [], [])
  _ -> pure (L1ParamList [], [])

-- * Recursive binders

muForm, fixForm :: [L0p] -> Res L1Form

muForm args = case args of
  [_, l] -> Fl1Construct . L1Mu <$> lambdaOf KType l
  _      -> bad

fixForm args = case args of
  [_, l] -> Fl1Construct . L1Fix <$> lambdaOf KValue l
  _      -> bad

-- * Branching

ifForm :: [L0p] -> Res L1Form
ifForm args = case args of
  [_, c, t, e] -> do
    c' <- form c
    t' <- form t
    e' <- form e
    pure (Fl1IfThenElse (L1IfThenElse c' t' e'))
  _ -> bad

-- | @(match <scrutinee> <arm>...)@ and @(case <scrutinee> (<lit>...) <arm>...)@.
matchForm :: Bool -> [L0p] -> Res L1Form
matchForm isCase args = case args of
  _ : scrut : rest -> do
    s' <- form scrut
    if not isCase
      then do
        as' <- mapM arm rest
        pure (Fl1Match (L1Match s' as'))
      else do
        let (ns, arms) = literals rest
        ls  <- mapM litId (dedup ns)
        as' <- withLits ls (mapM arm arms)
        pure (Fl1Match (L1Case s' ls as'))
  _ -> bad

-- | Split a @case@'s literal group from its arms.  The comment gives the shape
--   @(case <scrutinee> (<lit>...) (<pattern> <body>)...)@, so the element after the
--   scrutinee is the group when it is a list of names -- @()@ vacuously.
literals :: [L0p] -> ([String], [L0p])
literals (g : rest) = case unwrap g of
  EsList ys | Just ns <- mapM symName ys -> (ns, rest)
  _                                      -> ([], g : rest)
literals [] = ([], [])

-- | A @case@ literal: a fixed symbol, never a variable, so it is the unit-wide
--   identity of that name rather than a fresh one.
litId :: String -> Res L1Identifier
litId n = do
  i <- internData n
  pure (L1Symbol n i)

-- | @(<pattern> <body>)@: the body is resolved under the pattern's binders.
arm :: L0p -> Res (L1MatchArm L1Form)
arm x = case unwrap x of
  EsList [p, b] -> do
    (p', bs) <- patternOf p
    b'       <- under bs (form b)
    pure (L1PmArm p' b')
  _ -> pure (L1PmArm L1PmWildcard errForm)

-- * Patterns

-- | A pattern at the top of an arm, with the names it binds.
patternOf :: L0p -> Res (L1MatchPattern L1Form, [(String, Bnd)])
patternOf x = case unwrap x of
  EsLiteral (LiSymbol "_") -> pure (L1PmWildcard, [])
  EsLiteral (LiSymbol n)   -> do
    ml <- litOf n
    case ml of
      Just i  -> pure (L1PmLit (ViSymbol i), [])   -- a listed literal, not a variable
      Nothing -> do
        b <- newBnd KValue
        pure (L1PmBinder (symForm n (bndId b)), [(n, b)])
  EsLiteral (LiSyntaxSym _) -> pure (L1PmLit (ViSexp (EsErr EeUnexpectedSynSym)), [])
  EsLiteral l               -> case literalValue l of
    Just v  -> pure (L1PmLit v, [])
    Nothing -> pure (patBad, [])
  EsList ys                 -> patternList ys
  _                         -> pure (patBad, [])

patBad :: L1MatchPattern L1Form
patBad = L1PmLit (ViSexp (EsErr EeInvalidForm))

patternList :: [L0p] -> Res (L1MatchPattern L1Form, [(String, Bnd)])
patternList [] = pure (L1PmTuple [], [])
patternList xs@(h : rest) = case symName h of
  Just ":or"    -> alternatives L1PmOr rest
  Just ":and"   -> alternatives L1PmAnd rest
  Just ":as"    -> asPattern xs
  Just ":guard" -> guardPattern xs
  Just _        -> ctorPattern xs
  Nothing       -> tuplePattern xs

alternatives :: ([L1Form] -> L1MatchPattern L1Form) -> [L0p]
             -> Res (L1MatchPattern L1Form, [(String, Bnd)])
alternatives c xs = do
  ps <- mapM patForm xs
  pure (c (map fst ps), unions (map snd ps))

-- | @(:as <name> <pattern>)@.
asPattern :: [L0p] -> Res (L1MatchPattern L1Form, [(String, Bnd)])
asPattern args = case args of
  [_, n, p] -> case symName n of
    Just nm -> do
      b        <- newBnd KValue
      (p', bs) <- patForm p
      pure (L1PmAs (symForm nm (bndId b)) p', (nm, b) : bs)
    Nothing -> pure (patBad, [])
  _ -> pure (patBad, [])

-- | @(:guard <pattern> <cond>)@: the condition sees the pattern's binders.
guardPattern :: [L0p] -> Res (L1MatchPattern L1Form, [(String, Bnd)])
guardPattern args = case args of
  [_, p, c] -> do
    (p', bs) <- patForm p
    c'       <- under bs (form c)
    pure (L1PmGuard p' c', bs)
  _ -> pure (patBad, [])

-- | @(<ctor> <pattern> ...)@: the head is a use of the constructor.
ctorPattern :: [L0p] -> Res (L1MatchPattern L1Form, [(String, Bnd)])
ctorPattern (h : ps) = do
  h'  <- form h
  ps' <- mapM patForm ps
  pure (L1PmCtor h' (map fst ps'), unions (map snd ps'))
ctorPattern [] = pure (L1PmTuple [], [])

-- | A parenthesised pattern whose head is no name: a tuple of patterns.
tuplePattern :: [L0p] -> Res (L1MatchPattern L1Form, [(String, Bnd)])
tuplePattern xs = do
  ps <- mapM patForm xs
  pure (L1PmTuple (map fst ps), unions (map snd ps))

-- | A pattern in a slot typed as a form.  'L1MatchPattern' has no 'L1Form' wrapper,
--   so a nested pattern is read as its own head applied to its children.
patForm :: L0p -> Res (L1Form, [(String, Bnd)])
patForm x = case unwrap x of
  EsLiteral (LiSymbol "_") -> do
    i <- internData "_"
    pure (symForm "_" i, [])
  EsLiteral (LiSymbol n)   -> do
    ml <- litOf n
    case ml of
      Just i  -> pure (Fl1Common (L1Literal (ViSymbol i)), [])
      Nothing -> do
        b <- newBnd KValue
        pure (symForm n (bndId b), [(n, b)])
  EsLiteral (LiSyntaxSym _) -> pure (Fl1Common (L1Err EeUnexpectedSynSym), [])
  EsLiteral l               -> case literalValue l of
    Just v  -> do v' <- value v; pure (v', [])
    Nothing -> pure (errForm, [])
  EsList []                 -> do v' <- value ViUnit; pure (v', [])
  EsList (h : ys)           -> do
    h' <- form h
    ps <- mapM patForm ys
    pure (Fl1Apply (L1Apply h' [L1Argument Nothing v | (v, _) <- ps]), unions (map snd ps))
  _ -> pure (errForm, [])

-- | The binders of several subpatterns, first mention winning.
unions :: [[(String, Bnd)]] -> [(String, Bnd)]
unions = foldr step []
  where
    step bs acc = bs ++ [b | b <- acc, fst b `notElem` map fst bs]

-- * Signatures

sig2 :: (L1SigParam L1Form -> L1Form -> L1Sig L1Form) -> [L0p] -> Res L1Form
sig2 c args = case args of
  [_, a, b] -> do
    a' <- sigParamOf a
    b' <- form b
    pure (Fl1Signature (c a' b'))
  _ -> bad

-- | A signature form whose constraint group comes first: @constraint@ and
--   @include@, which differ only in how their bindings read.
sigList :: ([L1CstBinding L1Form] -> L1Form -> L1Sig L1Form)
        -> (L1Form -> L1Form -> L1CstBinding L1Form)
        -> [L0p] -> Res L1Form
sigList mk bnd args = case args of
  [_, cs, b] -> do
    cs' <- mapM (cstOf bnd) (elems cs)
    b'  <- form b
    pure (Fl1Signature (mk cs' b'))
  _ -> bad

-- | One constraint of such a group: a pair of types.  A constraint that is not a
--   pair leaves the second slot as an error.
cstOf :: (L1Form -> L1Form -> L1CstBinding L1Form) -> L0p
      -> Res (L1CstBinding L1Form)
cstOf bnd x = case unwrap x of
  EsList [a, b] -> do
    a' <- form a
    b' <- form b
    pure (bnd a' b')
  _ -> do
    a' <- form x
    pure (bnd a' errForm)

-- | A signature's first slot.  Only @(:Self t...)@ is a list of types; every
--   other shape, list or not, is a single type read as a matcher.
sigParamOf :: L0p -> Res (L1SigParam L1Form)
sigParamOf x = case unwrap x of
  EsList (h : rest) | Just ":Self" <- symName h -> L1SigList <$> mapM form rest
  _                                             -> L1SigMatcher <$> form x

-- * Constructs

recordForm, enumForm :: [L0p] -> Res L1Form

recordForm args = case args of
  _ : fs -> Fl1Construct . L1Record <$> mapM field fs
  _      -> bad

enumForm args = case args of
  _ : fs -> Fl1Construct . L1Enumerator <$> mapM field fs
  _      -> bad

-- | One entry of a record or enum: @(: <name> <type>)@, or @(:include <record>)@.
--   Whether an entry is a type binding or a value binding depends on the enclosing
--   signature, which this layer does not have, so it is read as a type binding; the
--   field name enters the enclosing scope so that later mentions share its identity.
field :: L0p -> Res (L1TypBinder L1Form)
field x = case unwrap x of
  EsList [k, n, t] | Just ":" <- symName k -> case symName n of
    Just nm -> do
      b  <- bindLocal nm KValue
      t' <- form t
      pure (L1TypBinding (L1Identifier nm (bndId b)) t')
    Nothing -> pure (L1RecInc errForm)
  EsList [k, r] | Just ":include" <- symName k -> L1RecInc <$> form r
  _ -> L1RecInc <$> form x

listForm :: [L0p] -> Res L1Form
listForm args = case args of
  _ : es -> Fl1Construct . L1List <$> mapM form es
  _      -> bad

invokeForm :: [L0p] -> Res L1Form
invokeForm args = case args of
  [_, a, b] -> do
    a' <- form a
    b' <- form b
    pure (Fl1Construct (L1Invoke a' b'))
  _ -> bad

-- | @(#type <level>)@.
typeKindForm :: [L0p] -> Res L1Form
typeKindForm args = case args of
  [_, n] -> case unwrap n of
    EsLiteral (LiInteger i) -> pure (Fl1Types (L1TypeKind i))
    _                       -> bad
  _ -> bad

-- | @(annotation (<meta> ...) <body>)@.
annotationForm :: [L0p] -> Res L1Form
annotationForm args = case args of
  [_, ms, b] -> do
    ms' <- mapM form (elems ms)
    b'  <- form b
    pure (Fl1Common (L1Annotation ms' b'))
  _ -> bad

-- * Delimited continuations

shiftForm :: [L0p] -> Res L1Form
shiftForm args = case args of
  [_, l] -> Fl1Cont1 . L1Shift1 <$> lambdaOf KValue l
  _      -> bad

-- * Modules

importForm :: [L0p] -> Res L1Form
importForm args = case args of
  [_, m] -> Fl1Module . L1Import <$> form m
  _      -> bad

-- | @(export <identifier> ...)@.  'L1Export' holds one identifier, so only the
--   first name is kept.
exportForm :: [L0p] -> Res L1Form
exportForm args = case args of
  _ : n : _ -> do
    mn <- named n
    case mn of
      Just i  -> pure (Fl1Module (L1Export i))
      Nothing -> bad
  _ -> bad

provideForm :: [L0p] -> Res L1Form
provideForm args = case args of
  [_, a, b, c] -> do
    m <- sequence [named a, named b, named c]
    case m of
      [Just x, Just y, Just z] -> pure (Fl1Module (L1Provide x y z))
      _                        -> bad
  _ -> bad

requireForm :: [L0p] -> Res L1Form
requireForm args = case args of
  [_, n] -> do
    mn <- named n
    case mn of
      Just i  -> pure (Fl1Module (L1Require i))
      Nothing -> bad
  _ -> bad

-- | A name in a declaration position: a fresh identity, entering the innermost
--   scope so that later mentions in it find the same identity.
named :: L0p -> Res (Maybe L1Identifier)
named x = case symName x of
  Just n  -> do b <- bindLocal n KValue; pure (Just (L1Identifier n (bndId b)))
  Nothing -> pure Nothing

-- * Quoting

-- | @(quote <datum>)@: the datum resolved as data, not as code.
quoteForm :: [L0p] -> Res L1Form
quoteForm args = case args of
  [_, x] -> Fl1Construct . L1Quote <$> datForm x
  _      -> bad

-- | Quoted data as a form: a literal value.
datForm :: L0p -> Res L1Form
datForm x = Fl1Common . L1Literal <$> dat x

-- | Quoted data.  A name becomes a symbol value -- 'L1Symbol', so that it is not
--   mistaken for a reference -- and a group a @Sexp@ of values.  Nothing is
--   resolved, and locations and errors survive inside the datum because 'Sexp'
--   carries them itself.
dat :: L0p -> Res Value
dat (EsLiteral (LiSymbol n))    = do i <- internData n; pure (datSym n i)
dat (EsLiteral (LiSyntaxSym _)) = pure (ViSexp (EsErr EeUnexpectedSynSym))
dat (EsLiteral l)               = pure (maybe (ViSexp (EsErr EeInvalidForm)) id
                                              (literalValue l))
dat (EsList [])                 = pure ViUnit
dat (EsList xs)                 = ViSexp . EsList <$> mapM datSexp xs
dat (EsSeq xs)                  = ViSexp . EsSeq <$> mapM datSexp xs
dat (EsLoc sp x)                = ViSexp . EsLoc sp <$> datSexp x
dat (EsErr e)                   = pure (ViSexp (EsErr e))

-- | The same, one node further in: a @Sexp@ of values, keeping the location and
--   error nodes the reader put there.
datSexp :: L0p -> Res (Sexp Value)
datSexp (EsLoc sp x)  = EsLoc sp <$> datSexp x
datSexp (EsList [])   = pure (EsLiteral ViUnit)
datSexp (EsList ys)   = EsList <$> mapM datSexp ys
datSexp (EsSeq ys)    = EsSeq  <$> mapM datSexp ys
datSexp (EsErr e)     = pure (EsErr e)
datSexp (EsLiteral l) = EsLiteral <$> dat (EsLiteral l)

-- | @(quasiquote <template>)@: quoted, except that @unquote@ and @splice@ escape
--   back to the language.
quasi :: L0p -> Res L1Form
quasi x = case unwrap x of
  EsList [h, e] | Just w <- symName h, w == "unquote" -> Fl1Construct . L1Unquote <$> form e
  EsList [h, e] | Just w <- symName h, w == "splice"  -> Fl1Construct . L1Splice  <$> form e
  EsList ys -> do
    ys' <- mapM quasi ys
    pure (Fl1Construct (L1List ys'))
  _ -> Fl1Common . L1Literal <$> dat x

-- | A form of exactly one subform.
unary :: (L1Form -> L1Form) -> (L0p -> Res L1Form) -> [L0p] -> Res L1Form
unary mk sub args = case args of
  [_, x] -> mk <$> sub x
  _      -> bad

-- * Form table

-- | Every form this layer knows, by head word.  A word absent from the table is an
--   application: the loop, branching, pipe and sequence families are macro
--   definitions, so they are the expander's to rewrite, not this pass's.
forms :: [(String, [L0p] -> Res L1Form)]
forms =
  [ ("define",     defineForm)
  , ("let",        letForm VLet)
  , ("let:seq",    letForm VSeq)
  , ("let:rec",    letForm VRec)
  , ("let:seqrec", letForm VSeqRec)
  , ("lambda",     lambdaForm)
  , ("forall",     forallForm)
  , ("mu",         muForm)
  , ("fix",        fixForm)
  , ("if",         ifForm)
  , ("match",      matchForm False)
  , ("case",       matchForm True)
  , ("function",   sig2 L1Function)
  , ("macro",      sig2 L1Macro)
  , ("type",       sig2 L1Type)
  , ("class",      sig2 L1Class)
  , ("module",     sig2 L1Module)
  , ("constraint", sigList L1Constraint L1ClsBinding)
  , ("include",    sigList L1Include    L1SupBinding)
  , ("record",     recordForm)
  , ("enum",       enumForm)
  , ("tuple",      tupleForm)
  , ("invoke",     invokeForm)
  , ("#type",      typeKindForm)
  , ("quote",      quoteForm)
  , ("quasiquote", unary (Fl1Construct . L1QuasiQuote) quasi)
  , ("list",       listForm)
  , ("syntax",     unary (Fl1Construct . L1Syntax) form)
  , ("splice",     unary (Fl1Construct . L1Splice) form)
  , ("unquote",    unary (Fl1Construct . L1Unquote) form)
  , ("reset",      unary (Fl1Cont1 . L1Reset1) form)
  , ("shift",      shiftForm)
  , ("annotation", annotationForm)
  , ("import",     importForm)
  , ("export",     exportForm)
  , ("provide",    provideForm)
  , ("require",    requireForm)
  ]

tupleForm :: [L0p] -> Res L1Form
tupleForm args = case args of
  _ : ts -> Fl1Construct . L1Tuple <$> mapM form ts
  _      -> bad

-- * Syntax helpers

-- | Drop the location wrapper.
unwrap :: L0p -> L0p
unwrap (EsLoc _ x) = unwrap x
unwrap x           = x

-- | Drop the location wrappers of a resolved form.
unwrapForm :: L1Form -> L1Form
unwrapForm (Fl1Common (L1Loc x _)) = unwrapForm x
unwrapForm x                       = x

-- | The name an occurrence spells, when it is an ordinary identifier.
symName :: L0p -> Maybe String
symName (EsLoc _ x)              = symName x
symName (EsLiteral (LiSymbol n)) = Just n
symName _                        = Nothing

-- | The subforms of a bracketed group, or the argument itself.
elems :: L0p -> [L0p]
elems x = case unwrap x of
  EsSeq ys  -> ys
  EsList ys -> ys
  _         -> [x]

-- | Drop repeated names, keeping the first.
dedup :: [String] -> [String]
dedup = go []
  where
    go _ [] = []
    go seen (n : ns)
      | n `elem` seen = go seen ns
      | otherwise     = n : go (n : seen) ns
