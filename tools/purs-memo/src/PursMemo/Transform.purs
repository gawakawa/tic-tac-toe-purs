-- | The transform: for every `React.do` block, lift eligible `let` bindings
-- | (and, when eligible, the final `pure` tail) into `useMemo` binds.
-- |
-- | Amendments to the design in issue #7 (see the issue / plan for the why):
-- |   * Scope is positional — a binding's free variables are intersected
-- |     with names bound strictly before it (plus, within one `let` group,
-- |     its siblings — a `let` group is simultaneously scoped, same as
-- |     Haskell), never with names bound by a later `DoStatement`. Closes
-- |     both a build break and a silent-staleness bug a flat, whole-scope
-- |     name intersection admits.
-- |   * Record field labels are excluded from free-variable collection
-- |     structurally (see `PursMemo.Scope`), not by position — labels and
-- |     idents lex identically and no position-based fix helps that.
-- |   * Only `DoStatement`s directly under `React.do` are ever touched; the
-- |     transform never recurses into a nested `Effect` do block looking for
-- |     more `DoLet`s.
-- |   * never-hits pruning and a cost floor replace "memoize everything": a
-- |     binding whose dependency closure already covers every unstable state
-- |     variable in scope can never hit, and a binding with neither a lambda
-- |     nor a JSX constructor in its body costs more to memoize than to
-- |     recompute. Both fold into one "stays plain" outcome that propagates
-- |     through the dependency graph exactly like ineligibility
-- |     (guards/where, a polymorphic signature, or an unsafe/Debug mention)
-- |     does — a binding that depends on a plain (unmemoized) binding can
-- |     never hit either, so it never gets memoized.
module PursMemo.Transform
  ( Options
  , defaultOptions
  , transformModule
  ) where

import Prelude

import Data.Array
  ( all
  , any
  , elem
  , filter
  , foldMap
  , mapMaybe
  , notElem
  , nub
  , null
  )
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import PureScript.CST.Traversal (defaultVisitor, rewriteModuleTopDown)
import PureScript.CST.Types
  ( AppSpine(..)
  , Binder
  , DoBlock
  , DoStatement(..)
  , Expr(..)
  , Guarded(..)
  , Labeled(..)
  , LetBinding(..)
  , Module
  , ModuleName(..)
  , Name(..)
  , QualifiedName(..)
  , SourceToken
  , Token(..)
  , Type
  , ValueBindingFields
  , Where(..)
  )
import PursMemo.Scope (boundNames, identsIn)
import PursMemo.Stability (Stability(..), anyToken, classifyDoBind, hasForallOrConstraint, mentionsUnsafeOrDebug)
import Tidy.Codegen (binaryOp, binderVar, doLet, exprCtor, exprIdent) as RawCodegen
import Tidy.Codegen (binderWildcard, doBind, exprApp, exprLambda, exprOp)
import Tidy.Codegen.Types (BinaryOp)

-- | `exprIdent`/`exprCtor`/`binderVar`/`binaryOp` all need `Partial` when
-- | given a `String` name (the name is lexed at codegen time and could, in
-- | principle, fail to lex as an identifier/operator — it never does for the
-- | names this transform generates). Wrapping once here, rather than at
-- | every call site, keeps the constraint out of every signature below.
ident :: forall e. String -> Expr e
ident = unsafePartial RawCodegen.exprIdent

ctor :: forall e. String -> Expr e
ctor = unsafePartial RawCodegen.exprCtor

var :: forall e. String -> Binder e
var = unsafePartial RawCodegen.binderVar

op :: forall e. String -> Expr e -> BinaryOp (Expr e)
op = unsafePartial RawCodegen.binaryOp

-- | Wrap a non-empty group of original `LetBinding` nodes (reused verbatim
-- | — a residual binding is never rewritten, only re-grouped) back into one
-- | `let` statement. Always non-empty by construction (every `Entry`
-- | contributes at least one `LetBinding`), hence `unsafePartial`.
letGroup :: Array (LetBinding Void) -> DoStatement Void
letGroup = unsafePartial RawCodegen.doLet

type Options = { prune :: Boolean }

defaultOptions :: Options
defaultOptions = { prune: true }

-- | Rewrite every `React.do` block in the module. `React.do` is matched on
-- | the enclosing lambda so its parameters are available as render scope
-- | (`component "Game" \props -> React.do ...`).
transformModule :: Options -> Module Void -> Module Void
transformModule opts = rewriteModuleTopDown (defaultVisitor { onExpr = onExpr })
  where
  onExpr = case _ of
    ExprLambda lam@{ body: ExprDo db }
      | Just alias <- reactDoAlias db.keyword ->
          ExprLambda lam
            { body = ExprDo
                ( transformDoBlock opts alias
                    (foldMap boundNames (NEA.toArray lam.binders))
                    db
                )
            }
    other -> other

-- | `React.do`'s `do` keyword lexes as a single qualified token; the module
-- | alias it carries is exactly the alias `useMemo`/`UnsafeReference` are
-- | reachable through, so no import patching is ever needed.
reactDoAlias :: SourceToken -> Maybe String
reactDoAlias tok = case tok.value of
  TokLowerName (Just (ModuleName "React")) "do" -> Just "React"
  _ -> Nothing

-- | What is known about a render-scope name once it has been processed.
-- | `Unstable` covers both raw hook state (from `stateNames`) and any plain
-- | (unmemoized) let-binding — both change reference every render, so both
-- | propagate their `unstableClosure` to whoever depends on them.
-- | `MemoizedResult` is a name this transform itself bound via `useMemo`: it
-- | contributes nothing further to a closure (it is now stabilized), but
-- | unlike the hook-table stable categories it is still kept (wrapped) in a
-- | downstream key, since its own memo can still miss.
type Info = { stability :: Stability, unstableClosure :: Array String }
type Known = Array (Tuple String Info)

-- | The transform's decision for one candidate binding, plus — when it
-- | stays plain — the `Info` a downstream binding sees. Computed together
-- | so the shared deps/closure derivation happens exactly once instead of
-- | being redone by a separate "and what if not" pass.
data Verdict = Memoize (Array String) | StaysPlain Info

infoOf :: String -> Known -> Info
infoOf n known = fromMaybe { stability: Unstable, unstableClosure: [ n ] }
  (Array.findMap (\(Tuple k i) -> if k == n then Just i else Nothing) known)

isSubsetOf :: Array String -> Array String -> Boolean
isSubsetOf xs ys = all (\x -> elem x ys) xs

type Entry =
  { names :: Array String
  , letBindings :: Array (LetBinding Void)
  , single :: Maybe { name :: String, vbf :: ValueBindingFields Void }
  }

transformDoBlock
  :: Options -> String -> Array String -> DoBlock Void -> DoBlock Void
transformDoBlock opts alias initialScope db = db { statements = rebuilt }
  where
  stmtsArr = NEA.toArray db.statements

  -- Every DoBind-introduced name whose value is genuinely unstable (a raw
  -- hook-returned state value, not its setter/ref/dispatch). Fixed once
  -- from the do-block's own DoBind statements; let-bindings never add to
  -- it, they only ever consume it.
  stateNames :: Array String
  stateNames = stmtsArr # foldMap case _ of
    DoBind binder _ rhs -> classifyDoBind binder rhs # mapMaybe
      (\(Tuple n s) -> if s == Unstable then Just n else Nothing)
    _ -> []

  initialKnown :: Known
  initialKnown = map
    (\n -> Tuple n { stability: Unstable, unstableClosure: [ n ] })
    stateNames

  rebuilt =
    case NEA.fromArray (goStatements initialScope initialKnown stmtsArr) of
      Just nea -> nea
      Nothing -> db.statements

  goStatements
    :: Array String
    -> Known
    -> Array (DoStatement Void)
    -> Array (DoStatement Void)
  goStatements scope known stmts = case Array.uncons stmts of
    Nothing -> []
    Just { head: stmt, tail: rest } -> case stmt of
      DoBind binder _ rhs ->
        let
          classified = classifyDoBind binder rhs
          scope' = scope <> boundNames binder
          known' = known <> map
            ( \(Tuple n s) -> Tuple n
                { stability: s
                , unstableClosure: if s == Unstable then [ n ] else []
                }
            )
            classified
        in
          [ stmt ] <> goStatements scope' known' rest

      DoLet _ bindings ->
        let
          Tuple emitted known' = transformLetGroup scope known
            (NEA.toArray bindings)
          scope' = scope <> foldMap letBindingNames (NEA.toArray bindings)
        in
          emitted <> goStatements scope' known' rest

      DoDiscard tailExpr
        | null rest
        , Just bodyExpr <- matchPureTail tailExpr
        , not (isBareIdent bodyExpr) ->
            finalizeTail scope known stmt bodyExpr

      DoDiscard _ ->
        [ stmt ] <> goStatements scope known rest

      DoError e -> absurd e

  finalizeTail
    :: Array String
    -> Known
    -> DoStatement Void
    -> Expr Void
    -> Array (DoStatement Void)
  finalizeTail scope known original bodyExpr =
    case verdictFor scope known false (identsIn bodyExpr) bodyExpr of
      StaysPlain _ -> [ original ]
      Memoize deps ->
        let
          name = freshName scope "memoized"
        in
          [ doBind (var name) (memoExpr alias deps known [] bodyExpr)
          , DoDiscard (exprApp (ident "pure") [ ident name ])
          ]

  -- Decide whether a candidate binding gets memoized, and — in the same
  -- pass — the `Info` a downstream binding sees if it doesn't (deps/infos/
  -- closure are derived once and reused for both, rather than a separate
  -- pass recomputing them to answer "what if it stays plain").
  -- `Memoize deps` keys the memo on the eventually-stable-filtered subset
  -- of `deps` (filtering happens in `memoExpr`, using `known`, since some
  -- deps drop out of the key entirely rather than being wrapped).
  verdictFor
    :: Array String
    -> Known
    -> Boolean
    -> Array String
    -> Expr Void
    -> Verdict
  verdictFor depsScope known structurallyIneligible rawFreeVars effectiveBody
    | structurallyIneligible =
        StaysPlain { stability: Unstable, unstableClosure: stateNames }
    | otherwise =
        let
          deps = nub (filter (\v -> elem v depsScope) rawFreeVars)
          infos = map (\d -> infoOf d known) deps
          closure = nub
            ( foldMap
                ( \i ->
                    if i.stability == Unstable then i.unstableClosure else []
                )
                infos
            )
          neverHits = opts.prune && stateNames `isSubsetOf` closure
          costPruned = opts.prune && not (hasLambdaOrJsx effectiveBody)
        in
          if neverHits || costPruned then
            StaysPlain { stability: Unstable, unstableClosure: closure }
          else Memoize deps

  transformLetGroup
    :: Array String
    -> Known
    -> Array (LetBinding Void)
    -> Tuple (Array (DoStatement Void)) Known
  transformLetGroup scope known bindings = go orderedGroups known
    where
    originalSig :: String -> Maybe (LetBinding Void)
    originalSig n = bindings # Array.find case _ of
      LetBindingSignature (Labeled { label: Name { name } }) -> unwrap name == n
      _ -> false

    sigType :: String -> Maybe (Type Void)
    sigType n = case originalSig n of
      Just (LetBindingSignature (Labeled { value })) -> Just value
      _ -> Nothing

    entries :: Array Entry
    entries = bindings # mapMaybe case _ of
      LetBindingName vbf ->
        let
          name = unwrap (unwrap vbf.name).name
        in
          Just
            { names: [ name ]
            , letBindings: maybe [] (\s -> [ s ]) (originalSig name) <>
                [ LetBindingName vbf ]
            , single: Just { name, vbf }
            }
      LetBindingPattern binder tok w -> Just
        { names: boundNames binder
        , letBindings: [ LetBindingPattern binder tok w ]
        , single: Nothing
        }
      LetBindingSignature _ -> Nothing
      LetBindingError e -> absurd e

    allNames :: Array String
    allNames = foldMap _.names entries

    -- Intra-group edges, for SCC (mutual recursion) detection. Only a
    -- `LetBindingName` entry with a plain Unconditional-no-where body ever
    -- originates an edge; anything else is a graph leaf (its own
    -- ineligibility, decided elsewhere, is what keeps it safe).
    edgesFrom :: Entry -> Array String
    edgesFrom e = case e.single of
      Just
        { vbf: { guarded: Unconditional _ (Where { bindings: Nothing, expr }) }
        } ->
        filter (\n -> elem n allNames) (identsIn expr)
      _ -> []

    ownerOf :: String -> Maybe Entry
    ownerOf n = Array.find (\e -> elem n e.names) entries

    reachable :: String -> Array String
    reachable start = step [ start ] []
      where
      step [] acc = acc
      step frontier acc =
        let
          acc' = nub (acc <> frontier)
          next = nub (frontier >>= (\n -> maybe [] edgesFrom (ownerOf n))) #
            filter (\n -> notElem n acc')
        in
          if null next then acc' else step next acc'

    sccOf :: String -> Array String
    sccOf n = filter
      (\m -> m /= n && elem m (reachable n) && elem n (reachable m))
      allNames

    mutuallyRecursive :: Array String
    mutuallyRecursive = filter (\n -> not (null (sccOf n))) allNames

    -- Batch entries into emission units: a lone entry, or (for a
    -- multi-member SCC) every member of that SCC together, combined into
    -- one residual `let`.
    batches :: Array (Array Entry)
    batches = step entries []
      where
      step es acc = case Array.uncons es of
        Nothing -> acc
        Just { head, tail } ->
          let
            group = nub (head.names <> foldMap sccOf head.names)
            members = filter (\e -> any (\n -> elem n group) e.names) es
            remaining = filter (\e -> not (any (\n -> elem n group) e.names))
              tail
          in
            step remaining (acc <> [ members ])

    -- Topological order over batches: a batch's external deps are the
    -- union of its members' raw free vars, restricted to other batches'
    -- names — repeatedly take the first batch whose external deps are
    -- already emitted.
    orderedGroups :: Array (Array Entry)
    orderedGroups = order batches []
      where
      externalDeps b = foldMap edgesFrom b # filter
        (\n -> notElem n (foldMap _.names b))
      order remaining done = case Array.uncons remaining of
        Nothing -> []
        Just _ ->
          case
            Array.find (\b -> all (\d -> elem d done) (externalDeps b))
              remaining
            of
            Just b -> [ b ] <> order
              (filter (\x -> not (eqBatch x b)) remaining)
              (done <> foldMap _.names b)
            Nothing -> remaining -- defensive: shouldn't happen once SCCs are batched together

      eqBatch a b = foldMap _.names a == foldMap _.names b

    go :: Array (Array Entry) -> Known -> Tuple (Array (DoStatement Void)) Known
    go groups known' = case Array.uncons groups of
      Nothing -> Tuple [] known'
      Just { head: batch, tail } ->
        let
          Tuple stmts known'' = emitBatch batch known'
          Tuple restStmts known''' = go tail known''
        in
          Tuple (stmts <> restStmts) known'''

    emitBatch :: Array Entry -> Known -> Tuple (Array (DoStatement Void)) Known
    emitBatch batch known' = case batch of
      [ e ]
        | Just { name, vbf } <- e.single
        , all (\n -> notElem n mutuallyRecursive) e.names ->
            let
              hasWhereOrGuard = case vbf.guarded of
                Unconditional _ (Where { bindings: Just _ }) -> true
                Guarded _ -> true
                _ -> false
              bodyExpr = case vbf.guarded of
                Unconditional _ (Where { bindings: Nothing, expr }) -> Just expr
                _ -> Nothing
              ineligible = hasWhereOrGuard
                || maybe false hasForallOrConstraint (sigType name)
                || maybe true mentionsUnsafeOrDebug bodyExpr
              rawFreeVars = maybe [] identsIn bodyExpr
              depsScope = filter (\n -> n /= name) (scope <> allNames)
              effectiveBody = case bodyExpr, Array.uncons vbf.binders of
                Just b, Just _ -> exprLambda vbf.binders b
                Just b, Nothing -> b
                Nothing, _ -> ident "unit" -- unreachable: ineligible is already true here
            in
              case
                verdictFor depsScope known' ineligible rawFreeVars
                  effectiveBody,
                bodyExpr
                of
                Memoize deps, Just body ->
                  Tuple
                    [ doBind (var name)
                        (memoExpr alias deps known' vbf.binders body)
                    ]
                    ( known' <>
                        [ Tuple name
                            { stability: MemoizedResult, unstableClosure: [] }
                        ]
                    )
                StaysPlain info, _ ->
                  Tuple [ letGroup e.letBindings ]
                    (known' <> [ Tuple name info ])
                Memoize _, Nothing ->
                  -- unreachable: `ineligible` is already true whenever
                  -- `bodyExpr` is `Nothing`, which forces `StaysPlain` above
                  Tuple [ letGroup e.letBindings ]
                    ( known' <>
                        [ Tuple name
                            { stability: Unstable, unstableClosure: stateNames }
                        ]
                    )

      _ ->
        -- Multi-member SCC, or a pattern-form entry (`let Tuple a b = e`):
        -- always residual, pessimistically Unstable so anything downstream
        -- that depends on it also never memoizes.
        Tuple [ letGroup (foldMap _.letBindings batch) ]
          ( known' <> foldMap
              ( \e -> map
                  ( \n -> Tuple n
                      { stability: Unstable, unstableClosure: stateNames }
                  )
                  e.names
              )
              batch
          )

letBindingNames :: LetBinding Void -> Array String
letBindingNames = case _ of
  LetBindingName vbf -> [ unwrap (unwrap vbf.name).name ]
  LetBindingPattern binder _ _ -> boundNames binder
  LetBindingSignature _ -> []
  LetBindingError e -> absurd e

freshName :: Array String -> String -> String
freshName scope base = go 0
  where
  go n =
    let
      candidate = if n == 0 then base else base <> show n
    in
      if elem candidate scope then go (n + 1) else candidate

-- | `pure expr`, `pure $ expr`, and `expr # pure` all count as a `pure`
-- | tail. A tail that is already a bare identifier (`pure memoized`) is left
-- | alone by the caller (idempotency: re-running the transform on its own
-- | output must not grow the tail further).
matchPureTail :: Expr Void -> Maybe (Expr Void)
matchPureTail = case _ of
  ExprApp (ExprIdent (QualifiedName { name })) args | unwrap name == "pure" ->
    case NEA.toArray args of
      [ term ] -> appTermExpr term
      _ -> Nothing
  ExprOp (ExprIdent (QualifiedName { name })) ops | unwrap name == "pure" ->
    case NEA.toArray ops of
      [ Tuple (QualifiedName { name: opName }) rhs ] | unwrap opName == "$" ->
        Just rhs
      _ -> Nothing
  ExprOp lhs ops ->
    case NEA.toArray ops of
      [ Tuple (QualifiedName { name: opName })
          (ExprIdent (QualifiedName { name }))
      ] | unwrap opName == "#" && unwrap name == "pure" -> Just lhs
      _ -> Nothing
  _ -> Nothing

appTermExpr :: AppSpine Expr Void -> Maybe (Expr Void)
appTermExpr = case _ of
  AppTerm e -> Just e
  AppType _ _ -> Nothing

isBareIdent :: Expr Void -> Boolean
isBareIdent = case _ of
  ExprIdent _ -> true
  _ -> false

-- | A syntactic proxy for "expensive enough that memoizing it can pay off":
-- | the body constructs a lambda (closures are the cheap-but-nonzero case
-- | `useMemo` still helps for) or looks like it builds JSX (a `TokBackslash`
-- | is treated as a lambda signal too, since `moves`-shaped bindings are
-- | exactly "a lambda passed to a combinator that builds a list of JSX"; a
-- | reference to `fragment`/`keyed`/`element`, or anything qualified
-- | through an `R` alias — the react-basic-dom convention this repo and the
-- | wider ecosystem use — is treated as a JSX signal). No type information
-- | is available at this stage; a binding this predicate misses is simply
-- | left unmemoized rather than wrongly memoized, so the failure mode is a
-- | missed optimization, never a correctness issue.
hasLambdaOrJsx :: Expr Void -> Boolean
hasLambdaOrJsx = anyToken isLambdaOrJsxToken
  where
  isLambdaOrJsxToken = case _ of
    TokBackslash -> true
    TokLowerName (Just (ModuleName "R")) _ -> true
    TokLowerName _ "fragment" -> true
    TokLowerName _ "keyed" -> true
    TokLowerName _ "element" -> true
    _ -> false

memoExpr
  :: String
  -> Array String
  -> Known
  -> Array (Binder Void)
  -> Expr Void
  -> Expr Void
memoExpr alias deps known binders body =
  exprApp (ident (alias <> ".useMemo"))
    [ key, exprLambda [ binderWildcard ] innerBody ]
  where
  keyDeps = filter
    ( \d -> case (infoOf d known).stability of
        StableDirect -> false
        StableSemantic -> false
        _ -> true
    )
    deps

  key = case Array.uncons keyDeps of
    Nothing -> ident "unit"
    Just { head, tail } -> case Array.uncons tail of
      Nothing -> wrapRef head
      Just _ -> exprOp (wrapRef head) (map (op "/\\" <<< wrapRef) tail)

  wrapRef n = exprApp (ctor (alias <> ".UnsafeReference")) [ ident n ]

  innerBody = case Array.uncons binders of
    Just _ -> exprLambda binders body
    Nothing -> body
