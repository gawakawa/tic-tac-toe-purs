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

import Data.Array (elem, filter, foldMap, mapMaybe, nub, null)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Foldable (all) as Foldable
import Data.Foldable (foldl)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst)
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
type Known = Map String Info

-- | The transform's decision for one candidate binding, plus — when it
-- | stays plain — the `Info` a downstream binding sees. Computed together
-- | so the shared deps/closure derivation happens exactly once instead of
-- | being redone by a separate "and what if not" pass. `Memoize` carries
-- | each dep's already-looked-up `Info` (not just its name) so `memoExpr`
-- | doesn't have to repeat the `infoOf` lookup that built the closure.
data Verdict = Memoize (Array (Tuple String Info)) | StaysPlain Info

infoOf :: String -> Known -> Info
infoOf n known = fromMaybe { stability: Unstable, unstableClosure: [ n ] }
  (Map.lookup n known)

type Entry =
  { names :: Array String
  , letBindings :: Array (LetBinding Void)
  , single ::
      Maybe
        { name :: String
        , vbf :: ValueBindingFields Void
        -- The entry's free variables, if it's a plain Unconditional-no-where
        -- binding (computed once here; both `edgesFrom` and `emitBatch`
        -- need exactly this set, gated on exactly this same pattern).
        , freeVars :: Array String
        }
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

  stateNamesSet :: Set String
  stateNamesSet = Set.fromFoldable stateNames

  -- The pessimistic `Info` assigned to a binding that stays plain outright
  -- (a where/guarded body, a multi-member SCC): it could reference any
  -- state variable, so its closure covers all of them.
  pessimisticInfo :: Info
  pessimisticInfo = { stability: Unstable, unstableClosure: stateNames }

  initialKnown :: Known
  initialKnown = Map.fromFoldable
    ( map (\n -> Tuple n { stability: Unstable, unstableClosure: [ n ] })
        stateNames
    )

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
          scope' = scope <> map fst classified
          known' = Map.union
            ( Map.fromFoldable $ classified <#> \(Tuple n s) -> Tuple n
                { stability: s
                , unstableClosure: if s == Unstable then [ n ] else []
                }
            )
            known
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
          [ doBind (var name) (memoExpr alias deps [] bodyExpr)
          , DoDiscard (exprApp (ident "pure") [ ident name ])
          ]

  -- Decide whether a candidate binding gets memoized, and — in the same
  -- pass — the `Info` a downstream binding sees if it doesn't (deps/infos/
  -- closure are derived once and reused for both, rather than a separate
  -- pass recomputing them to answer "what if it stays plain").
  -- `Memoize` keys the memo on the eventually-stable-filtered subset of its
  -- deps (filtering happens in `memoExpr`, since some deps drop out of the
  -- key entirely rather than being wrapped) — each dep's `Info` travels
  -- with it, already looked up here, so `memoExpr` need not repeat that.
  verdictFor
    :: Array String
    -> Known
    -> Boolean
    -> Array String
    -> Expr Void
    -> Verdict
  verdictFor depsScope known structurallyIneligible rawFreeVars effectiveBody
    | structurallyIneligible = StaysPlain pessimisticInfo
    | otherwise =
        let
          deps = nub (filter (\v -> elem v depsScope) rawFreeVars)
          infos = map (\d -> Tuple d (infoOf d known)) deps
          closure = nub
            ( foldMap
                ( \(Tuple _ i) ->
                    if i.stability == Unstable then i.unstableClosure else []
                )
                infos
            )
          neverHits = opts.prune &&
            stateNamesSet `Set.subset` Set.fromFoldable closure
          costPruned = opts.prune && not (hasLambdaOrJsx effectiveBody)
        in
          if neverHits || costPruned then
            StaysPlain { stability: Unstable, unstableClosure: closure }
          else Memoize infos

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
          freeVars = case vbf.guarded of
            Unconditional _ (Where { bindings: Nothing, expr }) ->
              identsIn expr
            _ -> []
        in
          Just
            { names: [ name ]
            , letBindings: maybe [] (\s -> [ s ]) (originalSig name) <>
                [ LetBindingName vbf ]
            , single: Just { name, vbf, freeVars }
            }
      LetBindingPattern binder tok w -> Just
        { names: letBindingNames (LetBindingPattern binder tok w)
        , letBindings: [ LetBindingPattern binder tok w ]
        , single: Nothing
        }
      LetBindingSignature _ -> Nothing
      LetBindingError e -> absurd e

    allNames :: Array String
    allNames = foldMap _.names entries

    -- Every name in scope by the time this group's residual bindings are
    -- reached — fixed for the whole group, so hoisted rather than rebuilt
    -- per entry.
    fullScope :: Array String
    fullScope = scope <> allNames

    -- Intra-group edges, for SCC (mutual recursion) detection. Only a
    -- `LetBindingName` entry with a plain Unconditional-no-where body ever
    -- originates an edge; anything else is a graph leaf (its own
    -- ineligibility, decided elsewhere, is what keeps it safe).
    edgesFrom :: Entry -> Array String
    edgesFrom e = case e.single of
      Just { freeVars } -> filter (\n -> elem n allNames) freeVars
      _ -> []

    nameToIndex :: Map String Int
    nameToIndex = Map.fromFoldable do
      Tuple i e <- Array.mapWithIndex Tuple entries
      n <- e.names
      pure (Tuple n i)

    indexOfName :: String -> Maybe Int
    indexOfName n = Map.lookup n nameToIndex

    -- The same dependency graph `edgesFrom` already describes, keyed by
    -- entry index (a pattern binding introduces more than one name for a
    -- single graph node) rather than name, for `topoSort`. A self-edge
    -- (plain self-recursion, `let f x = ... f ...`) is dropped: it isn't
    -- mutual recursion, and left in would make `topoSort` see the entry as
    -- permanently stuck on itself.
    graph :: Map Int (Set Int)
    graph = Map.fromFoldable $ entries # Array.mapWithIndex \i e ->
      Tuple i
        ( Set.fromFoldable (Array.mapMaybe indexOfName (edgesFrom e)) #
            Set.delete i
        )

    -- Batches of entry indices in emission order: a singleton for an
    -- ordinary binding, or every member of one detected cycle (mutual
    -- recursion) together. Repeatedly runs `topoSort`, and whenever it
    -- reports a cycle, batches that cycle's nodes, removes them (along with
    -- whatever already sorted cleanly before them) and continues on what's
    -- left — the whole graph is only ever one `let` group's bindings, so
    -- redoing the sort from scratch each time costs nothing.
    indexBatches :: Map Int (Set Int) -> Array (Array Int)
    indexBatches g
      | Map.isEmpty g = []
      | otherwise = case topoSort g of
          Right sorted -> pure <$> List.toUnfoldable sorted
          Left { sorted, cycle } ->
            let
              handled = Set.fromFoldable sorted <> Set.fromFoldable cycle
              remaining = g
                # Map.filterKeys (\k -> not (Set.member k handled))
                # map (Set.filter (\k -> not (Set.member k handled)))
            in
              (pure <$> List.toUnfoldable sorted)
                <> [ List.toUnfoldable cycle ]
                <> indexBatches remaining

    -- Topological order over batches, and — inlined here since it's now
    -- just an index lookup rather than a search — the ineligibility check
    -- `emitBatch` used to make separately: mutual recursion is exactly a
    -- multi-member batch.
    orderedGroups :: Array (Array Entry)
    orderedGroups = indexBatches graph <#> Array.mapMaybe (Array.index entries)

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
      [ e ] | Just { name, vbf, freeVars } <- e.single ->
        let
          -- `bodyExpr` is `Nothing` exactly when a `where`/guard makes
          -- this binding ineligible outright, so that case never needs
          -- `verdictFor` at all — it's always `StaysPlain`, pessimistic
          -- (a where/guarded binding could reference anything).
          bodyExpr = case vbf.guarded of
            Unconditional _ (Where { bindings: Nothing, expr }) -> Just expr
            _ -> Nothing
          depsScope = filter (\n -> n /= name) fullScope
        in
          case bodyExpr of
            Nothing ->
              Tuple [ letGroup e.letBindings ]
                (Map.insert name pessimisticInfo known')
            Just body ->
              let
                ineligible =
                  maybe false hasForallOrConstraint
                    (sigType name)
                    || mentionsUnsafeOrDebug body
                effectiveBody = case Array.uncons vbf.binders of
                  Just _ -> exprLambda vbf.binders body
                  Nothing -> body
              in
                case
                  verdictFor depsScope known' ineligible freeVars effectiveBody
                  of
                  Memoize deps ->
                    Tuple
                      [ doBind (var name)
                          (memoExpr alias deps vbf.binders body)
                      ]
                      ( Map.insert name
                          { stability: MemoizedResult, unstableClosure: [] }
                          known'
                      )
                  StaysPlain info ->
                    Tuple [ letGroup e.letBindings ]
                      (Map.insert name info known')

      _ ->
        -- Multi-member SCC, or a pattern-form entry (`let Tuple a b = e`):
        -- always residual, pessimistically Unstable so anything downstream
        -- that depends on it also never memoizes.
        Tuple [ letGroup (foldMap _.letBindings batch) ]
          ( Map.union
              ( Map.fromFoldable $ foldMap _.names batch <#> \n -> Tuple n
                  pessimisticInfo
              )
              known'
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

-- | Topologically sort a dependency graph (each node mapped to the other
-- | nodes it depends on), closely modeled on the same-named, unexported
-- | function in `PureScript.CST.ModuleGraph` — the PureScript compiler
-- | frontend's own Kahn's-algorithm-plus-DFS-cycle-extraction, used there to
-- | detect circular module imports. Adapted for `let`-binding dependencies
-- | (mutual recursion) instead of imports, `topoSort` differs from the
-- | original in two ways: on hitting a cycle it also returns the prefix that
-- | already sorted cleanly, since the caller here needs to batch off one
-- | cycle and keep going, not abort with a "circular" error; and it breaks
-- | ties among simultaneously-ready nodes by taking the *largest* key first
-- | rather than the smallest. Module compilation order genuinely doesn't
-- | care which of several ready siblings goes first, but a codegen tool
-- | does — `sorted` is built by consing each processed node onto the front,
-- | so the *last*-processed node ends up first; picking ties
-- | largest-first, then, is what reproduces each `let` group's original
-- | declaration order among bindings that don't depend on each other,
-- | rather than silently reversing it.
topoSort
  :: forall a
   . Ord a
  => Map a (Set a)
  -> Either { sorted :: List a, cycle :: List a } (List a)
topoSort graph = go { roots: startingNodes, sorted: Nil, usages: importCounts }
  where
  go { roots, sorted, usages } = case Set.findMax roots of
    Nothing
      | Foldable.all (eq 0) usages -> Right sorted
      | otherwise ->
          let
            stuck = usages
              # Map.filterWithKey
                  ( \a count -> count > 0 &&
                      not (maybe true Set.isEmpty (Map.lookup a graph))
                  )
              # Map.keys
            found = foldl
              (\b a -> if isJust b then b else depthFirst Nil Set.empty a)
              Nothing
              stuck
          in
            Left { sorted, cycle: fromMaybe Nil found }
    Just curr ->
      let
        deps = fromMaybe Set.empty (Map.lookup curr graph)
        usages' = foldl (\u k -> Map.insertWith add k (-1) u) usages deps
      in
        go
          { roots: foldl (appendRoot usages') (Set.delete curr roots) deps
          , sorted: curr : sorted
          , usages: usages'
          }

  appendRoot usages roots curr = maybe roots (flip Set.insert roots) do
    count <- Map.lookup curr usages
    isRoot (Tuple curr count)

  startingNodes = Map.keys $ Map.filterWithKey
    (\k v -> isJust (isRoot (Tuple k v)))
    importCounts

  importCounts = Map.fromFoldableWith add do
    Tuple a bs <- Map.toUnfoldable graph
    [ Tuple a 0 ] <> map (flip Tuple 1) (Set.toUnfoldable bs :: Array a)

  isRoot (Tuple a count) = if count == 0 then Just a else Nothing

  depthFirst path visited curr =
    if Set.member curr visited then Just (curr : path)
    else if maybe true Set.isEmpty (Map.lookup curr graph) then Nothing
    else Map.lookup curr graph >>= \next ->
      foldl
        ( \b a ->
            if isJust b then b
            else depthFirst (curr : path) (Set.insert curr visited) a
        )
        Nothing
        next

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
  -> Array (Tuple String Info)
  -> Array (Binder Void)
  -> Expr Void
  -> Expr Void
memoExpr alias deps binders body =
  exprApp (ident (alias <> ".useMemo"))
    [ key, exprLambda [ binderWildcard ] innerBody ]
  where
  keyDeps = filter
    ( \(Tuple _ info) -> case info.stability of
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

  wrapRef (Tuple n _) = exprApp (ctor (alias <> ".UnsafeReference")) [ ident n ]

  innerBody = case Array.uncons binders of
    Just _ -> exprLambda binders body
    Nothing -> body
