-- | Lifts eligible `let` bindings (and the final `pure` tail) in `React.do` blocks into `useMemo` binds.
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

-- | `Partial` codegen wrappers — the names this tool generates always lex cleanly.
ident :: forall e. String -> Expr e
ident = unsafePartial RawCodegen.exprIdent

ctor :: forall e. String -> Expr e
ctor = unsafePartial RawCodegen.exprCtor

var :: forall e. String -> Binder e
var = unsafePartial RawCodegen.binderVar

op :: forall e. String -> Expr e -> BinaryOp (Expr e)
op = unsafePartial RawCodegen.binaryOp

-- | Re-wraps original (unmodified) `LetBinding`s back into one `let` statement.
letGroup :: Array (LetBinding Void) -> DoStatement Void
letGroup = unsafePartial RawCodegen.doLet

type Options = { prune :: Boolean }

defaultOptions :: Options
defaultOptions = { prune: true }

-- | Rewrites every `React.do` block; matched on the enclosing lambda so its params are in scope.
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

-- | The alias `React.do` was written with — also how `useMemo`/`UnsafeReference` get qualified.
reactDoAlias :: SourceToken -> Maybe String
reactDoAlias tok = case tok.value of
  TokLowerName (Just (ModuleName "React")) "do" -> Just "React"
  _ -> Nothing

-- | `Unstable` propagates its closure downstream; `MemoizedResult` doesn't, but still appears in keys.
type Info = { stability :: Stability, unstableClosure :: Array String }
type Known = Map String Info

-- | A candidate's decision, plus the `Info` a downstream binding sees either way.
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
        -- Free vars, cached once for edgesFrom/emitBatch.
        , freeVars :: Array String
        }
  }

transformDoBlock
  :: Options -> String -> Array String -> DoBlock Void -> DoBlock Void
transformDoBlock opts alias initialScope db = db { statements = rebuilt }
  where
  stmtsArr = NEA.toArray db.statements

  -- Raw hook state only (not setters/refs), fixed before any let is processed.
  stateNames :: Array String
  stateNames = stmtsArr # foldMap case _ of
    DoBind binder _ rhs -> classifyDoBind binder rhs # mapMaybe
      (\(Tuple n s) -> if s == Unstable then Just n else Nothing)
    _ -> []

  stateNamesSet :: Set String
  stateNamesSet = Set.fromFoldable stateNames

  -- Assigned to a binding that stays plain outright — could reference any state variable.
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
    case
      verdictFor scope known (mentionsUnsafeOrDebug bodyExpr)
        (identsIn bodyExpr)
        bodyExpr
      of
      StaysPlain _ -> [ original ]
      Memoize deps ->
        let
          name = freshName scope "memoized"
        in
          [ doBind (var name) (memoExpr alias deps [] bodyExpr)
          , DoDiscard (exprApp (ident "pure") [ ident name ])
          ]

  -- Decides Memoize vs StaysPlain and the Info a downstream binding sees, in one pass.
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
          -- Guard against Set.subset being vacuously true when stateNamesSet is empty.
          neverHits = opts.prune && not (Set.isEmpty stateNamesSet) &&
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

    fullScope :: Array String
    fullScope = scope <> allNames

    -- Intra-group dependency edges, for mutual-recursion (SCC) detection.
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

    -- `edgesFrom`'s graph keyed by entry index; self-edges dropped (not mutual recursion).
    graph :: Map Int (Set Int)
    graph = Map.fromFoldable $ entries # Array.mapWithIndex \i e ->
      Tuple i
        ( Set.fromFoldable (Array.mapMaybe indexOfName (edgesFrom e)) #
            Set.delete i
        )

    -- Emission-order batches: singletons, or a detected cycle's members together.
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

    -- Topological order over batches; a multi-member batch is exactly mutual recursion.
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
        -- Multi-member SCC or a pattern binding: always residual and pessimistically Unstable.
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

-- | Kahn's-algorithm topo sort, modeled on the compiler's own (unexported) `PureScript.CST.ModuleGraph.topoSort`.
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

  -- curr is already path's head when revisited, so it isn't re-added.
  depthFirst path visited curr =
    if Set.member curr visited then Just path
    else if maybe true Set.isEmpty (Map.lookup curr graph) then Nothing
    else Map.lookup curr graph >>= \next ->
      foldl
        ( \b a ->
            if isJust b then b
            else depthFirst (curr : path) (Set.insert curr visited) a
        )
        Nothing
        next

-- | Matches `pure expr`/`pure $ expr`/`expr # pure`; a bare-ident tail is left to the caller.
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

-- | Heuristic "worth memoizing" signal: a lambda or JSX-shaped token; false negatives just skip memoization.
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
