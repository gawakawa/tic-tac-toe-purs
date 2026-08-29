-- | The hook stability table, plus the two syntactic guards that keep a
-- | binding out of the transform entirely (as opposed to affecting its memo
-- | key): a discarded signature that can't be monomorphized, and a body that
-- | reaches outside PureScript's purity guarantees.
module PursMemo.Stability
  ( Stability(..)
  , anyToken
  , classifyDoBind
  , hasForallOrConstraint
  , mentionsUnsafeOrDebug
  ) where

import Prelude

import Data.Array (any)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import PureScript.CST.Range (class TokensOf, tokensOf)
import PureScript.CST.Range.TokenList (toArray)
import PureScript.CST.Types (Binder, Expr(..), ModuleName(..), QualifiedName(..), Token(..), Type)
import PursMemo.Scope (boundNames)

-- | `StableDirect` — the value itself is reference-stable across renders
-- | (a `useRef` cell, a `useReducer` dispatch, a cached raw `useState`
-- | setter, a `useEffectEvent` result).
-- | `StableSemantic` — the value's *identity* changes every render but its
-- | *behavior* does not, so capturing render 1's copy is sound (the
-- | `useState'` setter: `\x -> cachedSetter (const x)`).
-- | Both are dropped from a downstream memo's key entirely.
-- | `MemoizedResult` — a name `PursMemo.Transform` itself bound via
-- | `useMemo`. It is stable in the sense that it doesn't propagate further
-- | instability, but unlike the two hook-table categories above it is NOT
-- | omitted from a downstream key: React's own `useMemo` can still return a
-- | fresh value when ITS key misses, so a consumer must still watch it.
-- | `Unstable` — everything else. Never dropped from a key, and always
-- | propagates (it is what a never-hits closure is built from).
data Stability = StableDirect | StableSemantic | MemoizedResult | Unstable

derive instance eqStability :: Eq Stability

-- | Classify the name(s) a `DoBind` introduces, in binder order. Anything
-- | not recognized by the table defaults to `Unstable` for every name it
-- | binds — the safe direction, since Unstable never gets dropped from a key.
classifyDoBind :: Binder Void -> Expr Void -> Array (Tuple String Stability)
classifyDoBind binder rhs =
  case appHeadName rhs, names of
    Just "useState'", [ state, setter ] ->
      [ Tuple state Unstable, Tuple setter StableSemantic ]
    Just "useState", [ state, setter ] ->
      [ Tuple state Unstable, Tuple setter StableDirect ]
    Just "useReducer", [ state, dispatch ] ->
      [ Tuple state Unstable, Tuple dispatch StableDirect ]
    Just "useRef", [ ref ] -> [ Tuple ref StableDirect ]
    Just "useEffectEvent", [ fn ] -> [ Tuple fn StableDirect ]
    _, _ -> map (\n -> Tuple n Unstable) names
  where
  names = boundNames binder

-- | The function name at the head of an application, ignoring any module
-- | qualifier (`useState'` and `React.useState'` classify the same way) and
-- | any `$`/`#` the hook call is piped through (`useState' $ expensive init`
-- | or `expensive init # useState'`, both used in the wild) — `$` applies
-- | its left operand to its right (head stays left), `#` is `flip ($)`
-- | (head is its right operand instead), so which side to recurse into
-- | depends on the last operator in the chain, not a fixed side.
appHeadName :: Expr Void -> Maybe String
appHeadName = case _ of
  ExprApp f _ -> appHeadName f
  ExprOp lhs ops -> case Array.last (NEA.toArray ops) of
    Just (Tuple (QualifiedName { name: opName }) rhs)
      | unwrap opName == "#" -> appHeadName rhs
    _ -> appHeadName lhs
  ExprIdent (QualifiedName { name }) -> Just (unwrap name)
  _ -> Nothing

-- | A discarded `LetBindingSignature` mentioning `forall` or `=>` is the one
-- | place dropping the signature is a real risk: `useMemo`'s result type is
-- | monomorphic, so a genuinely polymorphic binding can't be lifted at all,
-- | signature or not. Leave it in a residual `let`.
hasForallOrConstraint :: Type Void -> Boolean
hasForallOrConstraint = anyToken isForallOrConstraint
  where
  isForallOrConstraint = case _ of
    TokForall _ -> true
    TokRightFatArrow _ -> true
    _ -> false

-- | A binding that reaches outside purity (`unsafePerformEffect`,
-- | `unsafeCoerce`, anything from `Debug`) can't be soundly memoized: purity
-- | is what makes recomputation-on-miss harmless, and none of these are pure.
mentionsUnsafeOrDebug :: Expr Void -> Boolean
mentionsUnsafeOrDebug = anyToken isUnsafeOrDebug
  where
  isUnsafeOrDebug = case _ of
    TokLowerName _ "unsafePerformEffect" -> true
    TokLowerName _ "unsafeCoerce" -> true
    TokLowerName (Just (ModuleName "Debug")) _ -> true
    _ -> false

anyToken :: forall a. TokensOf a => (Token -> Boolean) -> a -> Boolean
anyToken p = any (p <<< _.value) <<< toArray <<< tokensOf
