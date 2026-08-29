-- | The hook stability table, plus the syntactic guards that keep a binding out of the transform entirely.
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

-- | `StableDirect`/`StableSemantic` drop from a key; `MemoizedResult` doesn't propagate but still appears in one; `Unstable` always propagates.
data Stability = StableDirect | StableSemantic | MemoizedResult | Unstable

derive instance eqStability :: Eq Stability

-- | Classifies the name(s) a `DoBind` introduces; unrecognized calls default to `Unstable` (the safe direction).
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
    -- Recognizes this tool's own output, for idempotency on a re-run.
    Just "useMemo", [ result ] -> [ Tuple result MemoizedResult ]
    _, _ -> map (\n -> Tuple n Unstable) names
  where
  names = boundNames binder

-- | The function at the head of an application, seeing through a module qualifier and `$`/`#` piping.
appHeadName :: Expr Void -> Maybe String
appHeadName = case _ of
  ExprApp f _ -> appHeadName f
  ExprOp lhs ops -> case Array.last (NEA.toArray ops) of
    Just (Tuple (QualifiedName { name: opName }) rhs)
      | unwrap opName == "#" -> appHeadName rhs
    _ -> appHeadName lhs
  ExprIdent (QualifiedName { name }) -> Just (unwrap name)
  _ -> Nothing

-- | A polymorphic signature can't be lifted into a monomorphic `useMemo` result.
hasForallOrConstraint :: Type Void -> Boolean
hasForallOrConstraint = anyToken isForallOrConstraint
  where
  isForallOrConstraint = case _ of
    TokForall _ -> true
    TokRightFatArrow _ -> true
    _ -> false

-- | A binding reaching outside purity can't be soundly memoized.
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
