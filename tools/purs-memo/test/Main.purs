module Test.Main where

import Prelude

import Data.Array (length)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Effect (Effect)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PursMemo.Transform (defaultOptions, transformModule)
import Test.Unit (Test, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Tidy.Codegen (printModule)

-- | Parse, transform with the shipped (pruned) defaults, and re-print.
-- | `Nothing` on a parse failure of either the input or the output --
-- | every fixture is expected to parse both times, so callers assert
-- | `isJust` before inspecting the printed text.
transform :: String -> Maybe String
transform src = case parseModule src of
  ParseSucceeded m -> case printModule (transformModule defaultOptions m) of
    out | reparses out -> Just out
    _ -> Nothing
  _ -> Nothing
  where
  reparses s = case parseModule s of
    ParseSucceeded _ -> true
    _ -> false

-- | Run `transform` on `src` and hand the printed output to `body`; fails
-- | the test with a `label`-derived message if parsing/transforming/
-- | reparsing didn't succeed.
withTransformed :: String -> String -> (String -> Test) -> Test
withTransformed label src body = case transform src of
  Nothing -> Assert.assert (label <> " failed to parse/transform/reparse")
    false
  Just out -> body out

contains :: String -> String -> Boolean
contains needle haystack = String.contains (Pattern needle) haystack

occurrences :: String -> String -> Int
occurrences needle haystack = length (String.split (Pattern needle) haystack) -
  1

indexOf :: String -> String -> Maybe Int
indexOf needle haystack = String.indexOf (Pattern needle) haystack

-- The mkGame shape: two state variables, a non-JSX let (cost-floor pruned),
-- a JSX-valued let whose closure omits one state variable (survives
-- never-hits pruning), and a final `pure` tail depending only on the
-- already-memoized binding (gets tail-split).
fixtureMkGame :: String
fixtureMkGame =
  """module Fixture where

import Prelude
import React.Basic.DOM as R
import React.Basic.Hooks (Component, component, useState')
import React.Basic.Hooks as React
import Data.Tuple.Nested ((/\))

mkGame :: Component Unit
mkGame = component "Game" \_ ->
  React.do
    history /\ setHistory <- useState' 0
    currentMove /\ setCurrentMove <- useState' 0
    let
      currentSquares = combine history currentMove
      moves = \_ -> R.ol_ (buildItems history)
    pure (R.div { children: [ moves ] })
"""

-- A `useEffect`-shaped DoBind sitting between two eligible lets, pinning
-- that memo emission stays at each let's own position rather than being
-- hoisted across it.
fixtureInsertionPoint :: String
fixtureInsertionPoint =
  """module Fixture where

import Prelude
import React.Basic.DOM as R
import React.Basic.Hooks (Component, component, useEffect, useState')
import React.Basic.Hooks as React
import Data.Tuple.Nested ((/\))

mkThing :: Component Unit
mkThing = component "Thing" \_ ->
  React.do
    n /\ setN <- useState' 0
    m /\ setM <- useState' 0
    let
      squared = \_ -> n * n
    _ <- useEffect n (pure mempty)
    let
      doubled = \_ -> m + m
    pure (R.div { children: [] })
"""

-- A mutually recursive `let` pair (an SCC): must stay a plain `let`, never
-- a `useMemo` bind, and must keep the final tail plain too (it depends on
-- a residual binding, which can never hit).
fixtureMutualRecursion :: String
fixtureMutualRecursion =
  """module Fixture where

import Prelude
import React.Basic.DOM as R
import React.Basic.Hooks (Component, component, useState')
import React.Basic.Hooks as React
import Data.Tuple.Nested ((/\))

mkLoop :: Component Unit
mkLoop = component "Loop" \_ ->
  React.do
    n /\ setN <- useState' 0
    let
      isEven = \k -> if k == 0 then true else isOdd (k - 1)
      isOdd = \k -> if k == 0 then false else isEven (k - 1)
    pure (R.div { children: [ R.text (show (isEven n)) ] })
"""

-- A plain self-recursive binding (not mutual recursion — one entry, edging
-- to itself). The dependency graph `topoSort` walks must drop that
-- self-edge, or the entry looks permanently stuck on itself and never gets
-- a verdict at all. `fact` doesn't reference either state variable, so it
-- should still memoize (empty key) like any other eligible binding.
fixtureSelfRecursion :: String
fixtureSelfRecursion =
  """module Fixture where

import Prelude
import React.Basic.DOM as R
import React.Basic.Hooks (Component, component, useState')
import React.Basic.Hooks as React
import Data.Tuple.Nested ((/\))

mkFact :: Component Unit
mkFact = component "Fact" \_ ->
  React.do
    n /\ setN <- useState' 0
    m /\ setM <- useState' 0
    let
      fact = \k -> if k == 0 then 1 else k * fact (k - 1)
    pure (R.div { children: [ R.text (show (fact m)) ] })
"""

-- A guarded `let` binding: ineligible by construction, must stay plain,
-- and must keep the final tail plain too (same propagation as above).
fixtureGuarded :: String
fixtureGuarded =
  """module Fixture where

import Prelude
import React.Basic.DOM as R
import React.Basic.Hooks (Component, component, useState')
import React.Basic.Hooks as React
import Data.Tuple.Nested ((/\))

mkGuard :: Component Unit
mkGuard = component "Guard" \_ ->
  React.do
    n /\ setN <- useState' 0
    let
      classify
        | n > 0 = "positive"
        | otherwise = "non-positive"
    pure (R.div { children: [ R.text classify ] })
"""

-- The positional-scope / record-label regression: a record field label
-- ("history") coincides with an in-scope state variable name, next to a
-- real reference ("other") to a different state variable. If field labels
-- were ever collected as free variables (the bug the plan's amendment
-- closes), this key would wrongly include `history`.
fixtureRecordLabel :: String
fixtureRecordLabel =
  """module Fixture where

import Prelude
import React.Basic.DOM as R
import React.Basic.Hooks (Component, component, useState')
import React.Basic.Hooks as React
import Data.Tuple.Nested ((/\))

mkLabel :: Component Unit
mkLabel = component "Label" \_ ->
  React.do
    history /\ setHistory <- useState' 0
    other /\ setOther <- useState' 0
    let
      obj = \_ -> { history: other }
    pure (R.div { children: [ R.text (show (obj unit)) ] })
"""

-- The `$`-piped hook regression: `useState' $ init` is an `ExprOp`, not an
-- `ExprApp` — `appHeadName` must see through it to classify the setter as
-- `StableSemantic` (dropped from keys). This repo's own `history` bind uses
-- exactly this shape. If the setter were misclassified `Unstable` (the bug
-- this pins), it would leak into `advance`'s key.
fixtureDollarPipedHook :: String
fixtureDollarPipedHook =
  """module Fixture where

import Prelude
import React.Basic.DOM as R
import React.Basic.Hooks (Component, component, useState')
import React.Basic.Hooks as React
import Data.Tuple.Nested ((/\))

mkPiped :: Component Unit
mkPiped = component "Piped" \_ ->
  React.do
    history /\ setHistory <- useState' $ initial 9
    step /\ setStep <- useState' 0
    let
      advance = \_ -> setHistory (step + 1)
    pure (R.div { children: [ R.text (show (advance unit)) ] })
"""

-- The `#`-piped hook regression: `init # useState'` is also an `ExprOp`,
-- but unlike `$`, `#` puts the function on its RIGHT operand
-- (`#` = `flip ($)`). If `appHeadName` always recursed left (the bug this
-- pins), `setHistory` would be misclassified `Unstable` and leak into
-- `advance`'s key.
fixtureHashPipedHook :: String
fixtureHashPipedHook =
  """module Fixture where

import Prelude
import React.Basic.DOM as R
import React.Basic.Hooks (Component, component, useState')
import React.Basic.Hooks as React
import Data.Tuple.Nested ((/\))

mkPiped :: Component Unit
mkPiped = component "Piped" \_ ->
  React.do
    history /\ setHistory <- initial 9 # useState'
    step /\ setStep <- useState' 0
    let
      advance = \_ -> setHistory (step + 1)
    pure (R.div { children: [ R.text (show (advance unit)) ] })
"""

-- The final `pure` tail must run the same purity guard as an ordinary
-- let-binding. Neither state variable is referenced (empty closure, so
-- never-hits pruning doesn't apply either) and the tail builds JSX (so the
-- cost floor doesn't apply either) -- absent the guard this would get
-- wrapped in `React.useMemo unit`, making the effectful call run once ever
-- instead of on every render.
fixtureUnsafeTail :: String
fixtureUnsafeTail =
  """module Fixture where

import Prelude
import React.Basic.DOM as R
import React.Basic.Hooks (Component, component, useState')
import React.Basic.Hooks as React
import Data.Tuple.Nested ((/\))
import Effect.Unsafe (unsafePerformEffect)

mkUnsafe :: Component Unit
mkUnsafe = component "Unsafe" \_ ->
  React.do
    n /\ setN <- useState' 0
    m /\ setM <- useState' 0
    pure (unsafePerformEffect (pure (R.div { children: [ R.text "static" ] })))
"""

-- A component with zero raw `useState`/`useState'`/`useReducer` results
-- (only a `useRef`, which is `StableDirect` and never counts as state).
-- `stateNamesSet` is empty here, so `Set.subset` on it is vacuously true
-- against any closure -- absent the guard, `thing` would be wrongly
-- pruned as "can never hit" even though it depends on nothing unstable at
-- all.
fixtureNoState :: String
fixtureNoState =
  """module Fixture where

import Prelude
import React.Basic.DOM as R
import React.Basic.Hooks (Component, component, useRef)
import React.Basic.Hooks as React

mkNoState :: Component Unit
mkNoState = component "NoState" \_ ->
  React.do
    ref <- useRef 0
    let
      thing = \_ -> R.div { children: [] }
    pure (R.div { children: [ thing unit ] })
"""

-- The idempotency regression: `already` is shaped exactly like this
-- tool's own previously-emitted output (as if re-running on already-
-- transformed code). If `classifyDoBind` doesn't recognize `useMemo`
-- (the bug this pins), `already` falls through to `Unstable` and is
-- wrongly added to `stateNames` as phantom state -- `thing` (whose real
-- closure is just `n`, the only actual state) then fails the
-- never-hits-pruning subset check (`{n, already} ⊆ {n}` is false) and
-- gets wastefully memoized instead of staying plain.
fixtureReprocessedMemo :: String
fixtureReprocessedMemo =
  """module Fixture where

import Prelude
import React.Basic.DOM as R
import React.Basic.Hooks (Component, component, useState')
import React.Basic.Hooks as React
import Data.Tuple.Nested ((/\))

mkIdempotent :: Component Unit
mkIdempotent = component "Idempotent" \_ ->
  React.do
    n /\ setN <- useState' 0
    already <- React.useMemo (React.UnsafeReference n) \_ -> n + 1
    let
      thing = \_ -> R.div { children: [ R.text (show n) ] }
    pure (R.div { children: [ thing unit ] })
"""

main :: Effect Unit
main = runTest do
  suite "PursMemo.Transform" do
    test
      "mkGame shape: prunes cost-floor/never-hits bindings, memoizes the survivor and the tail"
      do
        withTransformed "fixtureMkGame" fixtureMkGame \out -> do
          Assert.assert "currentSquares stays plain (cost floor)"
            (not (contains "currentSquares <- React.useMemo" out))
          Assert.assert "moves is memoized (survives never-hits pruning)"
            (contains "moves <- React.useMemo" out)
          Assert.assert "moves's key omits currentMove"
            (not (contains "UnsafeReference currentMove" out))
          Assert.assert "the pure tail is split into a useMemo bind"
            ( contains "memoized <- React.useMemo" out && contains
                "pure memoized"
                out
            )

    test
      "a useEffect-shaped statement pins insertion order across two eligible lets"
      do
        withTransformed "fixtureInsertionPoint" fixtureInsertionPoint \out ->
          case
            indexOf "squared <-" out,
            indexOf "useEffect n" out,
            indexOf "doubled <-" out
            of
            Just i1, Just i2, Just i3 -> Assert.assert
              "order: squared < useEffect < doubled"
              (i1 < i2 && i2 < i3)
            _, _, _ -> Assert.assert
              "expected all three markers in the output"
              false

    test "a mutually recursive let group stays untransformed" do
      withTransformed "fixtureMutualRecursion" fixtureMutualRecursion \out ->
        Assert.equal 0 (occurrences "useMemo" out)

    test "plain self-recursion is not mistaken for mutual recursion" do
      withTransformed "fixtureSelfRecursion" fixtureSelfRecursion \out ->
        Assert.assert "fact is memoized (self-edge doesn't block a verdict)"
          (contains "fact <- React.useMemo" out)

    test "a guarded let binding stays untransformed" do
      withTransformed "fixtureGuarded" fixtureGuarded \out ->
        Assert.equal 0 (occurrences "useMemo" out)

    test "a record field label is never collected as a free variable" do
      withTransformed "fixtureRecordLabel" fixtureRecordLabel \out -> do
        Assert.assert "obj is memoized" (contains "obj <- React.useMemo" out)
        Assert.assert "the label `history` never leaks into a key"
          (not (contains "UnsafeReference history" out))
        Assert.assert "the real reference `other` is the key"
          (contains "UnsafeReference other" out)

    test "a useState' setter bound via `$` is still dropped from a key" do
      withTransformed "fixtureDollarPipedHook" fixtureDollarPipedHook \out -> do
        Assert.assert "advance is memoized"
          (contains "advance <- React.useMemo" out)
        Assert.assert "the setter never leaks into a key"
          (not (contains "UnsafeReference setHistory" out))
        Assert.assert "the real dependency `step` is the key"
          (contains "UnsafeReference step" out)

    test "a useState' setter bound via `#` is still dropped from a key" do
      withTransformed "fixtureHashPipedHook" fixtureHashPipedHook \out -> do
        Assert.assert "advance is memoized"
          (contains "advance <- React.useMemo" out)
        Assert.assert "the setter never leaks into a key"
          (not (contains "UnsafeReference setHistory" out))
        Assert.assert "the real dependency `step` is the key"
          (contains "UnsafeReference step" out)

    test "a final pure tail mentioning unsafePerformEffect stays untransformed"
      do
        withTransformed "fixtureUnsafeTail" fixtureUnsafeTail \out ->
          Assert.equal 0 (occurrences "useMemo" out)

    test
      "a component with no raw useState/useReducer state still memoizes an eligible binding"
      do
        withTransformed "fixtureNoState" fixtureNoState \out ->
          Assert.assert
            "thing is memoized (empty stateNamesSet doesn't vacuously prune it)"
            (contains "thing <- React.useMemo" out)

    test
      "a DoBind shaped like this tool's own useMemo output is classified MemoizedResult, not phantom state"
      do
        withTransformed "fixtureReprocessedMemo" fixtureReprocessedMemo \out ->
          Assert.assert
            "thing stays plain (already isn't wrongly added to stateNames)"
            (not (contains "thing <- React.useMemo" out))
