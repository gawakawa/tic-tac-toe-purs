-- | Positional scope and free-variable analysis over the parsed CST.
-- |
-- | `identsIn` deliberately does NOT distinguish shadowed inner binders (a
-- | lambda/case binder reusing a render-scope name) from real references —
-- | over-including such a name is safe, it just wastes a memo key slot. It
-- | DOES distinguish record field labels from value identifiers, since those
-- | lex identically (`TokLowerName Nothing s` either way) and conflating them
-- | is what breaks the build or produces stale values (see plan §"Claims that
-- | fell", item 1). Labels are excluded structurally, by walking the real AST
-- | instead of a raw token scan.
module PursMemo.Scope
  ( boundNames
  , identsIn
  , separatedItems
  ) where

import Prelude

import Data.Array (foldMap, nub)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Tuple (snd)
import PureScript.CST.Traversal (foldMapBinder, foldMapExpr)
import PureScript.CST.Types (Binder(..), Expr(..), Name(..), QualifiedName(..), RecordLabeled(..), Separated(..), Wrapped(..))

separatedItems :: forall a. Separated a -> Array a
separatedItems (Separated { head, tail }) = [ head ] <> map snd tail

punName :: forall a. RecordLabeled a -> Array String
punName = case _ of
  RecordPun (Name { name }) -> [ unwrap name ]
  RecordField _ _ _ -> []

-- | Every unqualified value identifier referenced in an expression,
-- | including record puns (`{ x }`), which the generic CST traversal skips
-- | entirely (it treats `RecordPun` as an opaque leaf, never invoking a
-- | callback on it). Record field labels (`{ x: e }`, `r.x`, `r { x = e }`)
-- | are excluded: `PureScript.CST.Traversal`'s own record/accessor/update
-- | traversal never recurses into the label position, only the value.
identsIn :: Expr Void -> Array String
identsIn = nub <<< foldMapExpr
  { onExpr
  , onBinder: const mempty
  , onDecl: const mempty
  , onType: const mempty
  }
  where
  onExpr = case _ of
    ExprIdent (QualifiedName { module: Nothing, name }) -> [ unwrap name ]
    ExprRecord (Wrapped { value }) -> maybe []
      (foldMap punName <<< separatedItems)
      value
    _ -> []

-- | Every name a binder introduces, recursively. Used both for `DoBind`
-- | binders (`history /\ setHistory <- ...`) and for record/constructor
-- | patterns nested inside them. Only `BinderVar`/`BinderNamed`'s own name
-- | and record puns need a case here — `PureScript.CST.Traversal`'s generic
-- | recursion already walks every other constructor's children (same
-- | mechanism `identsIn` above relies on for `Expr`), and puns are the one
-- | spot it doesn't recurse into on its own (an opaque leaf, just like the
-- | `Expr` side).
boundNames :: Binder Void -> Array String
boundNames = foldMapBinder
  { onExpr: const mempty
  , onBinder
  , onDecl: const mempty
  , onType: const mempty
  }
  where
  onBinder = case _ of
    BinderVar (Name { name }) -> [ unwrap name ]
    BinderNamed (Name { name }) _ _ -> [ unwrap name ]
    BinderRecord (Wrapped { value }) -> maybe []
      (foldMap punName <<< separatedItems)
      value
    _ -> []
