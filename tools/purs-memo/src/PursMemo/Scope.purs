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
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Tuple (snd)
import PureScript.CST.Traversal (foldMapExpr)
import PureScript.CST.Types (Binder(..), Expr(..), Name(..), QualifiedName(..), RecordLabeled(..), Separated(..), Wrapped(..))

separatedItems :: forall a. Separated a -> Array a
separatedItems (Separated { head, tail }) = [ head ] <> map snd tail

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

  punName :: forall a. RecordLabeled a -> Array String
  punName = case _ of
    RecordPun (Name { name }) -> [ unwrap name ]
    RecordField _ _ _ -> []

-- | Every name a binder introduces, recursively. Used both for `DoBind`
-- | binders (`history /\ setHistory <- ...`) and for record/constructor
-- | patterns nested inside them.
boundNames :: Binder Void -> Array String
boundNames = case _ of
  BinderVar (Name { name }) -> [ unwrap name ]
  BinderNamed (Name { name }) _ inner -> [ unwrap name ] <> boundNames inner
  BinderConstructor _ binders -> foldMap boundNames binders
  BinderArray (Wrapped { value }) -> maybe []
    (foldMap boundNames <<< separatedItems)
    value
  BinderRecord (Wrapped { value }) -> maybe []
    (foldMap fieldNames <<< separatedItems)
    value
  BinderParens (Wrapped { value }) -> boundNames value
  BinderTyped inner _ _ -> boundNames inner
  BinderOp inner ops -> boundNames inner <> foldMap (boundNames <<< snd)
    (NEA.toArray ops)
  BinderWildcard _ -> []
  BinderBoolean _ _ -> []
  BinderChar _ _ -> []
  BinderString _ _ -> []
  BinderInt _ _ _ -> []
  BinderNumber _ _ _ -> []
  BinderError e -> absurd e
  where
  fieldNames :: RecordLabeled (Binder Void) -> Array String
  fieldNames = case _ of
    RecordPun (Name { name }) -> [ unwrap name ]
    RecordField _ _ b -> boundNames b
