-- | Positional scope and free-variable analysis over the parsed CST.
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

-- | Every unqualified value identifier referenced, including record puns; field labels are excluded.
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

-- | Every name a binder introduces, recursively (including nested record/constructor patterns).
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
