module Main where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic (JSX, fragment)
import React.Basic.DOM as R
import React.Basic.DOM.Client (createRoot, renderRoot)
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, component, useState')
import React.Basic.Hooks as React
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

mkSquare :: Component Unit
mkSquare = component "Square" \_ ->
  React.do
    value /\ setValue <- useState' Nothing
    let
      handleClick :: Effect Unit
      handleClick = setValue $ Just "X"
    pure $ R.button
      { className: "square"
      , onClick: handler_ handleClick
      , children: [ R.text $ fromMaybe "" value ]
      }

mkBoard :: (Unit -> JSX) -> Component Unit
mkBoard square = component "Board" \_ ->
  pure $ fragment
    [ R.div
        { className: "board-row"
        , children: [ square unit, square unit, square unit ]
        }
    , R.div
        { className: "board-row"
        , children: [ square unit, square unit, square unit ]
        }
    , R.div
        { className: "board-row"
        , children: [ square unit, square unit, square unit ]
        }
    ]

main :: Effect Unit
main = do
  square <- mkSquare
  board <- mkBoard square
  doc <- document =<< window
  root <- getElementById "root" $ toNonElementParentNode doc
  case root of
    Nothing -> throw "Could not find container element"
    Just container -> do
      reactRoot <- createRoot container
      renderRoot reactRoot $ board unit
