module Main where

import Prelude

import Data.Array (replicate, updateAt, (!!))
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

square :: Maybe String -> Effect Unit -> JSX
square value onSquareClick = R.button
  { className: "square"
  , onClick: handler_ onSquareClick
  , children: [ R.text $ fromMaybe "" value ]
  }

mkBoard :: Component Unit
mkBoard = component "Board" \_ ->
  React.do
    squares /\ setSquares <- useState' $ replicate 9 Nothing
    let
      handleClick :: Int -> Effect Unit
      handleClick i = setSquares $ fromMaybe squares $ updateAt i (Just "X")
        squares
    pure $ fragment
      [ R.div
          { className: "board-row"
          , children:
              [ square (join $ squares !! 0) (handleClick 0)
              , square (join $ squares !! 1) (handleClick 1)
              , square (join $ squares !! 2) (handleClick 2)
              ]
          }
      , R.div
          { className: "board-row"
          , children:
              [ square (join $ squares !! 3) (handleClick 3)
              , square (join $ squares !! 4) (handleClick 4)
              , square (join $ squares !! 5) (handleClick 5)
              ]
          }
      , R.div
          { className: "board-row"
          , children:
              [ square (join $ squares !! 6) (handleClick 6)
              , square (join $ squares !! 7) (handleClick 7)
              , square (join $ squares !! 8) (handleClick 8)
              ]
          }
      ]

main :: Effect Unit
main = do
  board <- mkBoard
  doc <- document =<< window
  root <- getElementById "root" $ toNonElementParentNode doc
  case root of
    Nothing -> throw "Could not find container element"
    Just container -> do
      reactRoot <- createRoot container
      renderRoot reactRoot $ board unit
