module Main where

import Prelude

import Control.Alternative (guard)
import Data.Array (findMap, replicate, updateAt, (!!))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Tuple.Nested (type (/\), (/\))
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
    xIsNext /\ setXIsNext <- useState' $ true
    squares /\ setSquares <- useState' $ replicate 9 Nothing
    let
      handleClick :: Int -> Effect Unit
      handleClick i = do
        let alreadyFilled = isJust $ join $ squares !! i
        let hasWinner = isJust $ calculateWinner squares
        unless (alreadyFilled || hasWinner) do
          let nextSquare = Just $ if xIsNext then "X" else "O"
          setSquares $ fromMaybe squares $ updateAt i nextSquare squares
          setXIsNext $ not xIsNext

      status :: String
      status = case calculateWinner squares of
        Just winner -> "Winner: " <> winner
        Nothing -> "Next player: " <> if xIsNext then "X" else "O"
    pure $ fragment
      [ R.div { className: "status", children: [ R.text status ] }
      , R.div
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

lines :: Array (Int /\ Int /\ Int)
lines =
  [ 0 /\ 1 /\ 2
  , 3 /\ 4 /\ 5
  , 6 /\ 7 /\ 8
  , 0 /\ 3 /\ 6
  , 1 /\ 4 /\ 7
  , 2 /\ 5 /\ 8
  , 0 /\ 4 /\ 8
  , 2 /\ 4 /\ 6
  ]

calculateWinner :: Array (Maybe String) -> Maybe String
calculateWinner squares = findMap checkLine lines
  where
  checkLine :: Int /\ Int /\ Int -> Maybe String
  checkLine (a /\ b /\ c) = do
    squareA <- join $ squares !! a
    squareB <- join $ squares !! b
    squareC <- join $ squares !! c
    guard $ squareA == squareB && squareB == squareC
    pure squareA

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
