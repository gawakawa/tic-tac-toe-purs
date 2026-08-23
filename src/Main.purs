module Main where

import Prelude

import Control.Alternative (guard)
import Data.Array (findMap, replicate, updateAt, (!!))
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Tuple.Nested (type (/\), (/\))
import Debug (todo)
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

data Player = X | O

type Mark = Player

type Squares = Array (Maybe Mark)

derive instance eqPlayer :: Eq Player

instance showPlayer :: Show Player where
  show X = "X"
  show O = "O"

type SquareProps =
  { value :: Maybe Mark
  , onSquareClick :: Effect Unit
  }

square :: SquareProps -> JSX
square { value, onSquareClick } = R.button
  { className: "square"
  , onClick: handler_ onSquareClick
  , children: [ R.text $ maybe "" show value ]
  }

type BoardProps =
  { xIsNext :: Boolean
  , squares :: Squares
  , onPlay :: Squares -> Effect Unit
  }

board :: BoardProps -> JSX
board { xIsNext, squares, onPlay } =
  let
    winner :: Maybe Player
    winner = calculateWinner squares

    handleClick :: Int -> Effect Unit
    handleClick i = do
      let alreadyFilled = isJust $ join $ squares !! i
      unless (alreadyFilled || isJust winner) do
        let nextSquare = Just $ if xIsNext then X else O
        onPlay $ fromMaybe squares $ updateAt i nextSquare squares

    status :: String
    status = case winner of
      Just player -> "Winner: " <> show player
      Nothing -> "Next player: " <> (show $ if xIsNext then X else O)
  in
    fragment
      [ R.div { className: "status", children: [ R.text status ] }
      , R.div
          { className: "board-row"
          , children:
              [ square
                  { value: join $ squares !! 0, onSquareClick: handleClick 0 }
              , square
                  { value: join $ squares !! 1, onSquareClick: handleClick 1 }
              , square
                  { value: join $ squares !! 2, onSquareClick: handleClick 2 }
              ]
          }
      , R.div
          { className: "board-row"
          , children:
              [ square
                  { value: join $ squares !! 3, onSquareClick: handleClick 3 }
              , square
                  { value: join $ squares !! 4, onSquareClick: handleClick 4 }
              , square
                  { value: join $ squares !! 5, onSquareClick: handleClick 5 }
              ]
          }
      , R.div
          { className: "board-row"
          , children:
              [ square
                  { value: join $ squares !! 6, onSquareClick: handleClick 6 }
              , square
                  { value: join $ squares !! 7, onSquareClick: handleClick 7 }
              , square
                  { value: join $ squares !! 8, onSquareClick: handleClick 8 }
              ]
          }
      ]

mkGame :: Component Unit
mkGame = component "Game" \_ ->
  React.do
    xIsNext /\ setXIsNext <- useState' true
    history /\ setHistory <- useState' $ NEA.singleton $ replicate 9 Nothing
    let
      currentSquares :: Squares
      currentSquares = NEA.last history

      handlePlay :: Squares -> Effect Unit
      handlePlay nextSquares = do
        setHistory $ NEA.snoc history nextSquares
        setXIsNext $ not xIsNext
    pure $ R.div
      { className: "game"
      , children:
          [ R.div
              { className: "game-board"
              , children:
                  [ board
                      { xIsNext
                      , squares: currentSquares
                      , onPlay: handlePlay
                      }
                  ]
              }
          , R.div { className: "game-info", children: [ R.ol_ [ todo ] ] }
          ]
      }

type Line = Int /\ Int /\ Int

lines :: Array Line
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

calculateWinner :: Squares -> Maybe Player
calculateWinner squares = findMap checkLine lines
  where
  checkLine :: Line -> Maybe Player
  checkLine (a /\ b /\ c) = do
    squareA <- join $ squares !! a
    squareB <- join $ squares !! b
    squareC <- join $ squares !! c
    guard $ squareA == squareB && squareB == squareC
    pure squareA

main :: Effect Unit
main = do
  game <- mkGame
  doc <- document =<< window
  root <- getElementById "root" $ toNonElementParentNode doc
  case root of
    Nothing -> throw "Could not find container element"
    Just container -> do
      reactRoot <- createRoot container
      renderRoot reactRoot $ game unit
