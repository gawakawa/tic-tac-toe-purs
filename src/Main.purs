module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic (JSX, fragment)
import React.Basic.DOM as R
import React.Basic.DOM.Client (createRoot, renderRoot)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

square :: String -> JSX
square value = R.button { className: "square", children: [ R.text value ] }

board :: JSX
board = fragment
  [ R.div
      { className: "board-row"
      , children: [ square "1", square "2", square "3" ]
      }
  , R.div
      { className: "board-row"
      , children: [ square "4", square "5", square "6" ]
      }
  , R.div
      { className: "board-row"
      , children: [ square "7", square "8", square "9" ]
      }
  ]

main :: Effect Unit
main = do
  doc <- document =<< window
  root <- getElementById "root" $ toNonElementParentNode doc
  case root of
    Nothing -> throw "Could not find container element"
    Just container -> do
      reactRoot <- createRoot container
      renderRoot reactRoot board
