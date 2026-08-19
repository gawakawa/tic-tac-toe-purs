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

board :: JSX
board = fragment
  [ R.div
      { className: "board-row"
      , children:
          [ R.button { className: "square", children: [ R.text "1" ] }
          , R.button { className: "square", children: [ R.text "2" ] }
          , R.button { className: "square", children: [ R.text "3" ] }
          ]
      }
  , R.div
      { className: "board-row"
      , children:
          [ R.button { className: "square", children: [ R.text "4" ] }
          , R.button { className: "square", children: [ R.text "5" ] }
          , R.button { className: "square", children: [ R.text "6" ] }
          ]
      }
  , R.div
      { className: "board-row"
      , children:
          [ R.button { className: "square", children: [ R.text "7" ] }
          , R.button { className: "square", children: [ R.text "8" ] }
          , R.button { className: "square", children: [ R.text "9" ] }
          ]
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
