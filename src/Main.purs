module Main where

import Prelude

import Control.Monad.Eff

import Data.Maybe.Unsafe (fromJust)
import Data.Nullable (toMaybe)

import DOM (DOM())
import DOM.HTML (window)
import DOM.HTML.Document (body)
import DOM.HTML.Types (htmlElementToElement)
import DOM.HTML.Window (document)

import DOM.Node.Types (Element())

import React
import qualified React.DOM as D
import qualified React.DOM.Props as P

import Life

incrementCounter ctx e = do
  val <- readState ctx
  writeState ctx (val + 1)

counter = createClass $ spec 5 \ctx -> do
  val <- readState ctx
  return $ D.p [ P.className "Counter"
               , P.onClick (incrementCounter ctx)
               ]
               [ D.text (show val)
               , D.text " Click me to increment!"
               ]


board = createClass $ spec unit renderBoard

renderBoard ctx = do
  props <- getProps ctx
  return $ D.div [] ((map renderCell) props.board)

renderCell cell = do
  D.div [] $ makeCell <$> cell

className :: Cell -> String
className Alive = "cell alive"
className Dead = "cell dead"

makeCell val = D.div [P.className (className val)] []

gameState = {board: Life.nextGeneration Life.testBoard}

main = container >>= render ui
  where
  ui :: ReactElement
  ui = D.div [] [ createFactory board gameState,
                  createFactory counter {} ]

  container :: forall eff. Eff (dom :: DOM | eff) Element
  container = do
    win <- window
    doc <- document win
    elm <- fromJust <$> toMaybe <$> body doc
    return $ htmlElementToElement elm
