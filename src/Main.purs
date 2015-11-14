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

board = createClass $ spec [[0, 1, 0], [0, 1, 0], [0, 1, 0]] renderBoard

renderText context = do
  val <- readState context
  return $ D.p [] $ D.text <$> val

renderBoard context = do
  val <- readState context
  return $ D.div [] $ renderCell <$> val

renderCell cell = do
  D.div [] $ makeCell <$> cell

cellStateName val = if val == 1 then "alive" else "dead"

makeCell val = D.div [
  P.className $ "cell " ++ cellStateName val ]
  []

main = container >>= render ui
  where
  ui :: ReactElement
  ui = D.div [] [ createFactory board {},
                  createFactory counter {} ]

  container :: forall eff. Eff (dom :: DOM | eff) Element
  container = do
    win <- window
    doc <- document win
    elm <- fromJust <$> toMaybe <$> body doc
    return $ htmlElementToElement elm
