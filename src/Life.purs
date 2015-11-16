module Life where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Data.Array
import Control.MonadPlus (guard)
import Data.Maybe
import Data.Foldable

data Cell = Alive | Dead

instance showCell :: Show Cell where
  show Alive = "Alive"
  show Dead = "Dead"

type Board = Array (Array Cell)

testBoard :: Board
testBoard = [[Dead, Alive, Dead],[Dead, Alive, Dead],[Dead, Alive, Dead]]

getCellState :: Board -> Int -> Int -> Cell
getCellState board row col =
  case state of
    (Just cell) -> cell
    Nothing -> Dead
  where
  state = do
    row <- (index board row)
    cell <- (index row col)
    return cell

neighbors row col = do
  i <- -1 .. 1
  j <- -1 .. 1
  guard $ i /= 0 || j /= 0
  return [ row + i, col + j ]

neighborCount board row col = count
  where
  states = map (\[row, col] -> getCellState board row col) (neighbors row col)
  cellCount Alive = 1
  cellCount Dead = 0
  count = sum (map cellCount states)

nextCellState :: Cell -> Int -> Cell
nextCellState Alive 2 = Alive
nextCellState _     3 = Alive
nextCellState _ _     = Dead

nextStateForPos board row col =
  nextCellState (getCellState board row col) (neighborCount board row col)

nextGeneration :: Board -> Board
nextGeneration board =
  map nextRow (0..(rows-1))
  where
  row = case (head board) of (Just x) -> x
  rows = length board
  cols = length row
  nextRow :: Int -> Array Cell
  nextRow i = map (nextStateForPos board i) (0..(cols - 1))
