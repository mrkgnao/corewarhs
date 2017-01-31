{-# LANGUAGE TemplateHaskell #-}
module Core
  ( Core(..)
  , CoreIx
  , mkCore
  , copyFromTo
  , setCellAt
  , setInstAt
  ) where

import           Data.Array   (Array (..), (!), (//))
import qualified Data.Array   as A (array)

import           Control.Lens

import           Asm
import           AsmInst
import           Settings     (coresize)

type CoreIx = Int

data Core = Core
  { _cells :: Array CoreIx Cell
  } deriving (Show, Eq)

data Cell = Cell
  { _inst :: AsmInst
  } deriving (Show, Eq)

makeLenses ''Core
makeLenses ''Cell

mkCore :: Core
mkCore =
  Core
  {_cells = A.array (0, coresize) $ [(x, Cell defaultInst) | x <- [0 .. coresize]]}

copyFromTo :: CoreIx -> CoreIx -> Core -> Core
copyFromTo src dest (Core cells) = Core (cells // [(dest, cells ! src)])

setCellAt :: CoreIx -> Cell -> Core -> Core
setCellAt i cell (Core cells) = Core (cells // [(i, cell)])

setInstAt :: CoreIx -> AsmInst -> Core -> Core
setInstAt i inst (Core cells) = Core (cells // [(i, Cell {_inst = inst})])

-- Utilities

cellAt :: CoreIx -> Core -> Cell
cellAt c ix = undefined
