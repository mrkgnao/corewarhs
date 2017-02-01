{-# LANGUAGE TemplateHaskell #-}
module Core where

import           Data.Vector      (Vector, (!), (//))
import qualified Data.Vector      as V
import           Data.Vector.Lens

import           Control.Lens

import           Asm
import           AsmInst
import           Player
import           Settings

data Core = Core
  { _cells :: Vector Cell
  } deriving (Eq)

instance Show Core where
  show (Core cells) = unlines $ V.toList $ fmap show cells

data Cell = Cell
  { _inst  :: AsmInst
  , _owner :: Maybe Player
  } deriving (Eq)

instance Show Cell where
  show (Cell inst owner) = show inst

makeLenses ''Core
makeLenses ''Cell

mkCore :: Settings -> Core
mkCore settings =
  Core
  { _cells = V.replicate (settings ^. coresize) (Cell defaultInst Nothing)
  }

copyFromTo :: Int -> Int -> Core -> Core
copyFromTo src dest (Core cells) = Core (cells // [(dest, cells ! src)])

setCellAt :: Int -> Cell -> Core -> Core
setCellAt i cell (Core cells) = Core (cells // [(i, cell)])

setInstAt :: Int -> AsmInst -> Core -> Core
setInstAt i inst (Core cells) =
  Core (cells // [(i, (cells ! i) {_inst = inst})])
