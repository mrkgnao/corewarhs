{-# LANGUAGE TemplateHaskell #-}
module Player where

import           Control.Lens

import qualified Graphics.Gloss as G

data Player = Player
  { _name      :: String
  , _cellColor :: G.Color
  } deriving (Show, Eq)

makeLenses ''Player
