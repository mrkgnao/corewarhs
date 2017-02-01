module Lib
    ( simulate
    ) where

import           Graphics.Gloss                       hiding (simulate)
import           Graphics.Gloss.Data.ViewPort         (ViewPort)
import           Graphics.Gloss.Interface.IO.Simulate (simulateIO)

import           Control.Monad.Except
import           Player

import           Control.Lens
import           Data.Maybe                           (catMaybes, fromMaybe)

import           Core
import           VM

width = 640

height = 480

window :: Display
window = InWindow "Nice Window" (width, height) (20,20)

background :: Color
background = black

fps :: Int
fps = 10

someFunc :: IO ()
someFunc = putStrLn "someFunc"

displayVM :: VMState -> IO Picture
displayVM vm = return $ pictures p
  where
    p =
      map (color white) $
      [ translate
        (totalSquareWidth * (fromIntegral x))
        (totalSquareWidth * (fromIntegral y))
        (drawCell vm ((numSquaresY - y) * numSquaresX + x))
      | x <- [0 .. numSquaresX - 1]
      , y <- [0 .. numSquaresY]
      ]

padding = 2
squareSize = 10
totalSquareWidth = padding + squareSize
numSquaresX = 10
numSquaresY = 10

drawCell :: VMState -> Int -> Picture
drawCell vm i = color c $ rectangleSolid squareSize squareSize
  where
    -- table = (\c -> _player c) <$> vm ^. core . cells
    -- c = fromMaybe red (lookup i table)
    c = red

updateVM :: ViewPort -> Float -> VMState -> IO VMState
updateVM _ dt prev = do
  (result, next) <- execute stepAll prev
  case result of
    Left err -> putStrLn err
    Right () -> return ()
  return next

simulate :: IO ()
simulate = do
  simulateIO window background fps initialVM displayVM updateVM
