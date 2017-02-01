{-# LANGUAGE TemplateHaskell #-}
module Settings
  ( Settings(..)
  , defaultSettings
  , coresize
  ) where

import           Control.Lens

data Settings = Settings
  { _coresize :: Int
  } deriving (Show, Eq)

makeLenses ''Settings

defaultSettings :: Settings
defaultSettings = Settings {_coresize = 8}
