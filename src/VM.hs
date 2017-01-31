{-# LANGUAGE TemplateHaskell #-}
module VM where

import           Asm
import           AsmInst
import           Core

import           Control.Lens
import           Control.Monad.State

import           Control.Monad.Except

data Mem = Mem
  { _core :: Core
  , _ptr  :: CoreIx
  } deriving (Show)

data VMException = VMException { _info :: String
                               }

makeLenses ''Mem

type MachineM = ExceptT String (StateT Mem IO)

initialMem :: Mem
initialMem = over core (setInstAt 0 mov01) zeroed
  where zeroed = Mem mkCore 0

incPtr :: MachineM ()
incPtr = do
  vm <- get
  liftIO $ putStrLn "hai"
  modify $ over ptr (+1)

evalInst :: AsmInst -> MachineM ()
evalInst (AsmInst MOV (Operand Direct av) (Operand Direct bv)) = do
  vm <- get
  modify $ over core (copyFromTo av bv)

execute :: ExceptT e (StateT s m) a -> s -> m (Either e a, s)
execute = runStateT . runExceptT

-- evalInst :: AsmInst -> State VM ()
