{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
module VM where

import           Asm
import           AsmInst
import           Core
import           Player
import           Settings

import           Control.Lens
import           Control.Monad.State

import           Control.Monad.Except

import           Data.Maybe           (fromMaybe)

import           Graphics.Gloss       (Color)
import qualified Graphics.Gloss       as G

data Process = Process
  { _ptr     :: Int
  , _isAlive :: Bool
  , _player  :: Player
  } deriving (Show)

mkProcess :: Int -> Color -> String -> Process
mkProcess x c s = Process x True (Player s c)

type CoreT e s a = ExceptT e ((StateT s a))
type CoreM m = CoreT String VMState m

data VMState = VMState
  { _core  :: Core
  , _procs :: [Process]
  } deriving (Show)

data VMException = VMException
  { _info :: String
  }

makeLenses ''Process
makeLenses ''VMState

circularIx i bound = ix (i `mod` bound)

cellAt :: Int -> Lens' VMState Cell
cellAt i = lens (getCell i) (\vm newInst -> vm & (helper i vm .~ newInst))
  where
    getCell i vm = vm ^?! helper i vm
    helper i vm = core . cells . circularIx i (bound vm)
    bound vm = vm ^. core . cells . to length

instrAt i = cellAt i . inst

procAt i = procs . ix i

initialVM :: VMState
initialVM =
  -- over core (setInstAt 0 (mkInstruction MOV ModAB (Immediate, 10) (Direct, 3))) $
  over
    core
    (setInstAt 0 mov01) --(mkInstruction JMP ModI (Immediate, 3) (Immediate, 0)))
    zeroed
  where
    zeroed = VMState (mkCore defaultSettings) [mkProcess 0 G.green "dude"] --, mkProcess 1]

-- | TODO I apologise for this code in the unlikely event that someone reads this.

step :: MonadIO m => Int -> CoreM m ()
step i = do
  vm <- get
  Process{..} <- gets ((!! i) . _procs)
  if _isAlive
    then do
      liftIO $ putStrLn $ "Stepping process " ++ show i ++ " at " ++ show _ptr
      inst <- gets (^. instrAt i)
      liftIO $ putStrLn (show inst)
      inc <- step' i _ptr inst
      liftIO $ putStrLn (show inc)
      procAt i . ptr += (maybe 0 id inc)
      cellAt _ptr . owner .= (Just _player)
      gets _core >>= liftIO . putStrLn .
        ("Core state after executing instruction:\n" ++) .
        show
    else do
      liftIO $ putStrLn "Dead process"
  where
    incrementBy
      :: Monad m
      => a -> m (Maybe a)
    incrementBy = return . Just
    step'
      :: MonadIO m
      => Int -> Int -> AsmInst -> CoreM m (Maybe Int)
    step' i ptr' (AsmInst MOV ModI (Operand Direct av) (Operand Direct bv)) = do
      instrAt (ptr' + bv) <~ use (instrAt (ptr' + av))
      incrementBy 1
    step' i ptr' (AsmInst MOV ModAB (Operand Immediate av) (Operand Direct bv)) = do
      instrAt bv . bval .= av
      incrementBy 1
    step' i ptr' (AsmInst MOV ModB (Operand Direct av) (Operand Immediate bv)) = do
      instrAt bv . aval .= bv
      incrementBy 1
    step' i _ (AsmInst JMP ModI (Operand Immediate v) _) = do
      procAt i . ptr .= v
      return Nothing
    step' _ _ (AsmInst NOP _ _ _) = incrementBy 1
    step' i ptr' (AsmInst DAT _ _ _) = do
      liftIO $ putStrLn $ "Process " ++ show i ++ " killed at " ++ show ptr'
      procAt i . isAlive .= False
      return Nothing
    step' _ _ _ = throwError "unimplemented"

stepAll :: MonadIO m => CoreM m ()
stepAll = do
  numProcs <- gets (length . _procs)
  mapM_ step [0..numProcs -1]

execute :: MonadIO m => CoreT e s m a -> s -> m (Either e a, s)
execute = runStateT . runExceptT
