module Asm where

import           AsmInst

data Program =
  Program [AsmInst]

mov01 = mkInstruction MOV (Direct, 0) (Direct, 1)

imp :: Program
imp = Program [mov01]
