{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module AsmInst where

import           Control.Lens
import           Data.Maybe   (fromJust)

type Value = Int

data Field
  = FieldA
  | FieldB
  deriving (Show, Eq)

data PointerChange
  = None
  | PreDec
  | PostInc
  deriving (Show, Eq)

data AddressMode
  = Immediate
  | Direct
  | Indirect Field
             PointerChange
  deriving (Show, Eq)

data Modifier
  = ModA
  | ModB
  | ModAB
  | ModBA
  | ModF
  | ModX
  | ModI
  deriving (Eq)

instance Show Modifier where
  show ModA  = "A "
  show ModB  = "B "
  show ModAB = "AB"
  show ModBA = "BA"
  show ModF  = "F "
  show ModX  = "X "
  show ModI  = "I "

toSymbol Immediate = "#"
toSymbol Direct = " " -- "$" is optional
toSymbol (Indirect f p) = fromJust $ lookup (f, p) pairs
  where
    pairs =
      [ ((FieldA, None), "*")
      , ((FieldB, None), "@")
      , ((FieldA, PreDec), "{")
      , ((FieldB, PreDec), "<")
      , ((FieldA, PostInc), "}")
      , ((FieldB, PostInc), ">")
      ]

data Operand = Operand
  { _mode  :: AddressMode
  , _value :: Value
  } deriving (Eq)

instance Show Operand where
  show (Operand m v) = toSymbol m ++ (show v)

data AsmInst = AsmInst
  { _opcode   :: Opcode
  , _modifier :: Modifier
  , _a        :: Operand
  , _b        :: Operand
  } deriving (Eq)

instance Show AsmInst where
  show AsmInst {..} =
    concat [show _opcode, ".", show _modifier, " ", show _a, ", ", show _b]

dat00 :: AsmInst
dat00 = mkInstruction DAT ModF (Direct, 0) (Direct, 0)

defaultInst :: AsmInst
defaultInst = dat00

mkInstruction :: Opcode
              -> Modifier
              -> (AddressMode, Value)
              -> (AddressMode, Value)
              -> AsmInst
mkInstruction opcode mod (amode, aval) (bmode, bval) =
  AsmInst opcode mod afield bfield
  where
    afield = Operand {_mode = amode, _value = aval}
    bfield = Operand {_mode = bmode, _value = bval}

data Opcode
  = DAT -- ^ "Data".
        -- ^ Kills the process.
  | MOV -- ^ "Move".
        -- ^ Copies data from one address to another.
  | ADD -- ^ "Add".
        -- ^ Adds one number to another.
  | SUB -- ^ "Subtract".
        -- ^ Subtracts one number from another.
  | MUL -- ^ "Multiply".
        -- ^ Multiplies one number with another.
  | DIV -- ^ "Divide".
        -- ^ Divides one number with another.
  | MOD -- ^ "Modulus".
        -- ^ Divides one number with another and gives the remainder.
  | JMP -- ^ "Jump".
        -- ^ Continues execution from another address.
  | JMZ -- ^ "Jump if zero".
        -- ^ Tests a number and jumps to an address if it's 0.
  | JMN -- ^ "Jump if not zero".
        -- ^ Tests a number and jumps if it isn't 0.
  | DJN -- ^ "Decrement and jump if not zero".
        -- ^ Decrements a number by one, and jumps unless the result is 0.
  | SPL -- ^ "Split".
        -- ^ Starts a second process at another address.
  | CMP -- ^ "Compare".
        -- ^ Same as SEQ.
  | SEQ -- ^ "Skip if equal".
        -- ^ Compares two instructions, and skips the next instruction if they are equal.
  | SNE -- ^ "Skip if not equal".
        -- ^ Compares two instructions, and skips the next instruction if they aren't equal.
  | SLT -- ^ "Skip if lower than".
        -- ^ Compares two values, and skips the next instruction if the first is lower than the second.
  | LDP -- ^ "Load from p-space".
        -- ^ Loads a number from private storage space.
  | STP -- ^ "Save to p-space".
        -- ^ Saves a number to private storage space.
  | NOP -- ^ "No operation".
        -- ^ Does nothing.
  deriving (Show, Read, Eq, Enum)

makeLenses ''Operand
makeLenses ''AsmInst

aval :: Lens' AsmInst Value
aval = a . value

bval :: Lens' AsmInst Value
bval = b . value
