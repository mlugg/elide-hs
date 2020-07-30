module IR where

import Data.Text(Text)

data IRType
  = IR8
  | IR16
  | IR32
  | IR64
  | IRF32
  | IRF64
  | IRStruct Integer -- size in bytes
  deriving (Show, Eq)

data IRLiteral
  = IRLit8 Integer
  | IRLit16 Integer
  | IRLit32 Integer
  | IRLit64 Integer
  | IRFLit32 Double
  | IRFLit64 Double
  | IRFLit80 Double
  deriving (Show, Eq)

data IROperator
  = IROpAdd
  | IROpSub
  | IROpMul
  | IROpDiv
  | IROpLShift
  | IROpRShift
  | IROpGT
  | IROpGTE
  | IROpLT
  | IROpLTE
  | IROpAnd
  | IROpOr
  | IROpXor
  | IROpEqual
  | IROpNEqual
  deriving (Show, Eq)

type IRBlock = [IRNode]

data IRNode
  -- Structure
  = IRIf IRBlock IRBlock IRBlock -- cond true false
  | IRWhile IRBlock IRBlock -- cond body

  -- Control - parameter is number of loops to break/continue out of
  | IRContinue Integer
  | IRBreak Integer

  -- Pops the given args (start of list = first popped = last function
  -- arg), then pops a function pointer and calls it
  | IRCall [IRType]

  -- Calls a naked function by name
  | IRNakedCall Text

  -- Looks up a global symbol and pushes a pointer to it onto the stack
  | IRLookup Text

  -- Pushes a literal onto the stacck
  | IRPushLit IRLiteral

  -- Pushes the value at the given offset into the stack frame
  | IRGetOffset Integer IRType

  -- Pops a value and uses it to replace the value at the given offset
  -- into the stack frame
  | IRSetOffset Integer IRType

  -- Pops a pointer, and dereferences it as the given type, pushing the
  -- value
  | IRDeref IRType

  -- Pops a value of the given type and discards it
  | IRPop IRType

  -- Pops a value, negates / binary NOTs it, and pushes the result
  | IRNegate IRType
  | IRInvert IRType

  -- Pops a then b of the given type, and pushes a <op> b
  | IRBinOp IRType IROperator
  deriving (Show, Eq)
