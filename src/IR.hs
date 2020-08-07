module IR where

import Data.Text(Text)

import AST

irPtrType = IRInt Unsigned I64

data IRType
  = IRInt Signed IntSize
  | IRFloat FloatSize
  | IRStruct Integer -- size in bytes
  deriving (Show, Eq)

data IRLiteral
  = IRIntLit Signed IntSize Integer
  | IRFloatLit FloatSize Double
  deriving (Show, Eq)

data IRBinOp
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

data IRUnOp
  = IROpInc
  | IROpDec
  | IROpBinNot
  | IROpNegate
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

  -- Pushes the address of the given offset into the stack frame
  | IRGetOffsetAddr Integer

  -- Pops a pointer, and dereferences it as the given type, pushing the
  -- value
  | IRDeref IRType

  -- Pops a value of the given type followed by a pointer, and sets the
  -- pointer destination to the value
  | IRSetAddr IRType

  -- Pops a value of the given type and discards it
  | IRPop IRType

  -- Pops a 1-byte value, boolean NOTs it, and pushes the result
  | IRNot

  -- Pops a then b of the given type, and pushes a <op> b
  | IRBinOp IRType IRBinOp

  -- Pops a of the given types and pushes <op> a
  | IRUnOp IRType IRUnOp

  -- Duplicates the given type of value on the top of the stack
  | IRDup IRType

  -- Rotates the two values on top of the stack (IRRot2 a b means top of
  -- stack has type a, next val has type b)
  | IRRot2 IRType IRType

  -- Right-rotates the top 3 values on the stack with the given ty[es
  | IRRot3 IRType IRType IRType
  deriving (Show, Eq)
