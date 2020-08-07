module AST where

import Data.Text(Text)

-- Types {{{

data IntSize
  = I8
  | I16
  | I32
  | I64
  deriving (Show, Eq)

data FloatSize
  = F32
  | F64
  | F80
  deriving (Show, Eq)

data Signed = Signed | Unsigned
  deriving (Show, Eq)

data ValType
  = VTVoid
  | VTBool
  | VTPtr RefType
  | VTInt Signed IntSize
  | VTFloat FloatSize
  | VTNewtype Text
  | VTStruct [(Text, ValType)]
  | VTUnion [(Text, ValType)]
  | VTFunc [ValType] ValType -- args ret
  deriving (Show, Eq)

data RefType = RefType
  { refMut :: Bool
  , refVol :: Bool
  , refToVal :: ValType
  } deriving (Show, Eq)

-- }}}

-- Expressions {{{

data UnOp
  = UORef
  | UODeref
  | UOPreInc
  | UOPostInc
  | UOPreDec
  | UOPostDec
  | UOBoolNot
  | UOBinNot
  | UOSizeof
  | UOPlus
  | UOMinus
  deriving (Show, Eq)

data BinOp
  = BOAdd
  | BOSub
  | BOMul
  | BODiv
  | BOMod
  | BOBoolAnd
  | BOBoolOr
  | BOBinAnd
  | BOBinOr
  | BOBinXor
  | BOEqual
  | BONEqual
  | BOAssign
  | BOLShift
  | BORShift
  | BOGT
  | BOGTE
  | BOLT
  | BOLTE
  deriving (Show, Eq)

data Literal
  = LBool Bool
  | LInt Signed IntSize Integer
  | LFloat FloatSize Double
  | LArray [Expr]
  | LStruct [Expr]
  | LFunc [(Text, ValType)] ValType Expr -- args ret body
  deriving (Show, Eq)

data Expr
  = ELiteral Literal
  | EIdent Text

  | ECall Expr [Expr] -- func args

  | EIf Expr Expr (Maybe Expr) -- cond true [false]
  | EWhile Text Expr Expr -- label cond body
  | EDefer Expr Expr -- deferred body

  | EBreak (Maybe Text) -- [label]
  | EContinue (Maybe Text) -- [label]
  | EReturn (Maybe Expr) -- [val]

  | EFieldAccess Expr Text -- expr field
  | ELet RefType Text Expr Expr -- type name val body
  | ECast ValType Expr -- type expr

  | EUnOp UnOp Expr
  | EBinOp BinOp Expr Expr
  deriving (Show, Eq)

-- }}}

-- Top-level constructs {{{

data TopLevel
  = TLFunc Text [(Text, ValType)] ValType Expr -- name args ret body
  | TLDecl Text RefType Expr -- name type val
  | TLNamespace Text [TopLevel]
  deriving (Show, Eq)

-- }}}
