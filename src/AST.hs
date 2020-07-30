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
  | VTFunc [RefType] ValType -- args ret
  deriving (Show, Eq)

data RefType = RefType
  { refMut :: Bool
  , refVol :: Bool
  , refValTo :: ValType
  } deriving (Show, Eq)

-- }}}

-- Expressions {{{

data Literal
  = LBool Bool
  | LInt Signed IntSize Integer
  | LFloat FloatSize Double
  | LArray [Expr]
  | LStruct [Expr]
  | LFunc [(Text, RefType)] ValType Expr -- args ret body
  deriving (Show, Eq)

data Expr
  = ELiteral Literal
  | EIdent Text

  | ECall Expr [Expr] -- func args

  | EIf Expr Expr (Maybe Expr) -- cond true [false]
  | EWhile Text Expr Expr -- label cond body

  | EBreak (Maybe Text) -- [label]
  | EContinue (Maybe Text) -- [label]
  | EReturn (Maybe Expr) -- [val]

  | EFieldAccess Expr Text -- expr field
  | ELet RefType Text Expr Expr -- type name val body
  | ECast ValType Expr -- type expr
  deriving (Show, Eq)

-- }}}

-- Top-level constructs {{{

data TopLevel
  = TLFunc Text [(Text, RefType)] ValType Expr -- name args ret body
  | TLDecl Text RefType Expr -- name type val
  | TLNamespace Text [TopLevel]
  deriving (Show, Eq)

-- }}}
