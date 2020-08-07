{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving, LambdaCase #-}

module Compile where

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Data.Text(Text)
import Data.Map(Map)
import qualified Data.Map as M
import Data.Functor

import AST
import IR
import TypeUtils
import Common

-- Utilities {{{

-- Given a MonadWriter action, returns a modified version of the action
-- whose output is removed and added to the return value
extract :: (MonadWriter w m) => m a -> m (a, w)
extract = censor (const mempty) . listen

-- when' is like when, but it takes the condition wrapped in the monad.
when' :: (Monad m) => m Bool -> m () -> m ()
when' c x = c >>= \c' -> when c' x
-- }}}

-- Data types {{{

data StackVar = StackVar
  { varName     :: Text
  , varType     :: RefType
  , varFrameOff :: Integer -- The offset of the variable within the stack frame
  } deriving (Show)

data CompileEnv = CompileEnv
  { envFnReturn :: ValType
  , envNewtypeDict :: Map Text ValType
  , envScope    :: [StackVar]
  } deriving (Show)

-- }}}

-- Compile monad {{{

newtype Compile a = Compile { unCompile :: ReaderT CompileEnv (WriterT IRBlock (Except CompileErr)) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader CompileEnv, MonadWriter IRBlock, MonadError CompileErr)

instance MonadNewtypeMap Compile where
  newtypeMap = asks envNewtypeDict

runCompile :: Compile a -> CompileEnv -> Either CompileErr (a, IRBlock)
runCompile x e = runExcept $ runWriterT $ runReaderT (unCompile x) e

-- }}}

-- evalValue {{{

evalValue :: Expr -> Compile ValType
evalValue e = case e of
  ELiteral l -> case l of
    LBool x -> tell [IRPushLit $ IRIntLit Unsigned I8 $ if x then 1 else 0] $> VTBool
    LInt s sz x -> tell [IRPushLit $ IRIntLit s sz x] $> VTInt s sz
    LFloat sz x -> tell [IRPushLit $ IRFloatLit sz x] $> VTFloat sz
    LArray xs -> undefined
    LStruct xs -> undefined
    LFunc args ret body -> undefined

  EIdent n -> undefined

  ECall f args -> do
    tf <- evalValue f
    case tf of
      VTFunc fargs ret -> do
        when (length args /= length fargs) $ throwError TypeError
        targs <- evalValue `mapM` args
        when (not $ and $ zipWith (==) targs fargs) $ throwError TypeError
        argsIR <- valTypeToIR `mapM` reverse targs
        tell [IRCall argsIR]
        pure ret
      _ -> throwError TypeError


  EIf c t f -> do
    (tc, cIR) <- extract $ evalValue c
    when (tc /= VTBool) $ throwError TypeError

    (tt, tIR) <- extract $ evalValue t
    (tf, fIR) <- extract $ maybe
      (pure VTVoid)
      evalValue
      f
    when (tt /= tf) $ throwError TypeError

    tell [IRIf cIR tIR fIR]

    pure tt

  EWhile lbl c b -> do
    (tc, cIR) <- extract $ evalValue c
    when (tc /= VTBool) $ throwError TypeError

    (tb, bIR) <- extract $ evalValue b
    when (tb /= VTVoid) $ throwError TypeError

    tell [IRWhile cIR bIR]
    pure VTVoid

  EDefer d x -> undefined
  EBreak l -> undefined
  EContinue l -> undefined
  EReturn x -> undefined
  EFieldAccess a f -> undefined
  ELet t n v b -> undefined
  ECast t x -> evalCast t x
  EUnOp o x -> evalUnOp o x
  EBinOp o x y -> evalBinOp o x y

-- evalUnOp {{{

evalUnOp :: UnOp -> Expr -> Compile ValType
evalUnOp o x = case o of
  UORef -> VTPtr <$> evalLValue x

  UODeref ->
    evalValue x >>= resolveNewtypes >>= \case
      VTPtr rt -> do
        let vt = refToVal rt
        irType <- valTypeToIR vt
        tell [IRDeref irType]
        pure vt
      _ -> throwError TypeError

-- Pre/post increment/decrement {{{

  UOPreInc -> do
    tx <- evalLValue x
    t <- valTypeToIR $ refToVal tx

    when (not $ refMut tx) $ throwError TypeError
    when' (not <$> vtIsNumeric (refToVal tx)) $ throwError TypeError

    tell [IRDup irPtrType, IRDeref t, IRUnOp t IROpInc, IRDup t, IRRot3 t t irPtrType, IRSetAddr t]
    pure $ refToVal tx

  UOPostInc -> do
    tx <- evalLValue x
    t <- valTypeToIR $ refToVal tx

    when (not $ refMut tx) $ throwError TypeError
    when' (not <$> vtIsNumeric (refToVal tx)) $ throwError TypeError

    tell [IRDup irPtrType, IRDeref t, IRDup t, IRRot3 t t irPtrType, IRUnOp t IROpInc, IRSetAddr t]
    pure $ refToVal tx

  UOPreDec -> do
    tx <- evalLValue x
    t <- valTypeToIR $ refToVal tx

    when (not $ refMut tx) $ throwError TypeError
    when' (not <$> vtIsNumeric (refToVal tx)) $ throwError TypeError

    tell [IRDup irPtrType, IRDeref t, IRUnOp t IROpDec, IRDup t, IRRot3 t t irPtrType, IRSetAddr t]
    pure $ refToVal tx

  UOPostDec -> do
    tx <- evalLValue x
    t <- valTypeToIR $ refToVal tx

    when (not $ refMut tx) $ throwError TypeError
    when' (not <$> vtIsNumeric (refToVal tx)) $ throwError TypeError

    tell [IRDup irPtrType, IRDeref t, IRDup t, IRRot3 t t irPtrType, IRUnOp t IROpDec, IRSetAddr t]
    pure $ refToVal tx

-- }}}

  UOBoolNot -> do
    tx <- evalValue x
    tx' <- resolveNewtypes tx

    when (tx' /= VTBool) $ throwError TypeError
    tell [IRNot]
    pure VTBool

  UOBinNot -> do
    tx <- evalValue x
    tx' <- resolveNewtypes tx

    t <- valTypeToIR tx

    case tx' of
      VTInt _ _ -> do
        tell [IRUnOp t IROpBinNot]
        pure tx
      _ -> throwError TypeError

  UOSizeof -> do
    (tx, _) <- extract $ evalValue x -- We don't actually want to generate the code for this, we just need the type
    s <- vtSizeof tx
    tell [IRPushLit $ IRIntLit Unsigned I64 s]
    pure $ VTInt Unsigned I64

  UOPlus -> do -- Mostly a nop, but checks the nested expr is numeric
    tx <- evalValue x
    when' (not <$> vtIsNumeric tx) $ throwError TypeError
    pure tx

  UOMinus -> do
    tx <- evalValue x
    t <- valTypeToIR tx

    when' (not <$> vtIsNumeric tx) $ throwError TypeError

    tell [IRUnOp t IROpNegate]
    pure tx

-- }}}

-- evalBinOp {{{

evalBinOp :: BinOp -> Expr -> Expr -> Compile ValType

evalBinOp BOAssign x y = do
  tx <- evalLValue x
  ty <- evalValue y
  when (refToVal tx /= ty) $ throwError TypeError
  t <- valTypeToIR ty
  tell [IRSetAddr t]
  pure VTVoid

evalBinOp o x y = do
  tx <- evalValue x
  ty <- evalValue y
  case o of
    BOAdd -> undefined
    BOSub -> undefined
    BOMul -> undefined
    BODiv -> undefined
    BOMod -> undefined

    BOBoolAnd -> do
      when (tx /= VTBool) $ throwError TypeError
      when (ty /= VTBool) $ throwError TypeError
      tell [IRBinOp (IRInt Unsigned I8) IROpAnd]
      pure VTBool

    BOBoolOr -> do
      when (tx /= VTBool) $ throwError TypeError
      when (ty /= VTBool) $ throwError TypeError
      tell [IRBinOp (IRInt Unsigned I8) IROpOr]
      pure VTBool

    BOBinAnd -> do
      when' (not <$> vtIsNumeric tx) $ throwError TypeError
      when' (not <$> vtIsNumeric ty) $ throwError TypeError
      tell [IRBinOp (IRInt Unsigned I8) IROpOr]
      pure VTBool

    BOBinOr -> do
      when' (not <$> vtIsNumeric tx) $ throwError TypeError
      when' (not <$> vtIsNumeric ty) $ throwError TypeError
      tell [IRBinOp (IRInt Unsigned I8) IROpOr]
      pure VTBool

    BOBinXor -> do
      when' (not <$> vtIsNumeric tx) $ throwError TypeError
      when' (not <$> vtIsNumeric ty) $ throwError TypeError
      tell [IRBinOp (IRInt Unsigned I8) IROpOr]
      pure VTBool

    BOEqual -> undefined
    BONEqual -> undefined
    BOLShift -> undefined
    BORShift -> undefined
    BOGT -> undefined
    BOGTE -> undefined
    BOLT -> undefined
    BOLTE -> undefined

-- }}}

-- evalCast {{{

evalCast :: ValType -> Expr -> Compile ValType
evalCast t x = do
  tx <- evalValue x
  undefined

{-

TODO
this should:
- check if the types are equal - if so, probably error
- resolve newtypes on both t and tx - you can cast in any way across
  equivalent newtype boundaries
- return if types are equal
- check for trivially compatible types, e.g. numeric types
- check for pointer types
- check for int<->pointer cast (but NOT float<->pointer)
- return after doing appropriate conversions

-}

-- }}}

-- }}}

-- evalLValue {{{

evalLValue :: Expr -> Compile RefType
evalLValue e = case e of
  EFieldAccess a f -> do
    ta <- evalLValue a >>= resolveNewtypes'
    (off, t) <- aggrGetField f (refToVal ta)
    tell
      [ IRPushLit (IRIntLit Unsigned I64 off)
      , IRBinOp irPtrType IROpAdd ]
    pure ta{refToVal = t}

  EIdent n -> do
    vars <- asks envScope

    case filter (\v -> varName v == n) vars of
      [] -> throwError NameError -- TODO: check globals!
      (var:_) -> do
        tell [IRGetOffsetAddr $ varFrameOff var]
        pure $ varType var

  EUnOp UODeref x -> do
    tx <- evalValue x
    case tx of
      VTPtr tx' -> pure tx'
      _ -> throwError TypeError

  _ -> throwError LValueError

-- }}}
