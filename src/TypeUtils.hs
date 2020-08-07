{-# LANGUAGE FlexibleContexts #-}

module TypeUtils where

import Data.Text(Text)
import Data.Map(Map)
import qualified Data.Map as M
import Control.Monad.Except

import Common
import AST
import IR

data CompileErr
  = TypeError
  | LValueError
  | NameError
  deriving (Show)

resolveNewtypes :: (MonadError CompileErr m, MonadNewtypeMap m) => ValType -> m ValType
resolveNewtypes t = case t of
  VTNewtype name ->
    newtypeMap >>= \ntm ->
    maybe
      (throwError NameError)
      resolveNewtypes
      (M.lookup name ntm)
  _ -> pure t

resolveNewtypes' :: (MonadError CompileErr m, MonadNewtypeMap m) => RefType -> m RefType
resolveNewtypes' t = (\x -> t{refToVal = x}) <$> resolveNewtypes (refToVal t)


valTypeToIR :: (MonadError CompileErr m, MonadNewtypeMap m) => ValType -> m IRType
valTypeToIR t = case t of
  VTVoid -> throwError TypeError
  VTBool -> pure $ IRInt Unsigned I8
  VTPtr _ -> pure irPtrType
  VTFunc _ _ -> pure irPtrType -- Functions are actually function pointers
  VTInt s sz -> pure $ IRInt s sz
  VTFloat sz -> pure $ IRFloat sz
  VTStruct _ -> error "TODO"
  VTUnion _ -> error "TODO"
  VTNewtype n -> newtypeMap >>= \ntm ->
    maybe
      (throwError NameError)
      valTypeToIR
      (M.lookup n ntm)

-- Returns (offset, type)
aggrGetField :: (MonadError CompileErr m, MonadNewtypeMap m) => Text -> ValType -> m (Integer, ValType)
aggrGetField name t = case t of
  VTStruct fs -> do
    fs' <- filter (\(n,_,_) -> n == name) <$> getStructOffsets fs
    when (null fs') $ throwError NameError
    let (_, t, o) = head fs'
    pure (o, t)

  VTUnion fs -> do
    let fs' = filter (\f -> fst f == name) fs
    when (null fs') $ throwError NameError
    let t = snd $ head fs'
    pure (0, t)

  _ -> throwError TypeError

intSizeBytes :: IntSize -> Integer
intSizeBytes s = case s of
  I8 -> 1
  I16 -> 2
  I32 -> 4
  I64 -> 8

floatSizeBytes :: FloatSize -> Integer
floatSizeBytes s = case s of
  F32 -> 4
  F64 -> 8
  F80 -> 16

vtSizeof :: (MonadError CompileErr m, MonadNewtypeMap m) => ValType -> m Integer
vtSizeof t = case t of
  VTVoid -> pure 0
  VTBool -> pure 1
  VTPtr _ -> pure 8
  VTInt _ s -> pure $ intSizeBytes s
  VTFloat s -> pure $ floatSizeBytes s
  VTNewtype n -> newtypeMap >>= \ntm ->
    maybe
      (throwError NameError)
      vtSizeof
      (M.lookup n ntm)

vtIsNumeric :: (MonadError CompileErr m, MonadNewtypeMap m) => ValType -> m Bool
vtIsNumeric t = case t of
  VTVoid -> pure False
  VTBool -> pure False
  VTPtr _ -> pure False
  VTInt _ s -> pure True
  VTFloat s -> pure True
  VTNewtype n -> newtypeMap >>= \ntm ->
    maybe
      (throwError NameError)
      vtIsNumeric
      (M.lookup n ntm)

getStructOffsets :: (MonadError CompileErr m, MonadNewtypeMap m) => [(Text, ValType)] -> m [(Text, ValType, Integer)]
getStructOffsets = fmap snd . foldM f (0, [])
  where
    f (curOff, xs) (n, t) = do
      s <- vtSizeof t
      let off = if curOff `mod` s == 0
                then curOff
                else curOff + s - curOff `mod` s
          x = (n, t, off)
      pure (off + s, xs ++ [x])
