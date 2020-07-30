{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}

module Compile where

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Data.Text(Text)

import AST
import IR

-- Utilities {{{

-- Given a MonadWriter action, returns a modified version of the action
-- whose output is removed and added to the return value
extract :: (MonadWriter w m) => m a -> m (a, w)
extract = censor (const mempty) . listen

-- }}}

-- Data types {{{

data CompileErr
  = TypeError
  | LValueError
  deriving (Show)

data StackVar = StackVar
  { varName     :: Text
  , varType     :: RefType
  , varFrameOff :: Integer -- The offset of the variable within the stack frame
  } deriving (Show)

data CompileEnv = CompileEnv
  { envFnReturn :: ValType
  , envScope    :: [StackVar]
  } deriving (Show)

-- }}}

-- Compile monad {{{

newtype Compile a = Compile { unCompile :: ReaderT CompileEnv (WriterT IRBlock (Except CompileErr)) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader CompileEnv, MonadWriter IRBlock, MonadError CompileErr)

runCompile :: Compile a -> CompileEnv -> Either CompileErr (a, IRBlock)
runCompile x e = runExcept $ runWriterT $ runReaderT (unCompile x) e

-- }}}
