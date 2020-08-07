module Common where

import Data.Text(Text)
import Data.Map(Map)

import AST

class (Monad m) => MonadNewtypeMap m where
  newtypeMap :: m (Map Text ValType)
