{-# LANGUAGE MultiParamTypeClasses #-}

module Errors.Types where

import Control.Monad.Error.Class

class (Monad m, MonadError e m) => Errors m e where
  notFound :: String -> m a
  badRequest :: String -> m a
  conflict :: String -> m a

