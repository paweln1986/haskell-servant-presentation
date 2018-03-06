{-# LANGUAGE MultiParamTypeClasses #-}

module Errors.Instances where
import Errors.Types
import Servant
import qualified Data.ByteString.Lazy.Char8 as C

instance Errors Handler ServantErr where
  notFound message = throwError $ err404 { errBody = C.pack message }
  badRequest message = throwError $ err400 { errBody = C.pack message }
  conflict message = throwError $ err409 { errBody = C.pack message }

