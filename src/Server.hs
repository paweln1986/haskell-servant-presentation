{-# LANGUAGE TypeOperators #-}
module Server ( application ) where

import Servant
import Servant.Server
import Users.UserApi
import Users.Instances
import Errors.Instances
import Users.Types (User)
import Data.Aeson

instance ToJSON User
instance FromJSON User

type Api = ApplicationAPI

apiImplementation :: Server Api
apiImplementation = userApi

applicationAPI :: Proxy Api
applicationAPI = Proxy

application :: Application
application = serve applicationAPI apiImplementation