{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Server ( application ) where

import Servant
import Servant.Server
import Users.UserApi
import Users.Instances
import Errors.Instances
import Users.Types (User)
import Data.Aeson
import Servant.Swagger.UI
import Servant.Swagger
import Data.Swagger

instance ToJSON User
instance FromJSON User
instance ToSchema User

type Api = SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> ApplicationAPI

apiImplementation :: Server Api
apiImplementation = swaggerSchemaUIServer swaggerDoc :<|> userApi

applicationAPI :: Proxy Api
applicationAPI = Proxy

application :: Application
application = serve applicationAPI apiImplementation

swaggerDoc :: Swagger
swaggerDoc = toSwagger (Proxy :: Proxy ApplicationAPI)