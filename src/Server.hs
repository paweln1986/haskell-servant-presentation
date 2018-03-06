{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Server ( application, generateJsClients) where

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
import Servant.JS
import System.FilePath

instance ToJSON User
instance FromJSON User
instance ToSchema User

type Api = SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> ApplicationAPI :<|> Raw

www :: FilePath
www = "examples/www"

apiImplementation :: Server Api
apiImplementation = swaggerSchemaUIServer swaggerDoc :<|> userApi :<|> serveDirectoryFileServer www

userApiProxy :: Proxy ApplicationAPI
userApiProxy = Proxy

applicationAPI :: Proxy Api
applicationAPI = Proxy

application :: Application
application = serve applicationAPI apiImplementation

swaggerDoc :: Swagger
swaggerDoc = toSwagger (Proxy :: Proxy ApplicationAPI)

generateJsClients = do
  writeJSForAPI userApiProxy jquery (www </> "api.js")
  writeJSForAPI userApiProxy vanillaJS (www </> "vanillajs-api.js")
  writeJSForAPI userApiProxy (angular defAngularOptions) (www </> "angular-api.js")
  writeJSForAPI userApiProxy (axios defAxiosOptions) (www </> "axios-api.js")