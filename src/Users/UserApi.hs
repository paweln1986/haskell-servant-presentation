{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Users.UserApi (userApi, ApplicationAPI) where

import Servant
import Errors.Types
import Data.Validated
import Users.Types (User, UsersService)
import qualified Users.Types as UserRepo


type UserAPI = Capture "id" Int :> Get '[JSON] User
               :<|> ReqBody '[JSON] User :> Post '[JSON] Int
               :<|> Capture "id" Int :> DeleteNoContent '[JSON] NoContent
               :<|> Get '[JSON] [User]

type ApplicationAPI = "users" :> UserAPI


userApi :: (UsersService m, Errors m e) => ServerT UserAPI m
userApi = findUserById :<|> addNewUser :<|> deleteUser :<|> listAllUsers

findUserById :: (Monad m, UsersService m, Errors m e) => Int -> m User
findUserById id = do
    user <- UserRepo.userById id
    case user of
      Nothing -> notFound $ "Can not find user with id: "++ show id
      Just us -> return us

addNewUser :: (Monad m, Errors m a, UsersService m) => User -> m Int
addNewUser user = do
  result <- UserRepo.addUser user
  case result of
    Valid newId -> return newId
    Invalid errors -> badRequest $ show errors

deleteUser :: (Functor m, UsersService m) => Int -> m NoContent
deleteUser id = NoContent <$ UserRepo.deleteUser id

listAllUsers ::(UsersService m) => m [User]
listAllUsers = UserRepo.users