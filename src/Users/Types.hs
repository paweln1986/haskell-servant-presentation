{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Users.Types where

import GHC.Generics
import Data.Time.Calendar
import qualified Data.IntMap.Strict as IntMap
import Control.Monad
import Data.Validated

data User = User{name :: String, age :: Int, email :: String,
                 registrationDate :: Day}
          deriving (Eq, Show, Generic, Read)

data UserRepoError = UserAlreadyExists String | InvalidEmail String deriving (Show)

class UserRepo m where
  accessData :: (IntMap.IntMap User -> b) -> m (Maybe b)
  withTransaction :: (IntMap.IntMap User -> m (IntMap.IntMap User,b)) -> m b

class UsersService m where
  users :: m [User]
  userById :: Int -> m (Maybe User)
  addUser :: User -> m (Validated [UserRepoError] Int)
  deleteUser :: Int -> m ()

readUsers ::(Functor m, UserRepo m) => m [User]
readUsers = concat <$> accessData IntMap.elems

readUsersById :: (Functor m, UserRepo m) => Int -> m (Maybe User)
readUsersById id = join <$> accessData (IntMap.lookup id)


validateUnique :: [User] -> String -> Validated [UserRepoError] String
validateUnique users newUserName = if any (\x -> name x == newUserName) users
  then Invalid [UserAlreadyExists $ "User with name " ++ newUserName ++ " already exists"]
  else Valid newUserName

validateEmail :: String -> Validated [UserRepoError] String
validateEmail email = if '@' `elem` email
  then Valid email
  else Invalid [InvalidEmail $ "Email address " ++ email ++ " is incorrect"]

addNewUser user = withTransaction (insertUser user) where
  insertUser user im = do
    let users = IntMap.elems im
    let newId = nextFreeId im
    let validation = user <$ validateUnique users (name user) <* validateEmail (email user)
    case validation of
      Invalid errors -> return (im, Invalid errors)
      Valid user -> return (IntMap.insert newId user im, Valid newId)

deleteUserById id = withTransaction $ \x -> do
  let newData = IntMap.delete id x
  return (newData,())

nextFreeId :: IntMap.IntMap a -> Int
nextFreeId im = case IntMap.lookupMax im of
  Nothing -> 1
  Just (x,_) -> x + 1

