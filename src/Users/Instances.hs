module Users.Instances (UsersService) where

import Users.Types
import Control.Concurrent.MVar
import qualified Data.IntMap.Strict as IntMap
import System.IO.Unsafe
import Servant
import Control.Monad.IO.Class
import Data.Validated
import Control.Monad

{-# NOINLINE userStorage #-}
userStorage :: MVar (IntMap.IntMap User)
userStorage = unsafePerformIO (newMVar IntMap.empty)

instance UserRepo IO where
  accessData f = (fmap.fmap) f (tryReadMVar userStorage)
  withTransaction = modifyMVar userStorage

instance UsersService IO where
  users = readUsers
  userById = readUsersById
  addUser = addNewUser
  deleteUser = deleteUserById

instance UsersService Handler where
  users = liftIO users
  userById id = liftIO $ userById id
  addUser user = liftIO $ addUser user
  deleteUser id = liftIO $ deleteUser id

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