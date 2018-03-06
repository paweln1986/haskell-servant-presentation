module Users.Instances (UsersService) where

import Users.Types
import Control.Concurrent.MVar
import qualified Data.IntMap.Strict as IntMap
import System.IO.Unsafe
import Servant
import Control.Monad.IO.Class

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
