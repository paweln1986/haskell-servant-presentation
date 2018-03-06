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



