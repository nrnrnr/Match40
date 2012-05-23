{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
module State ( Database(..), User(..)
             , emptyDatabase
             , findUser, UserFound(..)
             ) 
where

import Data.Acid
import Data.Monoid
import Data.SafeCopy
import Data.Typeable

import Course
import User

data Database = Database { users :: [User]
                         , courses :: [Course]
                         , courseData :: [CourseData]
                         }
  deriving (Typeable)

$(deriveSafeCopy 0 'base ''Database)
  
emptyDatabase :: Database
emptyDatabase = Database [] [] []

data UserFound = UserFound User
               | UserNotFound
               | Ambiguous String
                   
instance Monoid UserFound where
  mempty = UserNotFound
  UserFound a `mappend` _ = UserFound a
  UserNotFound `mappend` m = m
  Ambiguous msg `mappend` m = Ambiguous msg
  
findUser s users = by "unique ID" (show . uid) `mappend`
                   select "other ID" (any (==s) . map show . otherIds) `mappend`
                   by "full name" (show . name . profile) `mappend`
                   by "last name" (lastName . name . profile)
  where by what f = select what ((==s) . f)
        select what p = case filter p users of
                          [] -> UserNotFound
                          [u] -> UserFound u
                          _ -> Ambiguous what
