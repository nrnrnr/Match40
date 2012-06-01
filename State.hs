{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell, RecordWildCards #-}
module State ( Database(..), User(..)
             , emptyDatabase
             , nextUserNumber
             , findUser, UserFound(..)
             ) 
where

import Data.Acid
import Data.Monoid
import Data.SafeCopy
import Data.Typeable

import Course
import User

data Database = Database { firstUserNum :: Int
                         , users :: [User]
                         , courses :: [Course]
                         , courseData :: [CourseData]
                         }
  deriving (Typeable)

$(deriveSafeCopy 1 'extension ''Database)
  
emptyDatabase :: Database
emptyDatabase = Database 1000 [] [] []

nextUserNumber :: Database -> UserNumber
nextUserNumber (Database { firstUserNum = n, users = us }) = UserNumber (n + length us)

data UserFound = UserFound User
               | UserNotFound
               | Ambiguous String
instance Show UserFound where
  show (UserFound u) = show u
  show UserNotFound = "no such user"
  show (Ambiguous what) = "ambiguous " ++ what
                   
instance Monoid UserFound where
  mempty = UserNotFound
  UserFound a `mappend` _ = UserFound a
  UserNotFound `mappend` m = m
  Ambiguous msg `mappend` m = Ambiguous msg
  
findUser :: String -> [User] -> UserFound
findUser s users = by "unique ID" (show . uid) `mappend`
                   select "other ID" (any (==s) . map show . otherIds) `mappend`
                   by "full name" (show . name . profile) `mappend`
                   by "last name" (lastName . name . profile)
  where by what f = select what ((==s) . f)
        select what p = case filter p users of
                          [] -> UserNotFound
                          [u] -> UserFound u
                          _ -> Ambiguous what

------ old types
data Database0 = Database0 { users0 :: [User0]
	                   , courses0 :: [Course]
                           , courseData0 :: [CourseData]
                           }
  deriving (Typeable)

$(deriveSafeCopy 0 'base ''Database0)

instance Migrate Database where
  type MigrateFrom Database = Database0
  migrate Database0 {..} = Database { firstUserNum = start
                                    , users = zipWith numberUser nums users0
                                    , courses = courses0
                                    , courseData = courseData0
                                    }
      where start = firstUserNum emptyDatabase
            nums = map UserNumber [start..]
