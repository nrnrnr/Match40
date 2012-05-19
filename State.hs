{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
module State ( Database(..), User(..)
             , emptyDatabase
             ) 
where

import Data.Acid
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
