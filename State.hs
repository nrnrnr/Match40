{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
module State ( Database(..), User(..)
             , emptyDatabase
             ) 
where

import Data.Acid

import Control.Monad.State                   ( get, put )
import Control.Monad.Reader                  ( ask )
import Control.Applicative                   ( (<$>) )
import Data.SafeCopy

import Course
import User

data Database = Database { users :: [User]
                         , courses :: [Course]
                         , courseData :: [CourseData]
                         }

$(deriveSafeCopy 0 'base ''Database)
  
emptyDatabase :: Database
emptyDatabase = Database [] [] []

