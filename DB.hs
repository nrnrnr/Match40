{-# LANGUAGE DeriveDataTypeable #-}

module DB 
where
import Data.Typeable

import Types
import Invitation

-- | Project

data Project = Project { projectName :: String
                       , invitations :: [Invitation]
                       }
  deriving (Typeable)

data History = History [Project]
  deriving (Typeable)

data Database = Database [Student] History (Maybe Project)
  deriving (Typeable)

students :: Database -> [Student]
students (Database ss _ _) = ss

projHistory :: Database -> History
projHistory (Database _ h _) = h

project :: Database -> Maybe Project
project (Database _ _ p) = p


