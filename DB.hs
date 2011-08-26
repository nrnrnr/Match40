{-# LANGUAGE DeriveDataTypeable #-}

module DB 
where
import Data.Typeable

import Types
import Invitation

-- | Project

data Project = Project { projectName :: String
                       , activeInvitations :: [Invitation]
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

-- | Produce a list of all invitations for the current project,
-- even those that have been declined.
invitations :: Database -> [Invitation]
invitations db = case project db of
                   Nothing -> []
                   Just p  -> activeInvitations p
