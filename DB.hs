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

curProject :: Database -> Maybe Project
curProject (Database _ _ p) = p

