module DB 
where
import Types
import Invitation

-- | Project

data Project = Project { projectName :: String
                       , invitations :: [Invitation]
                       }

data History = History [Project]

data Database = Database [Student] History (Maybe Project)


