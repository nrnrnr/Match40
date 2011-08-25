module DB
where

type URL = String

data Student = Student { name  :: FullName
                       , email :: String
                       , aboutMe :: String
                       , photo :: URL
                       , password :: String
                       , status :: Enrollment
                       }

data Enrollment = Enrolled | Dropped

type Email = String
type Photo = String
data FullName = FullName { last  :: String
                         , first :: String
                         , synonyms :: [String]
                         }
