module Types
  ( Student(..)
  , Enrollment(..)
  , FullName(..)
  , fullName
  )
where
import Prelude hiding (last)
import qualified Data.Char



type URL = String

-- | What we know about a student
data Student = Student { name  :: FullName
                       , email :: String
                       , aboutMe :: String
                       , photo :: Maybe URL
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

fullName :: String -> FullName
fullName s = FullName { last = last, first = first, synonyms = [] }
    where last = reverse $ takeWhile nonBlank $ reverse s
          first = takeWhile nonBlank s
          nonBlank = not . Data.Char.isSpace