{-# LANGUAGE DeriveDataTypeable #-}

module Student
  ( Student(..)
  , Enrollment(..)
  , FullName(..)
  , fullName
  , firstname
  , readableName
  , hash
  )
where
import qualified Data.Char
import Data.HashTable (hashString)
import Data.Typeable
import Prelude hiding (last)

import Codec.Binary.Base64.String (encode)

type URL = String

-- | What we know about a student
data Student = Student { name  :: FullName
                       , email :: String
                       , aboutMe :: String
                       , photo :: Maybe URL
                       , urlid :: String
                       , enrollment :: Enrollment
                       }
  deriving (Typeable, Eq, Ord)

instance Show Student where
    show s = "<" ++ readableName s ++ ">"

data Enrollment = Enrolled | Dropped
  deriving (Typeable, Eq, Ord)

type Email = String
type Photo = String
data FullName = FullName { last  :: String
                         , first :: String
                         , synonyms :: [String]
                         }
  deriving (Typeable, Eq, Ord)

fullName :: String -> FullName
fullName s = FullName { last = last, first = first, synonyms = [] }
    where last = reverse $ takeWhile nonBlank $ reverse s
          first = takeWhile nonBlank s
          nonBlank = not . Data.Char.isSpace

readableName :: Student -> String
readableName s = first fn ++ " " ++ last fn
    where fn = name s

firstname s = first (name s)

hash s = readable $ encode $ show $ hashString tohash
  where
    readable = filter (\c -> c `elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
    tohash = readableName s ++ email s

