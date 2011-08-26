-- | Course policies are implemented here,
-- like "at most three assignments with one partner".
module Policy
  ( coursename, staffmail, signature
  , loginURL
  , eligible
  )
where

import Data.HashTable (hashString)

import Codec.Binary.Base64.String (encode)

import Invitation
import qualified DB
import Student

-- | Are two students eligible to work together?
eligible :: DB.History -> Student -> Student -> Bool
eligible _ _ _ = True -- bogus


coursename = "COMP 40"
staffmail = "comp40-staff@cs.tufts.edu"
signature = "The COMP 40 matchmaker"


baseURL = "http://match40.cs.tufts.edu"

loginURL student = "private/" ++ (readable $ encode $ show $ hashString tohash)
  where
    readable = filter (\c -> c `elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
    tohash = readableName student ++ email student 
