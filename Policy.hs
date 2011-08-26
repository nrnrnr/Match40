-- | Course policies are implemented here,
-- like "at most three assignments with one partner".
module Policy
  ( coursename, staffmail, signature
  , loginURL
  , eligible
  )
where

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

loginURL student = "(URL cloudy; try again later)"
