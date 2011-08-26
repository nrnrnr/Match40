-- | Course policies are implemented here,
-- like "at most three assignments with one partner".
module Policy
where

import Invitation
import qualified DB
import Student

-- | Are two students eligible to work together?
eligible :: DB.History -> Student -> Student -> Bool
eligible _ _ _ = True -- bogus
