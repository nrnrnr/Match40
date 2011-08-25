module Invitation 
where

import Data.Time.Clock

import Types

-- | |Declined| is declined by recipient
-- | |Witdrawn| is declined by original offerer

data Status = Offered | Accepted | Declined | Withdrawn

data Invitation = I { offeredBy :: Student
                    , offeredTo :: Student
                    , status    :: Status
                    , history   :: [(Status, UTCTime)]
                    }

