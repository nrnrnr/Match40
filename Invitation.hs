module Invitation 
where

import DB
import Data.Time.Clock

-- | |Declined| is declined by recipient
-- | |Witdrawn| is declined by original offerer

data Status = Offered | Accepted | Declined | Withdrawn

data Invitation = I { offeredBy :: Student
                    , offeredTo :: Student
                    , status    :: Status
                    , history   :: [(Status, UTCTime)]
                    }

