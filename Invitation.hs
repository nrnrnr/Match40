{-# LANGUAGE DeriveDataTypeable #-}
module Invitation 
where

import Data.Time.Clock
import Data.Typeable

import Types

-- | |Declined| is declined by recipient
-- | |Witdrawn| is declined by original offerer

data Status = Offered | Accepted | Declined | Withdrawn
  deriving (Typeable)

data Invitation = I { offeredBy :: Student
                    , offeredTo :: Student
                    , status    :: Status
                    , history   :: [(Status, UTCTime)]
                    }
  deriving (Typeable)

