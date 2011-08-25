{-# LANGUAGE DeriveDataTypeable #-}
module Invitation 
where

import Data.Time.Clock
import Data.Typeable

import Email
import Types

-- | |Declined| is declined by recipient
-- | |Withdrawn| is declined by original offerer

data Status = Offered | Accepted | Declined | Withdrawn
  deriving (Typeable)

data Invitation = I { offeredBy :: Student
                    , offeredTo :: Student
                    , status    :: Status
                    , history   :: [(Status, UTCTime)]
                    }
  deriving (Typeable)


type IState = [Invitation]
-- ^ Status of all invitations for a project

----------------------------------------------------------------

-- | What happens after an action

data Iresult = Blocked IState Message
             | Acted IState (IO ())
             


offer :: Student -> Student -> IState -> Iresult
offer = undefined
