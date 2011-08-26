{-# LANGUAGE DeriveDataTypeable #-}
module Invitation 
  ( Status(..), Invitation(..), IState
  , isPaired
  , offered
  )
where

import Data.Time.Clock
import Data.Typeable

import Email
import Student

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



isPair :: Invitation -> Bool
isPair (I { status = Accepted }) = True
isPair _ = False

isPaired :: IState -> Student -> Bool
isPaired invs student = any pairs invs
    where pairs (I { status = Accepted, offeredBy = s1, offeredTo = s2}) =
              s1 == student || s2 == student
          pairs _ = False

offered :: IState -> Student -> Student -> Maybe Invitation
offered invs by to =
    case filter wanted invs of
      [i] -> Just i
      [] -> Nothing
      i:is -> error "this can't happen; multiple offers"
  where wanted (I { status = Offered, offeredBy = s1, offeredTo = s2 }) =
            s1 == by && s2 == to
        wanted _ = False

