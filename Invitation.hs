{-# LANGUAGE DeriveDataTypeable #-}
module Invitation 
  ( Status(..), Invitation(..), IState
  , timestamp
  , isPaired
  , isByTo, isBy, isTo
  , splitByTo
  , offered
  )
where

import Data.List
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

timestamp :: Invitation -> IO Invitation
timestamp i = do now <- getCurrentTime
                 return $ i { history = (status i, now) : history i }


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
  where wanted (i @ I { status = Offered }) = isByTo by to i
        wanted _ = False

isByTo s1 s2 i = offeredBy i == s1 && offeredTo i == s2
isBy s i = offeredBy i == s
isTo s i = offeredTo i == s

-- | Pull out the existing invitation from A to B, if any
splitByTo :: Student -> Student -> [Invitation] -> (Maybe Invitation, [Invitation])
splitByTo by to invs =
    case partition (isByTo by to) invs of
      ([i], is) -> (Just i,  is)
      ([],  is) -> (Nothing, is)
      _ -> error "duplicate invitations in set"
