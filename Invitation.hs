{-# LANGUAGE DeriveDataTypeable #-}
module Invitation 
  ( Status(..), Invitation(..), IState
  , timestamp
  , isPaired
  , isByTo, isBy, isTo
  , hasStatus
  , splitByTo
  , offered
  , differentPairs
  , invariant1, invariant2, invariant3, invariant4, invariant5
  , allInvariants
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
  deriving (Typeable, Show, Eq)

data Invitation = I { offeredBy :: Student
                    , offeredTo :: Student
                    , status    :: Status
                    , history   :: [(Status, UTCTime)]
                    }
  deriving (Typeable)

instance Show Invitation where
    show i =
        show (offeredBy i) ++ "--(" ++ show (status i) ++ ")->" ++ show (offeredTo i)


timestamp :: Invitation -> IO Invitation
timestamp i = do now <- getCurrentTime
                 return $ i { history = (status i, now) : history i }


type IState = [Invitation]
-- ^ All invitations for a project, satisfying these invariants:
--     1. at most one A -> B
--     2. if A -> B and B -> A, then one must be declined or withdrawn
--     3. if A -> A, then status must be Accepted (flying solo)
--
--     4. for all i, status i == fst (head (history i))
-- 
--     5. if A is part of an Accepted invitation, there are no outstanding
--        offers to A or by A

invariant1, invariant2, invariant3, invariant4, invariant5 :: IState -> Bool

invariant1 [] = True
invariant1 (i:is) = null (filter (isByTo (offeredBy i) (offeredTo i)) is) &&
                    invariant1 is

invariant2 [] = True
invariant2 (i:is) = ok i && invariant2 is
    where okstatus (I { status = Declined  }) = True
          okstatus (I { status = Withdrawn }) = True
          okstatus _ = False
          ok i = okstatus i ||
                 all okstatus (filter (isByTo (offeredTo i) (offeredBy i)) is)

invariant3 [] = True
invariant3 (i:is) = invariant3 is &&
                    if offeredBy i == offeredTo i then
                        status i == Accepted
                    else
                        True

invariant4 = all (\i -> not (null (history i)) && status i == fst (head (history i)))

invariant5 is = all ok is
    where ok i = if status i == Accepted then
                     no_offers_to_or_by (offeredBy i) &&
                     no_offers_to_or_by (offeredTo i)
                 else
                     True
          no_offers_to_or_by student = not (any (offer_to_or_by student) is)
          offer_to_or_by s i =
              status i == Offered && (s == offeredBy i || s == offeredTo i)


allInvariants is =
    all (\p -> p is) [invariant1, invariant2, invariant3, invariant4, invariant5]

----------------------------------------------------------------

-- Useful predicates and things

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

-- | Do two invitations involve different pairs of students?
differentPairs :: Invitation -> Invitation -> Bool
differentPairs i1 i2 = (toBy i1 /= toBy i2 && toBy i1 /= swap (toBy i2))
    where toBy i = (offeredTo i, offeredBy i)
          swap (x, y) = (y, x)

hasStatus :: Invitation -> Status -> Bool
hasStatus i s = status i == s
