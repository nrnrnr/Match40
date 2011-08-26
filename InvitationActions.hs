module InvitationActions
  ( offer, accept, withdraw, decline
  )
where

import qualified CourseMail
import Invitation
import Student

----------------------------------------------------------------

-- | What happens after an action
data IAresult
    = Blocked IState Message
    | Acted Invitation IState (IO ())
        -- ^ Result is a new invitations, the rest of the invitation state,
        -- and an IO action to perform if the transaction commits.
        -- The invitation must be timestamped before being added to the state.
             
type Message = String


-- | timestamp any outstanding invitation and return invitation state
finalIState :: IAresult -> IO IState
finalIState (Blocked s _) = return s
finalIState (Acted i is _) = timestamp i >>= return . (:is)


-- | One student proposes partnership to another, offering an invitation
offer :: (Student -> Student -> Bool) -- ^ Policy function says if students are permitted to work together
      -> Student -- ^ Student making the offer
      -> Student -- ^ Student to whom the offer is made
      -> IState  -- ^ Current state of all invitations
      -> IAresult
offer eligible by to invs
    | paired by = fail "You already have a partner."
    | paired to = fail (toname ++ " already has a partner.")
    | not (eligible by to) = fail ("You are not eligible to work with " ++ toname)
    | Just i <- offered invs to by = accept i invs
    | otherwise = Acted this others (CourseMail.offer by to)
  where toname = readableName to
        paired = isPaired invs
        fail msg = Blocked invs msg
        (this, others) = case splitByTo by to invs of
                           (Just i, is) -> (i, is)
                           (Nothing, is) -> (newOffer, is)
        newOffer = I by to Offered [] 
            
                 
-- | Accept an invitation
accept :: Invitation -> IState -> IAresult
accept inv invs = Acted (inv { status = Accepted }) invs
                  (CourseMail.accept (offeredTo inv) (offeredBy inv))

-- | Decline an invitation
decline :: Invitation -> IState -> IAresult
decline inv invs = Acted (inv { status = Declined }) invs
                  (CourseMail.decline (offeredTo inv) (offeredBy inv))
    
-- | Withdraw an invitation
withdraw :: Invitation -> IState -> IAresult
withdraw inv invs = Acted (inv { status = Withdrawn }) invs
                    (CourseMail.withdraw (offeredBy inv) (offeredTo inv))


-- at most one A -> B
-- not both A -> B and B -> A unless at least one is declined
