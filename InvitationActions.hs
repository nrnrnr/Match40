module InvitationActions
  ( offer, accept, withdraw, decline
  )
where

import Invitation
import Student

----------------------------------------------------------------

-- | What happens after an action

data IAresult = Blocked IState Message
              | Acted IState (IO ())
             
type Message = String



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
    | otherwise = create_or_resurrect
  where toname = readableName to
        paired = isPaired invs
        fail msg = Blocked invs msg
        (this, others) = case splitByTo by to invs of
                           (Just i, is) -> (i, is)
                           (Nothing, is) -> (newOffer, is)
        newOffer = I by to Offered [] 

        create_or_resurrect = error "unimp"
            
                 
accept = accept
withdraw = withdraw
decline = decline


-- at most one A -> B
-- not both A -> B and B -> A unless at least one is declined
