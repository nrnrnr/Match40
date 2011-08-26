module InvitationActions
  ( offer, accept, withdraw, decline
  )
where

import Invitation
import Types

----------------------------------------------------------------

-- | What happens after an action

data IAresult = Blocked IState Message
              | Acted IState (IO ())
             
type Message = String



offer :: (Student -> Student -> Bool) -> Student -> Student -> IState -> IAresult
offer eligible by to invs
    | paired by = fail "You already have a partner."
    | paired to = fail (toname ++ " already has a partner.")
    | not (eligible by to) = fail ("You are not eligible to work with " ++ toname)
    | Just i <- offered invs to by = accept i invs
    | otherwise = error "this code is not yet implemented"
  where toname = readableName to
        paired = isPaired invs
        fail msg = Blocked invs msg
                 
accept = accept
withdraw = withdraw
decline = decline


-- at most one A -> B
-- not both A -> B and B -> A unless at least one is declined
