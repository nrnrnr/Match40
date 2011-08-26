module InvitationActions
  ( offer, accept, withdraw, decline
  )
where

import Data.List

import qualified CourseMail
import Invitation
import Student

{-

Lots of hair here.  The issue is how to separate pure computation from
monadic computation, in a way that is compositional.

  - The pure part is essentially | [Invitation] -> [Invitation] |.

  - Any changed invitation needs to get timestamped, which is an IO action.

  - Most actions also result in sending emails.

For QuickCheck, we want to avoid the I/O actions, and we want to make
things compositional.

-}

data PendingAction = PA { old_is :: [Invitation] -- ^ untouched
                        , new_is :: [Invitation] -- ^ awaiting timestamps
                        , sends  :: IO ()        -- ^ emails to be sent
                        , errors :: [String]     -- ^ messages on blocked actions
                        }
   -- this is an intermediate state pending some action


type Action = PendingAction -> PendingAction


-- | What happens after an action
data IAResult
    = Blocked IState Message -- ^ The action is disallowed
    | Acted (IO IState)      -- ^ Launch emails and return new state
             
type Message = String


-- Normal actions are converted into a monadic result, but may be pure for testing
wrap_monadic :: Action -> IState -> IAResult
wrap_pure    :: Action -> IState -> IState

wrap_pure action is = map purestamp (new_is final) ++ old_is final
    where final = action (start_action is)

wrap_monadic action is =
    case final of
      PA { errors = es@(_ : _) } -> Blocked is (concat $ intersperse "\n" $ es)
      PA { errors = [] } -> Acted $
          do stamped <- mapM timestamp (new_is final)
             sends final
             return (stamped ++ old_is final)
    where final = action (start_action is)
          final :: PendingAction


start_action :: IState -> PendingAction
start_action is = PA is [] (return ()) []




----------------------------------------------------------------

-- Higher-order functions for creating actions


-- | Change status of an invitation
transition :: Status -> Status -> Invitation -> Maybe Invitation
transition old new i = if hasStatus i old then Just (i { status = new}) else Nothing
    
liftChange :: String -- ^ error message
           -> (Invitation -> Maybe Invitation) -- ^ pure change in invitation
           -> (Student -> Student -> IO a) -- ^ send email
           -> Invitation -> Action       -- ^ impure action function
liftChange msg change send inv state =
    case splitByTo by to (old_is state) of -- find the invitation
      (Just i, is) | Just i' <- change i -> -- change it
            state { old_is = is
                  , new_is = i' : new_is state
                  , sends  = sends state >> send by to >> return ()
                  } -- compute the new state
      _ -> state { errors = msg : errors state } -- or barf
    where to = offeredTo inv
          by = offeredBy inv



-- | One student proposes partnership to another, offering an invitation
offer :: (Student -> Student -> Bool) -- ^ Policy function says if students are permitted to work together
      -> Student -- ^ Student making the offer
      -> Student -- ^ Student to whom the offer is made
      -> IState  -- ^ Current state of all invitations
      -> IAResult
offer e by to = wrap_monadic $ offer' e by to
            
offer' :: (Student -> Student -> Bool) -- ^ Policy function says if students are permitted to work together
      -> Student -- ^ Student making the offer
      -> Student -- ^ Student to whom the offer is made
      -> Action  -- ^ Current state of all invitations
offer' eligible by to state@(PA old new sends errors)
    | paired by = fail "You already have a partner."
    | paired to = fail (toname ++ " already has a partner.")
    | not (eligible by to) = fail ("You are not eligible to work with " ++ toname)
    | Just i <- offered invs to by = accept' i state
    | otherwise = PA others (this:new) (sends >> CourseMail.offer by to) errors
  where toname = readableName to
        paired = isPaired invs
        invs = old ++ new
        fail msg = (PA old new sends (msg:errors))
        (this, others) = case splitByTo by to invs of
                           (Just i, is) -> (i, is)
                           (Nothing, is) -> (newOffer, is)
        newOffer = I by to Offered [] 
            
                 
-- | Accept an invitation
accept :: Invitation -> IState -> IAResult
accept inv = wrap_monadic $ accept' inv

accept' :: Invitation -> Action
accept' inv state@(PA old new sends errors) =
      merge declines $ merge withdraws $
      liftChange "Accepted an invitation, but could not find the offer"
                 (transition Offered Accepted)
                 (flip CourseMail.accept) inv state
    where to = offeredTo inv
          by = offeredBy inv
          declines  = map decline'  (filter (isTo to /\ (not . isBy by)) offers)
          withdraws = map withdraw' (filter (isBy to) offers)
          merge [] = \x -> x
          merge (f:fs) = f . merge fs
          offers = filter (flip hasStatus Offered) old

          p /\ q = \x -> p x && q x


-- | Decline an invitation
decline :: Invitation -> IState -> IAResult
decline inv = wrap_monadic $ decline' inv

decline' :: Invitation -> Action
decline' = liftChange "Declined an invitation, but could not find the offer"
           (transition Offered Declined) (flip CourseMail.decline)

-- | Withdraw an invitation
withdraw :: Invitation -> IState -> IAResult
withdraw inv = wrap_monadic $ withdraw' inv
withdraw' :: Invitation -> Action
withdraw' = liftChange "Withdrew an invitation, but could not find the offer"
            (transition Offered Withdrawn) CourseMail.withdraw

----------------------------------------------------------------


-- at most one A -> B
-- not both A -> B and B -> A unless at least one is declined


arbitraryTime = error "no peeking at timestamps during testing!"

purestamp :: Invitation -> Invitation
purestamp i = i { history = (status i, arbitraryTime) : history i }
