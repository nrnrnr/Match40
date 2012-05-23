{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
module Course
       ( Course(..), CourseData(..)
       , Dept(..), Term(..), CourseSection(..), Year(..)
       , comp, en
       , emptyCourseData
       )
where
  
import Data.SafeCopy
import Data.Time.Clock

import Identity




newtype Year = Year { unYear :: Int }
  deriving (Eq, Ord)

$(deriveSafeCopy 0 'base ''Year)

data Dept = COMP | EN -- more can come
  deriving (Show, Eq, Ord)
$(deriveSafeCopy 0 'base ''Dept)

data Term = Spring | Summer | Fall  -- might evolve to cover 2 summer terms
  deriving (Show, Eq, Ord)
$(deriveSafeCopy 0 'base ''Term)

data CourseSection = NamedSection String
                   | NumberedSection Int
  deriving (Eq, Ord)
$(deriveSafeCopy 0 'base ''CourseSection)
instance Show CourseSection where
  show (NamedSection s) = s
  show (NumberedSection n) = show n
                     
data Course = Course { dept :: Dept
                     , number :: Int
                     , year :: Year
                     , term :: Term
                     , section :: Maybe CourseSection
                     }
  deriving (Eq, Ord)
$(deriveSafeCopy 0 'base ''Course)
  
instance Show Course where
  show c = show (dept c) ++ " " ++ show (number c)
              
-- | shortcuts for standard COMP and EN courses
comp, en :: Int -> Year -> Term -> Course

comp n y t = Course COMP n y t Nothing
en   n y t = Course EN   n y t Nothing




--------------------------------------

data Commitment = Solo UserIdent -- ^ User working alone
                | Paired UserIdent UserIdent -- ^ Users working together
$(deriveSafeCopy 0 'base ''Commitment)
                  
data Offer
  = Invitation { fromUser :: UserIdent, toUser :: UserIdent } -- ^ offer to pair
  | ClaimPair  { byUser :: UserIdent, withUser :: UserIdent } 
        -- ^ claim of out-of-band agreement to pair
$(deriveSafeCopy 0 'base ''Offer)

-- N.B. An invitation extended does not affect the invitee,
-- but a claim of pairing devalues *both* users from the pool of
-- partners who might be recommended to others


data Event
  = Offered Offer UTCTime
  | Accepted Offer UTCTime
  | Withdrawn Offer UTCTime
  | Soloed UserIdent UTCTime
  | Divorced UserIdent UserIdent UTCTime
$(deriveSafeCopy 0 'base ''Event)

  
data Asst = Asst { asstId :: String -- ^ short identifier, e.g., "iii" or "hofs"
                 , asstLongName :: String -- ^ human-readable name
                 , commitments :: [Commitment]
                 , offers :: [Offer]
                 , history :: [Event]
                 , asstClosed :: Maybe UTCTime
                 , asstOpened :: UTCTime
                 }
$(deriveSafeCopy 0 'base ''Asst)
            
  

data CourseData = CourseData { course :: Course
                             , assignments :: [Asst]
                             , courseClosed :: Maybe UTCTime  -- ^ when course ended
                             }
$(deriveSafeCopy 0 'base ''CourseData)

emptyCourseData :: Course -> CourseData
emptyCourseData course = CourseData course [] Nothing

