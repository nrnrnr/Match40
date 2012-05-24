{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
module Course
       ( Course(..), CourseData(..)
       , Dept(..), Term(..), CourseSection(..), Year(..)
       , comp, en
       , emptyCourseData
       , courseNamed
       )
where
  
import Data.SafeCopy
import Data.Time.Clock
import System.IO

import Identity




newtype Year = Year { unYear :: Int }
  deriving (Eq, Ord)

instance Read Year where
  readsPrec n = map (\(a, s) -> (Year a, s)) . readsPrec n

instance Show Year where 
  show = show . unYear

$(deriveSafeCopy 0 'base ''Year)

data Dept = COMP | EN -- more can come
  deriving (Show, Eq, Ord, Read)
$(deriveSafeCopy 0 'base ''Dept)

data Term = Spring | Summer | Fall  -- might evolve to cover 2 summer terms
  deriving (Show, Eq, Ord)
$(deriveSafeCopy 0 'base ''Term)

instance Read Term where
  readsPrec _ ('u':s) = [(Summer, s)]
  readsPrec _ ('f':s) = [(Fall, s)]
  readsPrec _ ('s':s) = [(Spring, s)]
  readsPrec _ _ = []
  
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

courseNamed :: [String] -> IO (Maybe Course)
courseNamed = needsArg tryDept 
  where needsArg f [] = return Nothing
        needsArg f (s:ss) = f s ss

        tryDept dept ss = case readMaybe dept of
          Just dept -> withDept dept ss
          Nothing -> withDept COMP (dept:ss)

        withDept d = needsArg (withDept' d)
        withDept' dept num ss = case readMaybe num of
            Just num -> takeSem num ss
            Nothing -> return Nothing
          where takeSem num [] = do (year, term) <- nextSemester
                                    return $ Just $ Course dept num year term Nothing
                takeSem num (sem:ss) = error "unimp"

nextSemester :: IO (Year, Term)
nextSemester = do hPutStrLn stderr "this is appalling: it's always Fall 2012?!"
                  return (Year 2012, Fall)

readMaybe :: Read a => String -> Maybe a
readMaybe s = case [ x | (x, t) <- reads s, ("", "") <- lex t ] of
  [a] -> Just a
  _ -> Nothing




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

