{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell, RecordWildCards #-}
module Course
       ( Course(..), CourseData(..)
       , Dept(..), Term(..), CourseSection(..), Year(..)
       , comp, en
       , emptyCourseData
       , courseNamed
       , parseCourseIn, courseTestsParseOK
       )
where
  
import Debug.Trace

import Control.Applicative
import Data.Char
import Data.List
import Data.Maybe
import Data.SafeCopy
import Data.Time.Calendar
import Data.Time.Clock
import System.IO

import ArgParse
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

showFullCourse :: Course -> String
showFullCourse (Course {..}) =
    intercalate " " [show dept, show number ++ ss, show term, show year]
  where ss = fromMaybe "" (fmap (("-" ++) . show) section)
              
-- | shortcuts for standard COMP and EN courses
comp, en :: Int -> Year -> Term -> Course

comp n y t = Course COMP n y t Nothing
en   n y t = Course EN   n y t Nothing


-- | take default year and term, return course if possible
parseCourseIn :: Year -> Term -> Parser Course
parseCourseIn year term =
  course <$> (oneUpper pread <|> pure COMP) <*>
             pread <*>
             (pmaybe (oneUpper psection `pminus` yt)) <*>
             (fromMaybe (year, term) <$> pmaybe yt) <* eoargs
 where course dept n section (y, t) = Course dept n y t section
       psection = (NumberedSection <$> pread) `pelse` (NamedSection <$> pstring)
       yt = flip (curry id) <$> pterm <*> (Year <$> pread) <|> oneArg yeart
       pterm = oneLower $ oneArg $ \s -> case s of "spring" -> [Spring]
                                                   "summer" -> [Summer]
                                                   "fall"   -> [Fall]
                                                   _ -> []
       yeart s = [ (year, term) | (year, t) <- reads s, (term, u) <- reads t
                                , ("", "") <- lex u ]

courseTestsParseOK :: Bool
courseTestsParseOK =
  all ok [ ("40", default40)
         , ("comp 40", default40)
         , ("comp 40 fall 2008", orig40)
         , ("comp 40 2008f", orig40)
         , ("150 tw 2012s", Course COMP 150 (Year 2012) Spring
                                   (Just $ NamedSection "TW"))
         , ("en 9 21",  Course EN 9 yyyy term (Just $ NumberedSection 21))
         ]
 where default40 = Course COMP 40 yyyy term Nothing
       orig40    = Course COMP 40 (Year 2008) Fall Nothing
       yyyy = Year 2011
       term = Summer
       ok (s, c) = trace tr (result == Just c)
         where result = uniqueParse (parseCourseIn yyyy term) (words s)
               tr = "Expected " ++ showFullCourse c ++ "; got " ++
                    show (fmap showFullCourse result)
       

courseNamed :: [String] -> IO (Maybe Course)
courseNamed ss = do (year, term) <- nextSemester
                    return $ uniqueParse (parseCourseIn year term) ss

nextSemester :: IO (Year, Term)
nextSemester = do UTCTime { utctDay = day } <- getCurrentTime
                  return $ semOf $ toGregorian day
  where semOf (year, month, day) 
            | month `elem` [1..2] = thisYear Spring
            | month `elem` [3..9] = thisYear Fall
            | otherwise = nextYear Spring
          where thisYear t = (Year year', t)
                nextYear t = (Year (year'+1), t)
                year' = fromIntegral year
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

