{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
module User
       ( Role(..), Controlled(..), TimeOfRole(..)
       , User(..), Profile(..), Fullname(..), Photo(..), Phone(..), Email(..)
       , PartnerData(..), Orientation(..), SocialNetwork(..), PartnerBreadth(..)
       , newUser, nameOfEmail
       , parseName, noPartnerData
       )
where

import Data.Char
import Data.SafeCopy
  
import Auth
import Course
import Identity

data Role = Instructor Course
          | Student Course
          | TA Course
          | Former Role
          | Admin
$(deriveSafeCopy 0 'base ''Role)

data TimeOfRole = AnyTime | Current | Past
$(deriveSafeCopy 0 'base ''TimeOfRole)

data Controlled a -- ^ Controlled visibility
  = UserSees       { unControlled :: a } -- ^ Any user can see this info
  | InstructorSees { roleTime :: TimeOfRole, unControlled :: a }
  | ClassmateSees  { roleTime :: TimeOfRole, unControlled :: a }
  | TASees         { roleTime :: TimeOfRole, unControlled :: a }
$(deriveSafeCopy 0 'base ''Controlled)

instance Functor Controlled where
  fmap f (UserSees a) = UserSees (f a)
  fmap f (InstructorSees time a) = InstructorSees time (f a)
  fmap f (ClassmateSees time a) = ClassmateSees time (f a)
  fmap f (TASees time a) = TASees time (f a)

    
data Fullname = Fullname { firstName :: String -- ^ student's preferred name
                         , lastName :: String
                         , fullName :: String -- ^ official full name
                         }
$(deriveSafeCopy 0 'base ''Fullname)
  
instance Show Fullname where
  show name = firstName name ++ " " ++ lastName name

newtype Photo = Photo { unPhoto :: String } -- ^ pathname to somewhere
newtype Phone = Phone { unPhone :: String }
newtype Email = Email { unEmail :: String }

$(deriveSafeCopy 0 'base ''Photo)
$(deriveSafeCopy 0 'base ''Phone)
$(deriveSafeCopy 0 'base ''Email)


-- | Information stored about a user which that user can change.
-- (N.B. Admin controls roles but user controls their visibility.)
data Profile = Profile { auth  :: Authentication
                       , name  :: Fullname
                       , roles :: [Controlled Role]
                       , schedulePrefs :: Controlled ()
                       , portrait :: Controlled (Maybe Photo)
                       , thumbnail :: Controlled (Maybe Photo)
                       , blurb :: Maybe String
                       , phone :: Controlled (Maybe Phone)
                       , email :: Controlled (Maybe Email)
                       }
$(deriveSafeCopy 0 'base ''Profile)

parseName :: String -> Fullname
parseName full = Fullname first last full
  where first = takeWhile (not . isSpace) full
        last  = reverse . takeWhile (not . isSpace) . reverse $ full


data Orientation = FacesOut | FacesIn
data SocialNetwork = ManyFriendsIn Course
                   | FewFriendsIn Course
data PartnerBreadth = AnyPartner | LimitedPartner
  -- ^ @LimitedPartner@ is for known risk groups like first-generation
  -- college students.  The software will try to avoid pairing two
  -- people tagged LimitedPartner
$(deriveSafeCopy 0 'base ''Orientation)
$(deriveSafeCopy 0 'base ''SocialNetwork)
$(deriveSafeCopy 0 'base ''PartnerBreadth)

-- | Data provided by instructors to better match students.
data PartnerData = PartnerData { orientation :: Maybe Orientation
                               , socialNetwork :: [SocialNetwork]
                               , breadth :: Maybe PartnerBreadth
                               } 
$(deriveSafeCopy 0 'base ''PartnerData)
noPartnerData :: PartnerData
noPartnerData = PartnerData { orientation = Nothing
                            , socialNetwork = []
                            , breadth = Nothing
                            }


-- | What we know about a user.  Except for the profile,
-- only an instructor or administrator can change what's here.
data User = User { uid      :: UserIdent
                 , otherIds :: [OtherIdent]
                 , synonyms :: [String]
                 , profile  :: Profile
                 , partnerData :: PartnerData
                 }
$(deriveSafeCopy 0 'base ''User)

instance Show User where
  show u = show (name $ profile u) ++ " <" ++ show (uid u) ++ ">"
  



newUser :: String -> [Role] -> IO User
newUser email roles =
  do auth <- passwordPrompt $ Just $ "Password for " ++ show name ++ ":"
     let prof = Profile { roles = map private roles
                        , auth = auth
                        , name = name
                        , schedulePrefs = public ()
                        , portrait = public Nothing
                        , thumbnail = public Nothing
                        , blurb = Nothing
                        , phone = private Nothing
                        , email = public Nothing
                        }
     return $ User { profile = prof
                   , synonyms = []
                   , partnerData = noPartnerData
                   , uid = SISEmail email
                   , otherIds = [ ]
                   }
 where name = nameOfEmail email

nameOfEmail :: String -> Fullname  
nameOfEmail = parseName . map convert . takeWhile (/= '@')
  where convert '_' = '-'
        convert '.' = ' '
        convert c = c


public, private :: a -> Controlled a
public a = ClassmateSees Current a

private a = InstructorSees Current a


