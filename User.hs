{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell, RecordWildCards #-}
module User
       ( Role(..), Controlled(..), TimeOfRole(..)
       , User(..), Profile(..), Fullname(..), Photo(..), Phone(..), Email(..)
       , PartnerData(..), Orientation(..), SocialNetwork(..), PartnerBreadth(..)
       , UserNumber(..)
       , newUser, nameOfEmail
       , parseName, noPartnerData

       , User0, numberUser -- migration
       )
where

import Data.Char
import Data.SafeCopy
import Data.Typeable  

import Auth
import Course
import Identity

data Role = Instructor Course
          | Student Course
          | TA Course
          | Former Role
          | Admin
$(deriveSafeCopy 0 'base ''Role)

instance Show Role where
  show (Instructor c) = show c ++ " instructor"
  show (Student c) = show c ++ " student"
  show (TA c)      = show c ++ " TA"
  show (Former r)  = "former " ++ show r
  show Admin       = "hacker"

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
  deriving (Eq)
$(deriveSafeCopy 0 'base ''Fullname)
  
instance Show Fullname where
  show name = firstName name ++ " " ++ lastName name

instance Ord Fullname where
  compare n n' = compare (parts n) (parts n')
    where parts n = (map toLower (lastName n), map toLower (firstName n),
                     (lastName n, firstName n, fullName n))

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

newtype UserNumber = UserNumber Int
$(deriveSafeCopy 0 'base ''UserNumber)

instance Show UserNumber where
    show (UserNumber n) = show n

-- | What we know about a user.  Except for the profile,
-- only an instructor or administrator can change what's here.
data User = User { uid      :: UserIdent
                 , userNumber :: UserNumber
                 , otherIds :: [OtherIdent]
                 , synonyms :: [String]
                 , profile  :: Profile
                 , partnerData :: PartnerData
                 }
  deriving (Typeable)
$(deriveSafeCopy 1 'extension ''User)



instance Eq User where
  u == u' = uid u == uid u'
  

instance Ord User where
  compare u u' = compare (parts u) (parts u')
    where parts u = (fullName $ name $ profile u, uid u)

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
                   , userNumber =
                       error "user number assigned on insertion into database"
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

------------ old types


data User0 = User0 { uid0      :: UserIdent
                   , otherIds0 :: [OtherIdent]
                   , synonyms0 :: [String]
                   , profile0  :: Profile
                   , partnerData0 :: PartnerData
                   }
  deriving (Typeable)
$(deriveSafeCopy 0 'base ''User0)

numberUser :: UserNumber -> User0 -> User
numberUser n (User0 {..}) = User { userNumber = n
                                 , uid = uid0, otherIds = otherIds0
                                 , synonyms = synonyms0, profile = profile0
                                 , partnerData = partnerData0
                                 }

instance Migrate User where
  type MigrateFrom User = User0
  migrate = numberUser (error "direct migration without user number")
