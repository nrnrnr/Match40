{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
module User
       ( Role(..), Controlled(..), TimeOfRole(..)
       , User(..), Profile(..), Fullname(..), Photo(..), Phone(..), Email(..)
       , PartnerData(..), Orientation(..), SocialNetwork(..), PartnerBreadth(..)
       , newUtln, newCs
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
    
data Fullname = Fullname { firstName :: String -- ^ student's preferred name
                         , lastName :: String
                         , fullName :: String -- ^ official full name
                         }
$(deriveSafeCopy 0 'base ''Fullname)

newtype Photo = Photo { unPhoto :: String } -- ^ pathname to somewhere
newtype Phone = Phone { unPhone :: String }
newtype Email = Email { unEmail :: String }

$(deriveSafeCopy 0 'base ''Photo)
$(deriveSafeCopy 0 'base ''Phone)
$(deriveSafeCopy 0 'base ''Email)


data Profile = Profile { roles :: [Controlled Role]
                       , uid  :: UserIdent
                       , auth :: Authentication
                       , name :: Fullname
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


data User = User { profile :: Profile
                 , synonyms :: [String]
                 , partnerData :: PartnerData
                 }
$(deriveSafeCopy 0 'base ''User)




newUser :: UserIdent -> String -> [Role] -> IO User
newUser ident name roles =
  do auth <- passwordPrompt $ Just $ "Password for " ++ show ident ++ ":"
     let prof = Profile { roles = map private roles
                        , uid = ident
                        , auth = auth
                        , name = parseName name
                        , schedulePrefs = public ()
                        , portrait = public Nothing
                        , thumbnail = public Nothing
                        , blurb = Nothing
                        , phone = private Nothing
                        , email = public Nothing
                        }
     return $ User prof [] noPartnerData
  
newUtln, newCs :: String -> String -> [Role] -> IO User
newUtln = newUser . UTLN
newCs   = newUser . CsUid


public, private :: a -> Controlled a
public a = ClassmateSees Current a

private a = InstructorSees Current a


