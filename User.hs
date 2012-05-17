{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
module User
       ( Role(..), Controlled(..), TimeOfRole(..)
       , Profile(..), Fullname(..), Photo(..), Phone(..), Email(..)
       , newUtln, newCs
       , parseName
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



newUser :: UserIdent -> String -> [Role] -> IO Profile
newUser ident name roles =
  do auth <- passwordPrompt $ Just $ "Password for " ++ show ident ++ ":"
     return Profile { roles = map private roles
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
  
newUtln, newCs :: String -> String -> [Role] -> IO Profile
newUtln = newUser . UTLN
newCs   = newUser . CsUid


public, private :: a -> Controlled a
public a = ClassmateSees Current a

private a = InstructorSees Current a


