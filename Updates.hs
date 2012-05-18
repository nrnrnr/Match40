{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell, RankNTypes #-}
module Updates
where
  
import Control.Monad.State                   ( get, put )
import Control.Monad.Reader                  ( ask )
import Control.Applicative                   ( (<$>) )

import Data.Acid
import Data.SafeCopy
import Data.Typeable

import Auth
import Identity
import State
import User


data UpdateResult = UpdateOK | UserNotFound UserIdent
  deriving (Typeable)
$(deriveSafeCopy 0 'base ''UpdateResult)

type UserUpdate a = UserIdent -> a -> Update Database UpdateResult

{-
newPassword  :: UserUpdate Authentication
newFirstname :: UserUpdate String
newPortrait  :: UserUpdate Photo
newThumbnail :: UserUpdate Photo
newBlurb     :: UserUpdate String
newPhone     :: UserUpdate Phone
newEmail     :: UserUpdate Email
-}
newPassword  :: UserIdent -> Authentication -> Update Database UpdateResult
newFirstname :: UserIdent -> String         -> Update Database UpdateResult
newPortrait  :: UserIdent -> Photo          -> Update Database UpdateResult
newThumbnail :: UserIdent -> Photo          -> Update Database UpdateResult
newBlurb     :: UserIdent -> String         -> Update Database UpdateResult
newPhone     :: UserIdent -> Phone          -> Update Database UpdateResult
newEmail     :: UserIdent -> Email          -> Update Database UpdateResult

profileUpdate :: (a -> Profile -> Profile) -> UserUpdate a
profileUpdate addToProf userid a = do
    db <- get
    return UpdateOK
    change (users db)
           (\us -> put (db { users = us }) >> return UpdateOK)
           (return $ UserNotFound userid)
  where change :: forall a . [User] -> ([User] -> a) -> a -> a
        change (u:us) succ fail =
          if uid u == userid then succ (update u : us)
          else change us (succ . (u:)) fail
        change [] succ fail = fail
        update u = u { profile = addToProf a (profile u) }
          
          
inject :: a -> Controlled (Maybe a) -> Controlled (Maybe a)
inject = fmap . const . Just

newPassword = profileUpdate $ \auth p -> p { auth = auth }
newPortrait = profileUpdate $
              \photo p -> p { portrait = inject photo (portrait p) }
newThumbnail = profileUpdate $
              \photo p -> p { thumbnail = inject photo (thumbnail p) }
newBlurb = profileUpdate $ \blurb p -> p { blurb = Just blurb }
newPhone = profileUpdate $ \ph p -> p { phone = inject ph (phone p) }
newEmail = profileUpdate $ \em p -> p { email = inject em (email p) }
newFirstname = profileUpdate $ \first p -> p { name = newFirst (name p) first }
  where newFirst full first = full { firstName = first }


$(makeAcidic ''Database [ 'newPassword, 'newFirstname, 'newPortrait
                        , 'newThumbnail, 'newBlurb, 'newPhone, 'newEmail
                        ])

