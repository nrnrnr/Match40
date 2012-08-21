{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Server 
       ( login, shortUsersPage, dblogin
       , servePhoto, safePhotoServing
       , authCookieName
       )
where



import Control.Applicative ((<$>), optional)
import Control.Monad.Trans.Class
import Data.Acid
import Data.Acid.Advanced
import Data.List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (unpack, pack)
import qualified Data.Text.Lazy as L
import Happstack.Lite
import Happstack.Server.Cookie (addCookie)
import Happstack.Server.FileServe.BuildingBlocks (guessContentType)
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import System.Entropy

import Auth
import Pages.User
import State
import Updates
import User

login :: (Text -> Html -> Response) -> ServerPart Response
login template = msum [ view, update, lossage ]
    where
      view :: ServerPart Response
      view =
          do method GET
             ok $ template "fortune" $ do
                    H.p "Sign in:"
                    form ! action "/xlogin" ! enctype "multipart/form-data" ! A.method "POST" $ do
                    label ! A.for "userid" $ "Your user id: "
                    input ! type_ "text" ! A.id "userid" ! A.name "userid"
                    " (this is often your Tufts email address)"
                    H.br
                    label ! A.for "passphrase" $ "Your passphrase: "
                    input ! type_ "text" ! A.id "passphrase" ! A.name "passphrase"
                    H.br
                    input ! type_ "submit" ! A.name "signin"
                          ! value "Sign in"

      update :: ServerPart Response
      update =
          do method POST
             userid <- lookText "userid"
             passphrase <- lookText "passphrase"
             addCookies [(Session, mkCookie "fortune" "your mama")]
             seeOther ("/echo/" ++ unpack userid ++ "ZZZ" ++ unpack passphrase :: String) (toResponse ())

      lossage :: ServerPart Response
      lossage = seeOther ("/lossage" :: String) (toResponse ())
      

dblogin :: (Text -> Html -> Response) -> AcidState Database -> ServerPart Response
dblogin template acid = msum [ view, update, lossage ]
    where
      view :: ServerPart Response
      view =
          do method GET
             ok $ template "login" $ do
                    H.p "Sign in:"
                    form ! action "/login" ! enctype "multipart/form-data" ! A.method "POST" $ do
                    label ! A.for "userid" $ "Your user id: "
                    input ! type_ "text" ! A.id "userid" ! A.name "userid"
                    " (this is often your Tufts email address)"
                    H.br
                    label ! A.for "passphrase" $ "Your passphrase: "
                    input ! type_ "text" ! A.id "passphrase" ! A.name "passphrase"
                    H.br
                    input ! type_ "submit" ! A.name "signin"
                          ! value "Sign in"

      update :: ServerPart Response
      update =
          do method POST
             users <- query' acid PeekUsers      
             userid <- lookText "userid"
             passphrase <- lookText "passphrase"
             msum [ allowed users (unpack userid) (unpack passphrase)
                  , denied "access denied"
                  ]

      denied :: String -> ServerPart Response
      denied why = ok $ template "Access Denied" $ do
        H.p $ H.toHtml $ "Authentication failed; " ++ why ++ "."

      allowed :: [User] -> String -> String -> ServerPart Response
      allowed users userid passphrase = 
        case findUser userid users of
          UserFound user ->
            if validateAuth (auth (profile user)) passphrase then
              do bits <- lift $ getEntropy 16
                 let hash = show (uid user) ++ "|" ++ show bits
                 addCookie Session (authCookie hash)
                 -- store a fast hash of this cookie in the DB?
                 ok $ template "In" $ H.p $ H.html "You are authorized!"
            else
              ok $ template "Testing" $ H.p "bad passphrase"
          bad -> ok $ template "Trouble" $ do
            H.p $ H.toHtml $
              "Tried to find " ++ show userid ++ ", but " ++ show bad ++ "."
          {-
             addCookies [(Session, mkCookie "fortune" "your mama")]
             seeOther ("/echo/" ++ unpack userid ++ "ZZZ" ++ unpack passphrase :: String) (toResponse ())
           -}

      lossage :: ServerPart Response
      lossage = seeOther ("/lossage" :: String) (toResponse ())

authCookieName = "match40-credential"

authCookie hash = private $ mkCookie authCookieName hash 
  where _rivate cookie = cookie { secure = True, httpOnly = True } -- XXX needs SSL
        private cookie = cookie { httpOnly = True }

shortUsersPage :: (Text -> Html -> Response) -> AcidState Database
               -> ServerPart Response
shortUsersPage template acid = do
  users <- query' acid PeekUsers
  shortUsers template [Admin] (sort users)


shortUsers :: (Text -> Html -> Response) -> [Role] -> [User] -> ServerPart Response
shortUsers template roles users = 
  ok $ template "User List" $ do
    H.p $ H.toHtml $ "The system has " ++ show (length users) ++ " users."
    mapM_ (H.p . renderUserLineFrom roles) users


servePhoto :: Photo -> ServerPart Response
servePhoto photo = serveFile (asContentType mime_ty) path
  where mime_ty = fromMaybe "image/jpeg" (guessContentType mimeTypes path)
        path = photoPath photo
        
photoPath (Photo path) = "images/" ++ path

safePhotoServing :: String -> ServerPart Response
safePhotoServing s =
  if any (== '/') s then servePhoto defaultThumbnail
  else servePhoto (Photo s)
