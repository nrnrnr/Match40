{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Server 
       ( login, shortUsersPage
       )
where


import Control.Applicative ((<$>), optional)
import Data.Acid
import Data.Acid.Advanced
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Happstack.Lite
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

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
             userid <- lookText "userid"
             passphrase <- lookText "passphrase"
             addCookies [(Session, mkCookie "fortune" "your mama")]
             seeOther ("/echo/" ++ unpack userid ++ "ZZZ" ++ unpack passphrase :: String) (toResponse ())

      lossage :: ServerPart Response
      lossage = seeOther ("/lossage" :: String) (toResponse ())
      

shortUsersPage :: (Text -> Html -> Response) -> AcidState Database
               -> ServerPart Response
shortUsersPage template acid = do
  users <- query' acid PeekUsers
  shortUsers template [Admin] users


shortUsers :: (Text -> Html -> Response) -> [Role] -> [User] -> ServerPart Response
shortUsers template roles users = 
  ok $ template "User List" $ do
    H.p $ H.toHtml $ "The system has " ++ show (length users) ++ " users."
    mapM_ (H.p . renderUserLineFrom roles) users

