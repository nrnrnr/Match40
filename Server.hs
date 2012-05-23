{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Server 
       ( login
       )
where


import Control.Applicative ((<$>), optional)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Happstack.Lite
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

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
                    input ! type_ "text" ! A.id "userid" ! name "userid"
                    " (this is often your Tufts email address)"
                    H.br
                    label ! A.for "passphrase" $ "Your passphrase: "
                    input ! type_ "text" ! A.id "passphrase" ! name "passphrase"
                    H.br
                    input ! type_ "submit" ! name "signin"
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
      

