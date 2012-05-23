{-# LANGUAGE OverloadedStrings #-}

module Pages.User ( renderUserLineFrom
                  )
where
  
import Control.Monad
import Data.List
import Data.Maybe
import Data.String

import           Text.Blaze.Html5 ((!), ToHtml(..))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Pages.AppTemplate
import Permissions
import User

template s = appTemplate []

renderUserLineFrom :: [Role] -> User -> H.Html
renderUserLineFrom viewroles user = do
  toHtml thumb ! A.alt "thumbnail image"
  (H.toHtml . show . name . profile) user
  (H.toHtml . commafy . map show . catMaybes . map cansee . roles . profile) user
  where cansee = sees viewroles user
        commafy [] = ""
        commafy xs = " (" ++ intercalate ", " xs ++ ")"
        thumb =
          fromMaybe defaultThumb (join $ cansee $ thumbnail $ profile user)
        
instance ToHtml Photo where
  toHtml (Photo path) = H.img ! A.src (fromString $ "/images/" ++ path)
        
                     
                     
defaultThumb :: Photo
defaultThumb = Photo "defthumb.jpg"
