{-# LANGUAGE OverloadedStrings #-}

module Pages.User ( renderUserLineFrom
                  , defaultThumbnail
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
  toHtml thumb ! A.alt "thumbnail image" ! A.height (fromString $ show 25)
  " "
  (H.toHtml . show . name . profile) user
  (H.toHtml . commafy . map show . catMaybes . map cansee . roles . profile) user
  where cansee = sees viewroles user
        commafy [] = ""
        commafy xs = " (" ++ intercalate ", " xs ++ ")"
        thumb =
          fromMaybe defaultThumbnail (join $ cansee $ thumbnail $ profile user)
        
instance ToHtml Photo where
  toHtml (Photo path) = H.img ! A.src (fromString $ "/images/" ++ path)
        
                     
                     
defaultThumbnail :: Photo
defaultThumbnail = Photo "elephant.jpg"
