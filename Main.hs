{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Main where

import Prelude                 hiding (head)

import Control.Monad           (msum)
import Control.Monad.Reader
import Data.Data               (Data, Typeable)
import Data.Monoid             (mconcat)
import Happstack.Server        ( Response, ServerPartT, ok, toResponse
                               , simpleHTTP, nullConf, seeOther, dir, notFound
                               , seeOther
                               )
import Text.Blaze.Html4.Strict ( (!), html, head, body, title, p, toHtml
                               , toValue, ol, li, a
                               , Html
                               )
import Text.Blaze.Html4.Strict.Attributes (href)
import Web.Routes              ( PathInfo(..), RouteT, showURL
                               , runRouteT, liftRouteT
                               , Site(..), setDefault, mkSitePI)
import Web.Routes.TH           (derivePathInfo)
import Web.Routes.Happstack    (implSite)

import State
import Student

newtype PrivateStudentId = PrivateStudentId String
  deriving (Eq, Ord, Read, Show, PathInfo, Typeable)

data Sitemap
    = Home
    | Private PrivateStudentId
      deriving (Eq, Ord, Read, Show, Typeable)

$(derivePathInfo ''Sitemap)

route :: Sitemap -> RouteT Sitemap (ServerPartT IO) Response
route url =
    case url of
      Home           -> homePage
      (Private pSiD) -> privatePage pSiD

toyStudent = Student { name = fullName "Bruce Springsteen"
                     , email = "jamslam@gmail.com"
                     , aboutMe = "Singer"
                     , photo = Nothing
                     , enrollment = Enrolled
                     }

homePage :: RouteT Sitemap (ServerPartT IO) Response
homePage = 
    do students <- liftRouteT $ liftIO getStudents
       ok $ toResponse $ 
          html $ do
            head $ title $ (toHtml "Welcome Home!")
            body $ do
              ol $ mconcat (map mkStudent students)
    where
      mkStudent :: Student -> Html
      mkStudent s =
          li $ a ! href (toValue "blah") $ toHtml $ "Student blah blah"
          -- do url <- showURL (Private pSiD) 
             -- return $ li $ a ! href (toValue url) $  
                        -- toHtml $ "Student " ++ (show $ pSiD) 
          -- where pSiD = PrivateStudentId (show c) 

privatePage  :: PrivateStudentId -> RouteT Sitemap (ServerPartT IO) Response
privatePage pSiD =
    do homeURL <- showURL Home
       ok $ toResponse $ 
          html $ do
            head $ title $ (toHtml $ "Student " ++ show pSiD)
            body $ do
                   p $ toHtml $ "You are now on your profile" ++ show pSiD
                   p $ do toHtml "Click "
                          a ! href (toValue homeURL) $ toHtml "here"
                          toHtml " to return home."

site :: Site Sitemap (ServerPartT IO Response)
site = setDefault Home $ mkSitePI (runRouteT route)

main :: IO ()
main = simpleHTTP nullConf $ 
       msum [ dir "favicon.ico" $ notFound (toResponse ())
            , implSite "http://localhost:8000/" "" site
            , seeOther "" (toResponse ())
            ]
       
