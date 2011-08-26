{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Main where

import Prelude                 hiding (head)

import Control.Exception       (bracket)
import Control.Monad           (msum)
import Control.Monad.Reader
import Data.Data               (Data, Typeable)
import Data.Monoid             (mconcat)
import Happstack.Server        ( Response, ServerPartT, ok, toResponse
                               , simpleHTTP, nullConf, seeOther, dir, notFound
                               , seeOther
                               )
import Happstack.State         ( Proxy(..), createCheckpoint
                               , startSystemState, shutdownSystem
                               )
import Text.Blaze.Internal     (HtmlM)
import Text.Blaze.Html4.Strict ( (!), html, head, body, title, p, toHtml
                               , toValue, ol, li, a
                               , Html
                               )
import Text.Blaze.Html4.Strict.Attributes (href)
import Web.Routes              ( PathInfo(..), RouteT, showURL
                               , runRouteT, liftRouteT
                               , Site(..), setDefault, mkSitePI
                               , ShowURL, Link
                               )
import Web.Routes.TH           (derivePathInfo)
import Web.Routes.Happstack    (implSite)

import Auth
import DB
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

homePage :: RouteT Sitemap (ServerPartT IO) Response
homePage = 
    do students <- liftRouteT $ liftIO getStudents
       pairs <- sequence $ 
                map (\s -> do { url <- privateUrl s; return (s, url) }) 
                students
       ok $ toResponse $ 
          html $ do
            head $ title $ (toHtml "Welcome Home!")
            body $ do
              ol $ mconcat (map mkStudent pairs)
    where
      privateUrl :: Student -> RouteT Sitemap (ServerPartT IO) Link
      privateUrl s = showURL (Private $ PrivateStudentId $ hash s)
      

      mkStudent :: (Student, Link) -> Html
      mkStudent (s, url) =
             li $ a ! href (toValue $ url) $ toHtml $ readableName s

privatePage  :: PrivateStudentId -> RouteT Sitemap (ServerPartT IO) Response
privatePage (PrivateStudentId pSiD) =
    do homeURL <- showURL Home
       s <- liftRouteT $ liftIO $ getAuthedStudent pSiD
       ok $ toResponse $
          html $ do
            head $ title $ (toHtml $ "Student " ++ show s)
            body $ do
                   p $ toHtml $ "You are now on your profile: " ++ show s
                   p $ do toHtml "Click "
                          a ! href (toValue homeURL) $ toHtml "here"
                          toHtml " to return home."

site :: Site Sitemap (ServerPartT IO Response)
site = setDefault Home $ mkSitePI (runRouteT route)

main :: IO ()
main = do bracket (startSystemState (Proxy :: Proxy Database))
                  createCheckpointAndShutdown $
                    \_control -> simpleHTTP nullConf $
                      msum [ dir "favicon.ico" $ notFound (toResponse ())
                           , implSite "http://localhost:8000/" "" site
                           , seeOther "" (toResponse ())
                           ]
       where
          createCheckpointAndShutdown control =
            do createCheckpoint control
               shutdownSystem control 
       
