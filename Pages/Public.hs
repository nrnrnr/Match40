{-# LANGUAGE OverloadedStrings #-}

module Pages.Public (publicPage) where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze ((!))

import Pages.Components (dateStr)
import Pages.AppTemplate (appTemplate)
import Student
import DB

publicPage :: Student -> H.Html
publicPage student =
  appTemplate "Student Profile" [] (profileDisplay student)
      
      
profileDisplay student =
  H.toHtml $ readableName student