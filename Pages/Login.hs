{-# LANGUAGE OverloadedStrings #-}

module Pages.Login (loginPage) where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze ((!))

import Pages.Components (dateStr)
import Pages.AppTemplate (appTemplate)
import Student
import DB

loginPage :: Student -> H.Html
loginPage student =
  appTemplate "Student Profile" [] (loginDisplay student)
      
      
loginDisplay student =
  H.toHtml $ readableName student