{-# LANGUAGE OverloadedStrings #-}

module Pages.Private (privatePage) where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze ((!))

import Pages.Components (dateStr)
import Pages.AppTemplate (appTemplate)
import Student
import Invitation
import DB

privatePage :: [Invitation] -> H.Html
privatePage invitations =
  appTemplate "Invitations" [] (invitationDisplay invitations)
      
      
invitationDisplay entries =
  H.div ! A.class_ "invitations" $ do
    H.h2 ! A.class_ "invitation_title" $ "Pending Invitations"
    H.div ! A.class_ "clear" $ do
      H.ul ! A.class_ "invitationlist" $ do
       sequence_ . map invitationXML $ zip entries (cycle [False,True])

invitationXML :: (Invitation, Bool) -> H.Html  
invitationXML ((I offeredBy offeredTo status history), alt) =  
  H.li ! A.class_ (if alt then "alt" else "") $ do
    H.span $ H.toHtml $ readableName offeredBy
    H.span $ H.toHtml $ show status