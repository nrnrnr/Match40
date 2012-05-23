module Permissions
       ( sees
       )
       where

import User

sees :: [Role] -> User -> Controlled a -> Maybe a
sees _ _ (UserSees a) = Just a
sees viewroles _ c | any isAdmin viewroles = Just $ unControlled c
  where isAdmin Admin = True
        isAdmin _ = False
sees viewroles user (InstructorSees time a) =
  a `whenAny` [ c == c' | Student c <- userRoles user, Instructor c' <- viewroles ]
  -- XXX TODO ignores time
sees viewroles user (ClassmateSees time a) =
  a `whenAny` [ c == c' | Student c <- userRoles user, Student c' <- viewroles ]
  -- XXX TODO ignores time
sees viewroles user (TASees time a) =
  a `whenAny` [ c == c' | Student c <- userRoles user, TA c' <- viewroles ]
  -- XXX TODO ignores time
  
whenAny a ps = if or ps then Just a else Nothing
userRoles = map unControlled . roles . profile
