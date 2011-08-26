module CourseMail
  ( offer
  )
where

import Prelude hiding (lines)

import Email
import qualified Policy
import Student

coursename = Policy.coursename

offer :: Student -> Student -> IO ()
offer by to = do { send msg; return () }
  where msg = Message { to = email to
                      , from = coursename ++ " matcher <" ++
                               Policy.staffmail ++ ">"
                      , subject = "Offer of " ++ coursename ++
                                  " partnership from " ++ readableName by
                      , reply_to = Just (email by)
                      , body = body
                      }
        body = lines [ firstname to ++ ","
                     , ""
                     , readableName by ++ " has invited you to form a partnership" ++
                                    " for " ++ coursename ++ "."
                     , "To respond to this invitation, log in at " ++
                       Policy.loginURL to
                     , ""
                     , ""
                     , "Yours sincerely,"
                     , ""
                     , Policy.signature
                     ]



lines = concat . addNewlines
    where addNewlines [] = []
          addNewlines (s:ss) = s : "\n" : addNewlines ss
