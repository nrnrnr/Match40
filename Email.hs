module Email
  ( send
  , Message(..)
  )
where

import System.Exit
import System.IO

import qualified System.Process as P



type Address = String -- ^ An email address.  String will do for now
data Message = Message { to :: Address
                       , from :: Address
                       , subject :: String
                       , body :: String
                       , reply_to :: Maybe Address
                       }

sendmail = "/usr/sbin/sendmail"

headers = [ ("To",   Just . to)
          , ("From", Just . from)
          , ("Subject", Just. subject)
          , ("Reply-To", reply_to)
          ]

send :: Message -> IO ExitCode
send msg =
  do (Just inh, Just outh, _, pid) <-
        P.createProcess (P.proc sendmail ["-t"])
             { P.std_in  = P.CreatePipe
             , P.std_out = P.Inherit
             , P.std_err = P.Inherit }
     mapM_ (putHeader inh msg) headers
     hPutStr inh "\n"
     hPutStr inh (body msg)
     hPutStr inh "\n" -- too lazy to check for newline in body
     hFlush inh
     hClose inh

     P.waitForProcess pid

 where putHeader inh msg (name, f) =
           case f msg of Nothing -> return ()
                         Just h -> mapM_ (hPutStr inh) [name, ": ", h, "\n"]
