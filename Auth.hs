{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
module Auth
       ( Authentication
       , fromPassword
       , validateAuth
       , passwordPrompt
       )
where

import Control.Exception
import Data.ByteString.Char8 hiding (putStr, getLine)
import Data.Maybe
import Data.SafeCopy
import System.IO

import Crypto.BCrypt 

p = Data.ByteString.Char8.pack

               
data Authentication = Authentication { hashedPassword :: ByteString } 
$(deriveSafeCopy 0 'base ''Authentication)



fromPassword :: String -> IO Authentication
fromPassword s =
  do hashed <- hashPasswordUsingPolicy slowerBcryptHashingPolicy (p s)
     return $ case hashed of Nothing -> error "bcrypt internal settings invalid"
                             Just hash -> Authentication hash

validateAuth :: Authentication -> String -> Bool
validateAuth auth = validatePassword (hashedPassword auth) . p

passwordPrompt :: Maybe String -> IO Authentication
passwordPrompt prompt = do
  putStr $ fromMaybe "Password: " prompt
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  fromPassword pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action