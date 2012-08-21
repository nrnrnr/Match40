{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell, DoAndIfThenElse #-}
module Auth
       ( Authentication
       , fromPassword
       , validateAuth
       , passwordPrompt, authPromptFor
       )
where

import Control.Exception
import Data.ByteString.Char8 hiding (putStr, putStrLn, getLine)
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

authPromptFor :: Show a => a -> IO Authentication
authPromptFor a =
  (passwordPrompt $ Just $ "Password for " ++ show a ++ ": ") >>= fromPassword


passwordPrompt :: Maybe String -> IO String
passwordPrompt prompt = do
  pass  <- getPrompt $ fromMaybe "Password:" prompt ++ " "
  pass' <- getPrompt "Again: "
  if pass /= pass' then
    putStrLn "Inconsistent passwords" >> passwordPrompt prompt
  else
    return pass
 where
  getPrompt s = do putStr s
                   hFlush stdout
                   pass <- withEcho False getLine
                   putChar '\n'
                   return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action
