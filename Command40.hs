{-# LANGUAGE FlexibleInstances #-}
module Main (main) where

import Data.Acid
import Data.List
import Data.Maybe

import System.Environment
import System.IO

import Auth
import Course
import State
import Updates
import User

myOpen = openLocalFrom "test-state/state/State.Database"

class StringCommand a where
  arity :: a -> Maybe Int
  run :: a -> [String] -> IO ()
  
instance StringCommand (IO ()) where
  arity m = Just 0
  run m [] = m
  run _ _ = error "arity error"
  
instance StringCommand a => StringCommand (String -> a) where
  arity f = fmap (+1) $ arity (f "bogus")
  run f [] = error "not enough arguments"
  run f (s:ss) = run (f s) ss

arityMatch :: Maybe Int -> [a] -> Bool
arityMatch Nothing  _  = True
arityMatch (Just n) as = n == length as

data Command = Command { cmdName :: String
                       , cmdUsage :: String
                       , cmdArity :: Maybe Int
                       , cmdRun :: [String] -> IO ()
                       }
               
command :: StringCommand a => String -> a -> String -> Command
command what f usage = Command what usage (arity f) (run f)

varargs :: String -> ([String] -> IO ()) -> String -> Command
varargs what f usage = Command what usage Nothing f
                 
commands :: [Command]
commands = [ command "adduser" adduser   "adduser email     Add a user"
           , command "addadmin" addadmin "addadmin email    Add a hacker"
           , command "passwd"   passwd   "passwd email      Change a password"
           , command "auth"     auth     "auth email        Test auth code"
           , command "whois"   whois     "whois <name-or-email>   Find a user"
           , addCourseCommand
           ]
  where adduser email  = newUser email [] >>= addUser
        addadmin email = newUser email [] >>= addUser
        addUser user = do acid <- myOpen
                          result <- update acid (AddUser user)
                          putStrLn (show result)
        passwd email = do acid <- myOpen
                          users <- query acid PeekUsers
                          case findUser email users of
                            UserFound user -> -- XXX need some abstraction here
                              do auth <- authPromptFor user
                                 result <- update acid (NewPassword (uid user) auth)
                                 putStrLn (show result)
        
        auth email = do acid <- myOpen
                        auth <- passwordPrompt $ Just $ "Password for " ++ email
                        users <- query acid PeekUsers
                        case findUserPassphrase email auth users of
                            Left user -> putStrLn $ "Authorized " ++ show user
                            Right uf -> putStrLn $ "Failed: " ++ show uf

whois s = do acid <- myOpen
             users <- query acid PeekUsers
             putStrLn $ show $ findUser s users
             
addCourseCommand =
  varargs "addcourse" addcourse "addcourse [dept] number [section] [semester]"

addcourse ss = do course <- courseNamed ss
                  case course of Nothing -> courseError
                                 Just c -> do
                                   acid <- openLocal
                                   result <- update acid (AddCourse c)
                                   putStrLn (show result)
  where courseError = do
          hPutStrLn stderr $
            "Course '" ++ intercalate " " ss ++ "' does not parse!" 
          hPutStr stderr "Usage: "
          commandUsage addCourseCommand

main :: IO ()
main = do args <- getArgs
          case args of
            [] -> usage
            cmd:args ->
              case find ((==cmd) . cmdName) commands of
                Nothing -> usage
                Just command ->
                  if arityMatch (cmdArity command) args then
                    cmdRun command args
                  else
                    putStr "Usage:" >> commandUsage command
  where usage =  do putStrLn $ "Commands:"
                    mapM_ commandUsage commands

commandUsage = putStrLn . ("  " ++) . cmdUsage
