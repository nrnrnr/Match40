{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}
module Main (main) where

import Data.Acid

import System.Environment
import System.IO

import Updates
import User




main :: IO ()
main = do args <- getArgs
          case args of
            ["adduser", email]
              -> newUser email [] >>= addUser
            ["addadmin", email]
              -> newUser email [Admin] >>= addUser
            _ -> do putStrLn $ "Commands:"
                    putStrLn $ "  adduser email      Add a user."
  where addUser user = do acid <- openLocal
                          result <- update acid (AddUser user)
                          putStrLn (show result)
