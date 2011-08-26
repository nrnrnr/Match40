{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, 
    MultiParamTypeClasses, TemplateHaskell, TypeFamilies #-}
module Auth
where
import Control.Monad.Reader

import Happstack.State  ( Query, query, mkMethods )

import DB
import State
import Student

-- getAuthedStudent :: (MonadIO m) => String -> m (Maybe Student) 
-- getAuthedStudent id = 
  -- do students <- query PeekStudents 
     -- case filter (\s -> urlid s == id) students of 
       -- [s] -> return $ Just s 
       -- _   -> return Nothing 

