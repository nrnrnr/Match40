{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, 
    MultiParamTypeClasses, TemplateHaskell, TypeFamilies #-}
module Auth
where
import Control.Monad.Reader

import State
import Student

getAuthedStudent :: (MonadIO m) => String -> m (Maybe Student)
getAuthedStudent id =
  do students <- getStudents
     case filter (\s -> hash s == id) students of
       [s] -> return $ Just s
       _   -> return Nothing

