{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, 
    MultiParamTypeClasses, TemplateHaskell, TypeFamilies,
    FlexibleInstances #-}
module State
where
import Control.Applicative ((<$>))
import Control.Monad.Reader (ask, MonadIO)
import Control.Monad.State (modify)
import Data.Typeable

import Happstack.State ( Component(..), End, Proxy(..), Query, Update, Version
                       , createCheckpoint, deriveSerialize, mkMethods, query
                       , startSystemState, shutdownSystem, update
                       )


import DB
import Invitation
import Student
import qualified ToyRoster

-- | State

-- The "Version" instance is used for migration...
-- Unfortunately, "migration" is cover in a later section o_0
instance Version Enrollment
$(deriveSerialize ''Enrollment)
instance Version Database 
$(deriveSerialize ''Database) 
instance Version Student
$(deriveSerialize ''Student)
instance Version FullName
$(deriveSerialize ''FullName)
instance Version History
$(deriveSerialize ''History)
instance Version Project
$(deriveSerialize ''Project)
instance Version Invitation
$(deriveSerialize ''Invitation)
instance Version Status
$(deriveSerialize ''Status)

instance Component Database where
  type Dependencies Database = End
  initialValue = Database ToyRoster.roster (History []) Nothing

peekStudents :: Query Database [Student]
peekStudents = students <$> ask

addStudent :: Student -> Update Database ()
addStudent s = modify $ \(Database ss h p) -> Database (s:ss) h p

peekHistory :: Query Database History
peekHistory = projHistory <$> ask

setHistory :: History -> Update Database ()
setHistory h = modify $ \(Database s _ p) -> Database s h p

peekProject :: Query Database (Maybe Project)
peekProject = project <$> ask

setProject :: Project -> Update Database ()
setProject p = modify $ \(Database s h _) -> Database s h (Just p)

-- Register functions with Happstack.State
$(mkMethods ''Database [ 'peekStudents, 'addStudent
                       , 'peekHistory, 'setHistory
                       , 'peekProject, 'setProject]) 

-- Abstract the use of 'query' so we don't have to use it everywhere
getStudents :: (MonadIO m) => m [Student]
getStudents = query PeekStudents

getProject :: (MonadIO m) => m (Maybe Project)
getProject = query PeekProject

getHistory :: (MonadIO m) => m History
getHistory = query PeekHistory

