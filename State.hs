{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, 
    MultiParamTypeClasses, TemplateHaskell, TypeFamilies #-}
module State
where
import Control.Applicative ((<$>))
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Data.Typeable

import Happstack.State ( Component(..), End, Proxy(..), Query, Update, Version
                       , createCheckpoint, deriveSerialize, mkMethods, query
                       , startSystemState, shutdownSystem, update
                       )


import DB
import Invitation
import Types
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

newtype MaybeProject = MP (Maybe Project)
  deriving (Typeable)

instance Version MaybeProject
$(deriveSerialize ''MaybeProject)


instance Component Database where
  type Dependencies Database = End
  initialValue = Database ToyRoster.roster (History []) Nothing


peekProject :: Query Database (MaybeProject)
peekProject = MP <$> curProject <$> ask

setProject :: Project -> Update Database ()
setProject p =
  do (Database students history _) <- get
     put $ Database students history (Just p)

$(mkMethods ''Database ['peekProject, 'setProject]) 

-- Use 'query PeekProject'
-- and 'update (SetProject Project { projectName = "first", invitations = [] })'



