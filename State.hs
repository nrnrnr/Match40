{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, 
    MultiParamTypeClasses, TemplateHaskell, TypeFamilies #-}
module State
where
import Control.Applicative ((<$>))
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Happstack.State ( Component(..), End, Proxy(..), Query, Update, Version
                       , createCheckpoint, deriveSerialize, mkMethods, query
                       , startSystemState, shutdownSystem, update
                       )

import Invitation
import Types
import DB

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
  initialValue = Database [] (History []) Nothing

peekProject :: Query Database (Maybe Project)
peekProject = curProject <$> ask

setProject :: Project -> Update Database ()
setProject p =
  do (Database students history _) <- get
     put $ Database students history (Just p)

-- $(mkMethods ''Database ['peekProject, 'setProject]) 

-- Use 'query PeekProject'
-- and 'update (SetProject Project { projectName = "first", invitations = [] })'

