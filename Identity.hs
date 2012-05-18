{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
module Identity
       ( UserIdent(..), OtherIdent(..)
       )
where
  
import Data.SafeCopy
  
-- | A user's identity is determined by the unique Tufts email address stored in SIS
data UserIdent = SISEmail String
  deriving (Eq)
$(deriveSafeCopy 0 'base ''UserIdent)  

instance Show UserIdent where
  show (SISEmail s) = s

data OtherIdent = UTLN String | CsUid String
  -- ^ Other ways to identify a user
$(deriveSafeCopy 0 'base ''OtherIdent)  
 
instance Show OtherIdent where
  show (UTLN s) = s
  show (CsUid s) = s
