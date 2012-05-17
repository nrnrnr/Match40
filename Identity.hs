{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
module Identity
       ( UserIdent(..)
       )
where
  
import Data.SafeCopy
  
data UserIdent = UTLN String | CsUid String
$(deriveSafeCopy 0 'base ''UserIdent)  

instance Show UserIdent where
  show (UTLN s) = s
  show (CsUid s) = s
