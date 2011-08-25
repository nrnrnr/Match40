module Auth
where
import Control.Monad
import Control.Monad.Trans

import Happstack.Auth
import Happstack.Server ( CookieLife(Session), addCookie, mkCookie
                       , readCookieValue
                       )
import Happstack.State  ( query )

import State

-- authorized :: Bool 
-- authorized = 
  -- do students <- query peekStudents 
     -- return students 

