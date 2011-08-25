module ToyRoster
  ( roster
  )
where
import Types

roster :: [Student]

roster = [ Student { name = fullName "Noah Daniels"
                   , email = "ndaniels@mac.com"
                   , aboutMe = "Driving instructor"
                   , photo = Nothing
                   , password = ""
                   , status = Enrolled
                   }
         , Student { name = fullName "Norman Ramsey"
                   , email = "nr@cs.tufts.edu"
                   , aboutMe = "Woodshop instructor"
                   , photo = Nothing
                   , password = ""
                   , status = Enrolled
                   }
         , Student { name = fullName "Andrew Gallant"
                   , email = "agallant@cs.tufts.edu"
                   , aboutMe = "Grilling instructor"
                   , photo = Nothing
                   , password = ""
                   , status = Enrolled
                   }
         ]
