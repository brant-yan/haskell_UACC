module Domain.UserBindProjectInfo where

import Domain.Base    
    
data UserBindProjectInfo = UserBindProject {  ubp_userid :: UserId
                                             ,ubp_projectid :: ProjectId
                                            }deriving(Show) 