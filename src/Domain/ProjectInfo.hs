module Domain.ProjectInfo where
    
import Domain.Base
    
data ProjectInfo = Project { pi_name::ProjectName
                            ,pi_number ::ProjectNumber
                            }deriving(Show)