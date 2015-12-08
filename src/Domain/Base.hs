module Domain.Base where
    
data ErrorMessage =   NoProject | MoreProjects 
                    | NoUserBingProject
                    | PasswordError | UserNotExist
                    | NoData
                    deriving Show

type UserName   = String
type Password   = String
type FirstName  = String
type SecondName = String
type UserId     = Integer


type ProjectId = Integer
type ProjectName = String
type ProjectNumber = String

