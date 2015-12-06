module Domain where
    
    
import Data.Map as Map
import Control.Concurrent
import Control.Monad
import System.IO
import Data.Map as Map
import MemoryCache 
import Data.Maybe as May

data ErrorMessage =   NoProject | MoreProjects 
                    | NoUserBingProject
                    deriving Show
    
type UserName   = String
type Password   = String
type FirstName  = String
type SecondName = String
type UserId     = Integer


data UserInfo = User {username::UserName
                    , password::Password
                    , firstName :: FirstName
                    , secondName :: SecondName
                    }deriving(Show)
type ProjectId = Integer
type ProjectName = String
type ProjectNumber = String




data ProjectInfo = Project { projectname::ProjectName
                            ,projectnumber ::ProjectNumber
                            }deriving(Show)
                                           
                    
                    

type UserMap    = Map UserId UserInfo
type ProjectMap = Map ProjectId ProjectInfo


data UserBindProjectInfo = UserBindProject {  userid :: UserId
                                            , projectid :: ProjectId
                                           }deriving(Show) 

type BindId = Integer
type UserBindProjectMap = Map BindId UserBindProjectInfo

newtype UserState = UserState (MVar UserMap)
newtype ProjectState = ProjectState (MVar ProjectMap)
newtype UserBindProjectState = UserBindProjectState (MVar UserBindProjectMap)

newtype (Eq a,Ord b,Show c) => Transform a b c = Transform {runParse :: a -> [(a,b)]->Map b c->Either ErrorMessage (Map b c)}

toTrans ::(Eq a,Ord b,Show c)=>Transform a b c
toTrans = Transform(\uid ups ps -> case  Prelude.filter (\(_uid,pid)-> _uid==uid) ups of
                                    [] -> Left NoUserBingProject
                                    pids -> case Prelude.map (\pid-> Map.lookupLE pid ps) [pid | (_,pid) <- pids] of
                                                [] -> Left NoProject
                                                pinfos -> Right $ Map.fromList $ May.catMaybes pinfos
                                                                                                
                            
                   ) 
                                                                      
toTransUser :: TransUserId2ProjectInfo
toTransUser =  toTrans                                                                  
                                                                      
type TransUserId2ProjectInfo = Transform UserId ProjectId ProjectInfo

--transUserId2ProjectInfo uid ups ps = identity uid ups ps


                                                        
initUserInfo :: IO UserState
initUserInfo = do
    m <- newMVar (Map.singleton 0 User{username="",password="",firstName="",secondName=""})
    return (UserState m)
    
initProjectInfo :: IO ProjectState
initProjectInfo = do
    m <- newMVar (Map.empty)
    return (ProjectState m)
    
initUserBindProjectInfo :: IO UserBindProjectState
initUserBindProjectInfo = do
    m <- newMVar (Map.empty)
    return (UserBindProjectState m)

filterMap :: Map a b -> (b -> Bool) -> Either String (a,b)
filterMap m f = case Map.toList $ Map.filter f m of
                            [] -> Left "cant't find"
                            [(userid,userinfo)] -> Right (userid,userinfo)
                            _ -> Left "more infornamtion"
                            
validateNameAndPwd :: Map a b -> (b -> Bool) -> (b -> Bool) -> Either String (a,b)
validateNameAndPwd m f1 f2 = do
                            (userid,userinfo) <- filterMap m f1
                            case f2 userinfo of
                                True -> Right (userid,userinfo)
                                False -> Left "user password error"
