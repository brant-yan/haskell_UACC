module Domain where
    
    
import Data.Map as Map
import Control.Concurrent
import Control.Monad
import System.IO
import Data.Map as Map
import MemoryCache 
import Data.Maybe as May

import Domain.Base
import Domain.UserInfo  as MUserInfo
import Domain.ProjectInfo as MProjectInfo
import Domain.UserBindProjectInfo as MUserBindProjectInfo


            
                    

type UserMap    = Map UserId MUserInfo.UserInfo
type ProjectMap = Map ProjectId MProjectInfo.ProjectInfo




type BindId = Integer
type UserBindProjectMap = Map BindId MUserBindProjectInfo.UserBindProjectInfo

newtype UserState = UserState (MVar UserMap)
newtype ProjectState = ProjectState (MVar ProjectMap)
newtype UserBindProjectState = UserBindProjectState (MVar UserBindProjectMap)

newtype (Eq a,Ord b,Show c) => Transform a b c = Transform {runParse :: a -> [(a,b)]->Map b c->Either ErrorMessage (Map b c)}

toTrans ::(Eq a,Ord b,Show c)=>Transform a b c
toTrans = Transform(\uid ups ps -> 
                        case  Prelude.filter (\(_uid,pid)-> _uid==uid) ups of
                            [] -> Left NoUserBingProject
                            pids -> 
                                case Prelude.map (\pid-> Map.lookupLE pid ps) [pid | (_,pid) <- pids] of
                                        [] -> Left NoProject
                                        pinfos -> Right $ Map.fromList $ May.catMaybes pinfos
                                                                                                
                            
                   ) 
                                                                      
toTransUser :: TransUserId2ProjectInfo
toTransUser =  toTrans                                                                  
                                                                      
type TransUserId2ProjectInfo = Transform UserId ProjectId ProjectInfo

                                                        
initUserInfo :: IO UserState
initUserInfo = do
    m <- newMVar (Map.singleton 0 User{ui_username="",ui_password="",ui_firstname="",ui_secondname=""})
    return (UserState m)
    
initProjectInfo :: IO ProjectState
initProjectInfo = do
    m <- newMVar (Map.empty)
    return (ProjectState m)
    
initUserBindProjectInfo :: IO UserBindProjectState
initUserBindProjectInfo = do
    m <- newMVar (Map.empty)
    return (UserBindProjectState m)
    
                    
                                
filterMaps ::Map a b -> [(b->Bool,ErrorMessage)]->Either ErrorMessage [(a,b)]
filterMaps m [] = Right $ Map.toList m
filterMaps m ((f,msg):xs) = case Map.filter f m of
                                empty   -> Left msg
                                ms      -> filterMaps ms xs
                        
