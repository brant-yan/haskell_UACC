module Business.General where

import Control.Concurrent
import Control.Monad
import System.IO
import Data.Map as Map 
import Domain.Base
import MemoryCache 
import Domain.UserBindProjectInfo
import Domain.UserInfo
import Domain
    
    
 
userValidateConfs::UserName->Password->[(UserInfo->Bool,ErrorMessage)]
userValidateConfs name pwd = [  (\u -> name == ui_username u,UserNotExist)
                              , (\u -> pwd == ui_password u,PasswordError)
                              ]
    
userLogin  :: UserState -> ProjectState -> UserBindProjectState -> IO ()
userLogin (UserState us) (ProjectState ps) (UserBindProjectState ups)= do
    showMessage ["Input User username and password"]
    userInfo <- getLine
    case words userInfo of
        [name,pwd] -> do 
           umap <- copyMVar us
           case filterMaps umap $ userValidateConfs name pwd of
               Left s -> print s
               Right [(uid,uinfo)] -> do
                   projectMap <- copyMVar ps
                   userBindProjectMap <- copyMVar ups
                   case chechProjectByUserId uid (Prelude.map (\f-> (ubp_userid f,ubp_projectid f)) $ Map.elems userBindProjectMap) projectMap of 
                                        Left s -> putStrLn "Error"
                                        Right s -> putStrLn ("OK")
                
                   
    return ()
    
    
chechProjectByUserId :: UserId->[(UserId,ProjectId)]->ProjectMap  -> Either ErrorMessage Bool
chechProjectByUserId uid ups pmap = case runParse toTransUser uid ups pmap  of
                                        Left s -> Left s
                                        Right _ -> Right True