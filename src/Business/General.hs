module Business.General where

import Control.Concurrent
import Control.Monad
import System.IO
import Data.Map as Map 
import Domain 
import MemoryCache   
    
    
userLogin  :: UserState -> IO ()
userLogin (UserState us)= do
    showMessage ["Input User username and password"]
    userInfo <- getLine
    case words userInfo of
        [name,pwd] -> do 
           umap <- copyMVar us
           case validateNameAndPwd umap (\u -> name == username u) (\u -> pwd== password u) of
               Left s -> putStrLn s
               Right i -> putStrLn ("find user\t" ++ show i)
    return ()