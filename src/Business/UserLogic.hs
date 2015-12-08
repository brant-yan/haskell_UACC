module UserLogin where
import System.IO
import Domain 
import MemoryCache     
    
inserUser :: UserState -> IO ()
inserUser u@(UserState us)= do
    showMessage ["Input User Information"]
    userInfo <- getLine
    case words userInfo of
        [username,password,firstname,secondname] -> do
           let user= User username password firstname secondname
           insertOne us user
        otherwise -> error "error user information"
    return ()