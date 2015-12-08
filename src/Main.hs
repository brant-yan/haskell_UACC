import Data.Map as Map
import Control.Concurrent
import Control.Monad
import System.IO
import MemoryCache
import Domain
import Business.General
import Domain.UserInfo
import Domain.ProjectInfo

consoleMessage::[String]
consoleMessage = [
        "0.exit",
        "1.Add User",
        "2.User Login",
        "3.User Logout",
        "4.Show All User"
        ]


mainloop :: UserState -> ProjectState -> UserBindProjectState -> IO ()
mainloop us@(UserState uss) ps@(ProjectState pss) u_p@(UserBindProjectState u_p_s) = do
    showMessage ["Please input 0,1,2,3,4"]
    cmd <- getLine
    case cmd of
       "0" -> do
                showMessage ["System Shutdown"]
                return ()
       _ -> do 
            case cmd of 
                 "1" ->  inserUser us
                 "2" ->  userLogin us ps u_p
                 "3" ->  showMessage ["Input User username"]
                 "4" ->  copyMVar uss >>= toString >>= putStrLn.unlines
                 otherwise -> showMessage ["Error Cmd"]
            mainloop us ps u_p
            
main :: IO ()
main = do
    ius <- initUserInfo
    pus <- initProjectInfo
    u_p <- initUserBindProjectInfo
    showMessage ["Welcome to Haskell sso"]
    showMessage consoleMessage
    mainloop ius pus u_p
       
        
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



testU = 1
testUPs = [(1,1)]
testP = Map.fromList [(1,Project "abc" "123")]  

