import Data.Map as Map
import Control.Concurrent
import Control.Monad
import System.IO
import MemoryCache
import Domain
import Business.General

consoleMessage::[String]
consoleMessage = [
        "0.exit",
        "1.Add User",
        "2.User Login",
        "3.User Logout",
        "4.Show All User"
        ]


mainloop :: UserState -> ProjectState -> IO ()
mainloop us@(UserState uss) ps@(ProjectState pss) = do
    showMessage ["Please input 0,1,2,3,4"]
    cmd <- getLine
    case cmd of
       "0" -> do
                showMessage ["System Shutdown"]
                return ()
       _ -> do 
            case cmd of 
                 "1" ->  inserUser us
                 "2" ->  userLogin us
                 "3" ->  showMessage ["Input User username"]
                 "4" ->  copyMVar uss >>= toString >>= putStrLn.unlines
                 otherwise -> showMessage ["Error Cmd"]
            mainloop us ps
            

    


main :: IO ()
main = do
    ius <- initUserInfo
    pus <- initProjectInfo
    showMessage ["Welcome to Haskell sso"]
    showMessage consoleMessage
    mainloop ius pus
       
    
showUser :: UserState -> IO ()
showUser (UserState us) = do
    putStrLn "AllUser:"
    m <- takeMVar us
    print m
    return ()
    
inserUser :: UserState -> IO ()
inserUser u@(UserState us)= do
    showMessage ["Input User Information"]
    userInfo <- getLine
    case words userInfo of
        [username,password,firstname,secondname] -> do
           let user= User{username=username,password=password,firstName=firstname,secondName=secondname}
           insertOne us user
        otherwise -> error "error user information"
    return ()

chechProjectByUserId :: UserId->[(UserId,ProjectId)]->ProjectMap  -> Either ErrorMessage Bool
chechProjectByUserId uid ups pmap = case runParse toTransUser uid ups pmap  of
                                        Left s -> Left s
                                        Right _ -> Right True

testU = 1
testUPs = [(1,1)]
testP = Map.fromList [(1,Project "abc" "123")]  

