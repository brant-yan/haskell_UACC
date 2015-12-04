import Data.Map as Map
import Control.Concurrent
import Control.Monad
import System.IO
import MemoryCache
import Domain

consoleMessage::[String]
consoleMessage = [
        "0.exit",
        "1.Add User",
        "2.User Login",
        "3.User Logout",
        "4.Show All User"
        ]
showMessage :: [String] -> IO ()
--showMessage = mapM_ putStrLn
--showMessage = sequence_ . Prelude.map putStrLn
showMessage = putStrLn.unlines

mainloop :: UserState -> IO ()
mainloop us = do
    showMessage ["Please input 0,1,2,3,4"]
    cmd <- getLine
    case cmd of
       "0" -> do
                showMessage ["System Shutdown"]
                return ()
       _ -> do
            case cmd of 
                 "1" ->  inserUser us
                 "2" ->  showMessage ["Input User username and password"]
                 "3" ->  showMessage ["Input User username"]
                 "4" ->  case us of
                             (UserState uss) ->  takeMVar uss >>= toString >>= putStrLn.unlines
                             _ -> putStrLn "123"
                 otherwise -> showMessage ["Error Cmd"]
            mainloop us


main :: IO ()
main = do
    ius <- initUserInfo
    showMessage ["Welcome to Haskell sso"]
    showMessage consoleMessage
    mainloop ius
       
    
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
   