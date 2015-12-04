module Domain where
    
    
import Data.Map as Map
import Control.Concurrent
import Control.Monad
import System.IO
import Data.Map as Map 
    
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

type UserMap    = Map UserId UserInfo

newtype UserState = UserState (MVar UserMap)

initUserInfo :: IO UserState
initUserInfo = do
    m <- newMVar (Map.singleton 0 User{username="",password="",firstName="",secondName=""})
    return (UserState m)
