module MemoryCache where
import Data.Map as Map
import Control.Concurrent
import Control.Monad
import System.IO
import Data.Map as Map   

        

toString::(Show a,Show b)=> Map a b -> IO [String]
toString m = do
    return (Prelude.map (\(k,v)->show k ++":"++ show v) (Map.toList m))


insertOne ::(Enum a, Ord a,Show b)=> MVar (Map a b) -> b -> IO ()
insertOne mv b = do
        m <- takeMVar mv
        putMVar mv (Map.insert (succ.maximum $ Map.keys m) b m)
        return ()