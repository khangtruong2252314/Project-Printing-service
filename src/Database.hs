module Database(init_database) where

import Data.IORef (IORef, newIORef, modifyIORef)
import qualified Data.Map as M


import qualified Config as C

init_database :: IO (IORef C.DatabaseType)
init_database = do
    database <- newIORef (mempty :: C.DatabaseType)
    modifyIORef database $ M.insert "Paper refill date" mempty
    return database


