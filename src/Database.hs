module Database(init_database, init_print_database) where

import Data.IORef (IORef, newIORef, modifyIORef)
import qualified Data.Map as M


import qualified Config as C
import Model

init_database :: IO (IORef C.DatabaseType)
init_database = do
    database <- newIORef (mempty :: C.DatabaseType)
    modifyIORef database $ M.insert "Paper refill date" mempty
    modifyIORef database $ M.insert "File management" mempty
    return database

init_print_database :: IO (IORef [PrintData])
init_print_database = do
    print_database <- newIORef [PrintData "rg.txt" "11/08/2024" 3, PrintData "abc.txt" "30/11/2023" 4]
    return print_database



