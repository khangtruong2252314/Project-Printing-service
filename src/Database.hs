module Database(
    init_database, 
    init_print_database, 
    init_file_database,
    init_printer_database,
    init_user_database) where

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

init_file_database :: IO (IORef [FileData])
init_file_database = do
    file_database <- newIORef [FileData "rg.txt" 2, FileData "abc.txt" 5]
    return file_database

init_printer_database :: IO (IORef [PrinterData])
init_printer_database = do
    printer_database <- newIORef [PrinterData "Printer 1" "A1" True True, PrinterData "Printer 2" "H1" True True]
    return printer_database

init_user_database :: IO (IORef (M.Map String UserData))
init_user_database = newIORef. M.fromList $ [("demo_spso", UserData "demo_spso" "123" SPSO 0), ("demo_student", UserData "demo_student" "123" Student 15)]





