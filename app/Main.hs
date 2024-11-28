{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Web.Scotty

import Lib 
import qualified Database as DB

main :: IO ()
main = do
    database <- DB.init_database
    print_database <- DB.init_print_database
    file_database <- DB.init_file_database
    printer_database <- DB.init_printer_database
    user_database <- DB.init_user_database

    scotty 3000 $ do
        defaultRoute    
        homeRoute  
        loginRoute
        menuRoute
        printRoute printer_database file_database 
        paperManagementRoute database
        authHomeRoute
        logoutRoute
        paperManagementFormRoute database 
        uploadFileFormRoute file_database
        uploadFileRoute
        printFieldRoute printer_database user_database file_database
        printingSuccessRoute user_database print_database
        historyRoute print_database
        authHandlerRoute user_database
        printerRoute printer_database
        updatePrinterRoute printer_database
        addPrinterRoute printer_database
        purchaseRoute user_database