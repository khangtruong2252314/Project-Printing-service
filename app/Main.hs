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
    scotty 3000 $ do
        
        homeRoute  
        loginRoute
        menuRoute
        printRoute file_database 
        paperManagementRoute database
        authHomeRoute
        logoutRoute
        paperManagementFormRoute database 
        uploadFileFormRoute file_database
        uploadFileRoute
        printFieldRoute
        printingSuccessRoute print_database
        historyRoute print_database