{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Web.Scotty

import Lib 
import qualified Database as DB

main :: IO ()
main = do
    database <- DB.init_database
    print_database <- DB.init_print_database
    scotty 3000 $ do
        
        homeRoute  
        loginRoute
        menuRoute
        printRoute database
        paperManagementRoute database
        authHomeRoute
        logoutRoute
        paperManagementFormRoute database 
        uploadFileFormRoute database
        uploadFileRoute
        printFieldRoute
        printingSuccessRoute print_database
        historyRoute print_database