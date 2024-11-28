{-# LANGUAGE OverloadedStrings #-}

module Lib(
    homeRoute, 
    loginRoute, 
    menuRoute, 
    printRoute, 
    paperManagementRoute,
    authHomeRoute,
    logoutRoute,
    paperManagementFormRoute,
    uploadFileFormRoute,
    uploadFileRoute,
    printFieldRoute,
    printingSuccessRoute,
    historyRoute,
    authHandlerRoute,
    defaultRoute,
    printerRoute,
    updatePrinterRoute,
    addPrinterRoute,
    purchaseRoute) where

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty
import Web.Scotty.Cookie (getCookie, deleteCookie)
import Web.Cookie (SetCookie(..), defaultSetCookie, renderSetCookie)
import Data.Text (unpack)
import Data.IORef (IORef, modifyIORef, readIORef)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS 
import qualified Data.ByteString.Builder as B
import qualified Data.Text.Lazy.Encoding as TLE

import Model
import View
import Config (DatabaseType)
import Default

defaultRoute :: ScottyM ()
defaultRoute = get "/" $ redirect "/home"

homeRoute :: ScottyM () 
homeRoute = get "/home" $ html.renderHtml $ homeView

loginRoute :: ScottyM ()
loginRoute = get "/login" $ html.renderHtml $ loginView

menuRoute :: ScottyM ()
menuRoute = get "/menu/:role" $ do
    role <- (captureParam "role" :: ActionM String)
    setForeverCookie "role" role
    html.renderHtml $ menuView.toRole $ role

managePrinterRoute :: IORef [PrinterData] -> ActionM ()
managePrinterRoute ref = do
    printers <- liftIO $ readIORef ref
    html.renderHtml $ managePrinterView printers

printingRoute :: IORef [FileData] -> ActionM ()
printingRoute ref = do
    filedata <- liftIO $ readIORef ref
    html.renderHtml $ fileManagementView filedata

printRoute :: IORef [PrinterData] -> IORef [FileData] -> ScottyM ()
printRoute ref_printer ref_file = get "/Print" $ do
    cookie <- getCookie "role"
    let role = case cookie of
                    Just txt -> toRole.unpack $ txt
                    Nothing -> Guest
    nextRoute role

    where 
        nextRoute SPSO = managePrinterRoute ref_printer
        nextRoute Student = printingRoute ref_file
        nextRoute Guest = redirect "/login"

uploadFileFormRoute :: IORef [FileData] -> ScottyM ()
uploadFileFormRoute ref = post "/UploadFile" $ uploadFileHandler ref

uploadFileRoute :: ScottyM ()
uploadFileRoute = get "/UploadFile" $ html.renderHtml $ uploadFileView

uploadFileHandler :: IORef [FileData] -> ActionM ()
uploadFileHandler ref = do
    path <- formParam "filepath"
    pages <- formParam "numPages"
    liftIO $ modifyIORef ref $ (FileData path (read pages :: Int):)
    redirect $ "/Print"


paperManagementRoute :: IORef DatabaseType -> ScottyM ()
paperManagementRoute ref = get "/System" $ do
    db <- liftIO $ readIORef ref
    html.renderHtml $ paperManagementView $ db M.! "Paper refill date"

authHomeRoute :: ScottyM ()
authHomeRoute = get "/Home" $ do
    cookie <- getCookie "role"
    case cookie of
        Just role -> html.renderHtml $ menuView.toRole.unpack $ role
        Nothing -> redirect "/login"

paperManagementFormRoute :: IORef DatabaseType -> ScottyM ()
paperManagementFormRoute database = post "/PaperManagement" $ do
    paperManagementFormHandler database


paperManagementFormHandler :: IORef DatabaseType -> ActionM ()
paperManagementFormHandler database = do
    pages <- formParam "pages"
    date <- formParam "date"
    liftIO $ writeDatabase database pages date
    html.renderHtml $ paperManagementSuccessView (read pages) date

    where 
        writeDatabase ref page date_ = do
            modifyIORef ref $ \db -> M.insert "Paper refill date" ([(date_, page)] ++ db M.! "Paper refill date") db
        
logoutRoute :: ScottyM ()
logoutRoute = get "/logout" $ do
    deleteCookie "role"
    redirect "/login"

printFieldRoute :: IORef [PrinterData] -> IORef (M.Map String UserData) -> IORef [FileData] -> ScottyM ()
printFieldRoute printerRef userRef fileRef = get "/PrintField/:path" $ do 
    path <- (captureParam "path" :: ActionM String)
    maybe_username <- getCookie "username"
    user_map <- liftIO $ readIORef userRef
    file_list <- liftIO $ readIORef fileRef
    printers <- liftIO $ readIORef printerRef
    let foundFiles = filter ((== path).file_name) file_list
    direct printers foundFiles maybe_username user_map
    
    where
        direct _ [] _ _ = redirect "/Print"
        direct _ _ Nothing _ = redirect "/Print"
        direct printers (target:_) (Just name) db       | M.member (unpack name) db = html.renderHtml $ printFieldView printers target (db M.! unpack name)
                                                        | otherwise = redirect "/Print"
    

printingSuccessRoute :: IORef (M.Map String UserData) -> IORef [PrintData] -> ScottyM ()
printingSuccessRoute user_ref history_ref = post "/PrintingSuccess" $ do
    copies <- (formParam "copies" :: ActionM Int)
    path <- (queryParam "path" :: ActionM String)
    num_page <- (read <$> queryParam "num_page" :: ActionM Int)
    maybe_username <- getCookie "username"
    now <- liftIO getCurrentTime
    logHistory now (show copies) path
    adjustBalance copies num_page maybe_username
    html.renderHtml $ printSuccessView

    where 
        logHistory now_time num_cp file_path = do
            liftIO $ modifyIORef history_ref $ \data_ -> data_ ++ [PrintData file_path (formatTime defaultTimeLocale "%d/%m/%Y" now_time) (read num_cp)]
        adjustBalance num_cp pg (Just username) = liftIO $ do
            matches_data <- (M.! (unpack username)) <$> readIORef user_ref 
            modifyIORef user_ref $ M.insert (account_username matches_data) matches_data{account_balance=account_balance matches_data - num_cp * pg}
        adjustBalance _ _ Nothing = liftIO.pure $ ()
            

historyRoute :: IORef [PrintData] -> ScottyM ()
historyRoute ref = get "/History" $ do
    data_ <- liftIO $ readIORef ref
    html.renderHtml $ historyView data_

authHandlerRoute :: IORef (M.Map String UserData) -> ScottyM ()
authHandlerRoute ref = post "/Auth" $ do
    username <- (formParam "username" :: ActionM String)
    password <- (formParam "password" :: ActionM String)
    database <- liftIO $ readIORef ref
    case M.lookup username database of 
        Just acc -> checkPassword acc password $ authSuccess acc
        Nothing -> redirect "/login"
    
    where
        checkPassword account password nextStep     | account_password account == password = nextStep
                                                    | otherwise = redirect "/login"
        authSuccess account = do
            setForeverCookie "role" (roleToString.account_role $ account)
            setForeverCookie "username" (account_username account)
            redirect "/Home"
        roleToString Student = "student"
        roleToString SPSO = "spso"
        roleToString Guest = "guest"

printerRoute :: IORef [PrinterData] -> ScottyM ()
printerRoute ref = get "/Printer" $ do 
    database <- liftIO $ readIORef ref
    parameter <- (queryParam "printer" :: ActionM String)
    let view = case filter (matched parameter) database of
                    acc: _ -> printerView acc
                    _ -> baffleView
    html.renderHtml $ view

    where 
        matched parameter data_point = printer_name data_point == parameter

updatePrinterRoute :: IORef [PrinterData] -> ScottyM ()
updatePrinterRoute ref = post "/UpdatePrinter" $ do
    activation <- (formParam "activation" :: ActionM String)
    target_name <- (queryParam "printer_name" :: ActionM String)
    update_printer_data target_name $ toActivation activation
    redirect "/Print"

    where
        toActivation "Activate" = True
        toActivation _ = False
        update_printer_data target_name activation = liftIO $ do
            modifyIORef ref $ map $ update_func target_name activation
        
        update_func target_name activation datagram     | printer_name datagram == target_name = datagram{printer_activated = activation}
                                                        | otherwise = datagram

addPrinterRoute :: IORef [PrinterData] -> ScottyM ()
addPrinterRoute ref = do 
    get "/AddPrinter" $ do
        html.renderHtml $ printerInfoFormView
    post "/AddPrinter" $ do
        target_printer_name <- (formParam "printer_name" :: ActionM String)
        target_printer_location <- (formParam "printer_location" :: ActionM String)
        liftIO $ modifyIORef ref $ update_func target_printer_name target_printer_location
        redirect "/Print"
    where
        update_func name location = (defaultPrinterData{printer_name=name, printer_location=location}:)


purchaseRoute :: IORef (M.Map String UserData) -> ScottyM ()
purchaseRoute ref = do 
    get "/Purchase" $ do
        html.renderHtml $ do
            purchaseView
    
    post "/Purchase" $ do
        amount <- (formParam "amount" :: ActionM Int)
        maybe_username <- getCookie "username"
        case maybe_username of
            Nothing -> redirect "/login"
            Just username -> do
                liftIO.modifyIORef ref $ adjustBalance amount $ unpack username
                redirect "/Print"
    
    where 
        adjustBalance amount username database = M.insert username (M.findWithDefault defaultUserData username database) {account_balance = account_balance (M.findWithDefault defaultUserData username database) + amount} database



-- Utility

setForeverCookie :: String -> String -> ActionM ()
setForeverCookie name value = do 
        addHeader "Set-Cookie" (TLE.decodeUtf8 . B.toLazyByteString $ renderSetCookie cookie)
        
    where 
        cookie = defaultSetCookie {
                        setCookieName = BS.pack name, 
                        setCookieValue = BS.pack value,
                        setCookieMaxAge = Just 36000000,
                        setCookiePath = Just "/"
                    }

