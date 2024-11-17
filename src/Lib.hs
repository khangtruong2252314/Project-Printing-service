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
    historyRoute) where

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty
import Web.Scotty.Cookie (getCookie, deleteCookie)
import Web.Cookie (SetCookie(..), defaultSetCookie, renderSetCookie)
import Data.Text (unpack)
import Data.IORef (IORef, modifyIORef, readIORef)
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS 
import qualified Data.ByteString.Builder as B
import qualified Data.Text.Lazy.Encoding as TLE

import Model
import View
import Config (DatabaseType)

homeRoute :: ScottyM () 
homeRoute = get "/home" $ html.renderHtml $ homeView

loginRoute :: ScottyM ()
loginRoute = get "/login" $ html.renderHtml $ loginView

menuRoute :: ScottyM ()
menuRoute = get "/menu/:role" $ do
    role <- (captureParam "role" :: ActionM String)
    setForeverCookie "role" role
    html.renderHtml $ menuView.toRole $ role

managePrinterRoute :: ActionM ()
managePrinterRoute = html.renderHtml $ managePrinterView

printingRoute :: IORef DatabaseType -> ActionM ()
printingRoute ref = do
    db <- liftIO $ readIORef ref
    let table = db M.! "File management"
    let filenames = [filename | (filename, printed) <- table, printed /= "True"]
    html.renderHtml $ fileManagementView filenames

printRoute :: IORef DatabaseType -> ScottyM ()
printRoute ref = get "/Print" $ do
    cookie <- getCookie "role"
    let role = case cookie of
                    Just txt -> toRole.unpack $ txt
                    Nothing -> Guest
    nextRoute role

    where 
        nextRoute SPSO = managePrinterRoute
        nextRoute Student = printingRoute ref
        nextRoute Guest = redirect "/login"

uploadFileFormRoute :: IORef DatabaseType -> ScottyM ()
uploadFileFormRoute ref = post "/UploadFile" $ uploadFileHandler ref

uploadFileRoute :: ScottyM ()
uploadFileRoute = get "/UploadFile" $ html.renderHtml $ uploadFileView

uploadFileHandler :: IORef DatabaseType -> ActionM ()
uploadFileHandler ref = do
    db <- liftIO $ readIORef ref
    path <- formParam "filepath"
    liftIO $ modifyIORef ref $ M.insert "File management" $ (path, "False") : db M.! "File management"
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

printFieldRoute :: ScottyM ()
printFieldRoute = get "/PrintField/:path" $ do 
    path <- (captureParam "path" :: ActionM String)
    setForeverCookie "filepath" path
    html.renderHtml $ printFieldView path

printingSuccessRoute :: IORef [PrintData] -> ScottyM ()
printingSuccessRoute ref = post "/PrintingSuccess" $ do
    copies <- (formParam "copies" :: ActionM String)
    fileCookie <- getCookie "filepath"
    case fileCookie of
        Just cookie -> do
            liftIO $ modifyIORef ref $ \data_ -> data_ ++ [PrintData (unpack cookie) "Unknown" (read copies)]
        Nothing -> redirect "/login"
    html.renderHtml $ printSuccessView

historyRoute :: IORef [PrintData] -> ScottyM ()
historyRoute ref = get "/History" $ do
    data_ <- liftIO $ readIORef ref
    html.renderHtml $ historyView data_

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

