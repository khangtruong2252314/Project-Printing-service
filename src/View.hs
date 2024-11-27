{-# LANGUAGE OverloadedStrings #-}

module View (
    homeView, 
    loginView, 
    menuView, 
    debugView, 
    managePrinterView, 
    paperManagementView,
    paperManagementSuccessView,
    fileManagementView,
    uploadFileView,
    printSuccessView,
    printFieldView,
    historyView,
    printerView,
    baffleView,
    printerInfoFormView,
    purchaseView) where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Text (pack)
import Control.Monad.Loops

import Model


-- Pages
debugView :: Show a => a -> H.Html
debugView item = H.html . H.text.pack.show $ item

homeView :: H.Html 
homeView = baseView.backgroundView $ do
    ribbonViewWithLogin
    contentSpacing $ do
        H.div H.! A.style "width: 752px; height: 274px; left: 0px; top: 225px; position: absolute" $ do 
            H.div H.! A.style "width: 752px; height: 274px; left: 0px; top: 0px; position: absolute; opacity: 0.48; background: black" $ ""
            H.div H.! A.style "left: 41px; top: 39px; position: absolute; color: white; font-size: 72px; font-family: Noto Sans Malayalam; font-weight: 700; word-wrap: break-word" $ do 
                "Student smart"
                H.br 
                "Printing Service"


loginView :: H.Html 
loginView = baseView $ do
    noBackgroundView $ do
        guestRibbonView
        contentSpacing $ do
            H.h1 "Login"
            H.form H.! A.method "POST" H.! A.action "/Auth" $ do
                H.div H.! A.class_ "form-group" $ do
                    H.label $ "Username"
                    H.input H.! A.type_ "text" H.! A.class_ "form-control" H.! A.name "username" H.! A.required "true"
                    H.label $ "Password"
                    H.input H.! A.type_ "password" H.! A.class_ "form-control" H.! A.name "password" H.! A.required "true"
                    H.button H.! A.class_ "btn btn-primary" $ "Submit"

menuView :: Role -> H.Html
menuView SPSO = baseView $ do
    backgroundView $ do
        ribbonView
        buttonBar ["Home", "Print", "History", "System"]


menuView Student = baseView $ do
    backgroundView $ do
        ribbonView
        buttonBar ["Home", "Print", "History", "Purchase"]

menuView Guest = baseView $ do
    backgroundView $ do
        ribbonView

managePrinterView :: [PrinterData] -> H.Html
managePrinterView printer_data = baseView $ do
    noBackgroundView $ do 
        buttonBar menu_list
        ribbonView
        contentSpacing $ do
            table $ printer_data
            H.button H.! A.class_ "btn btn-primary" H.! A.onclick "window.location.href = '/AddPrinter'" $ "Add new printer"
    where 
        menu_list = ["Home", "Print", "History", "System"]
        row item = H.tr $ do
            H.td $ do
                H.div $ do
                    H.a H.! A.href (("/Printer?printer=" <>).H.toValue.printer_name $ item) $ do
                        H.toHtml.printer_name $ item
            H.td $ do
                H.div $ do
                    H.toHtml.printer_location $ item
            H.td . H.div . H.toHtml.printer_activated $ item
        table items = H.table H.! A.class_ "table" $ do
            H.tbody $ do
                H.thead $ do
                    H.tr $ do
                        H.th H.! A.scope "col" $ "Printer name"
                        H.th H.! A.scope "col" $ "Location"
                        H.th H.! A.scope "col" $ "Activated"
                mapM_ row items

paperManagementView :: [(String, String)] -> H.Html
paperManagementView table = baseView $ do
    noBackgroundView $ do
        buttonBar ["Home", "Print", "History", "System"]
        ribbonView
        contentSpacing $ do
            obj_list
            H.div H.! A.style "left: 0px; width: 400px" $ do
                H.h4 "Monthly paper provision"
                H.form H.! A.method "POST" H.! A.action "/PaperManagement" $ do
                    H.div H.! A.class_ "form-group" $ do
                        H.label $ "Number of pages"
                        H.input H.! A.type_ "number" H.! A.class_ "form-control" H.! A.name "pages"
                    H.div H.! A.class_ "form-group" $ do
                        H.label $ "Next providing date"
                        H.input H.! A.type_ "date" H.! A.class_ "form-control" H.! A.name "date"
                    H.button H.! A.class_ "btn btn-primary" $ "Submit"
    
    where 
        obj_list = do
            H.div H.! A.style "right: 0px; position: absolute" $ do
                H.table H.! A.class_ "table" $ do
                    H.tr $ do
                        H.td $ "Paper refill date "
                        H.td $ H.toHtml $ refilled_date
                    H.tr $ do
                        H.td $ "Pages refilled "
                        H.td $ H.toHtml $ pages_refilled
        (pages_refilled, refilled_date) = case table of 
            [] -> ("", "")
            ((pages, date):_) -> (date, pages)

fileManagementView :: [FileData] -> H.Html
fileManagementView files = baseView $ do
    noBackgroundView $ do
        buttonBar ["Home", "Print", "History", "Purchase"]
        ribbonView
        contentSpacing $ do
            H.div $ do
                H.h1 "File management"
                H.table H.! A.class_ "table" $ do
                    H.thead $ do
                        H.tr $ do
                            H.th H.! A.scope "col" $ "File name"
                            H.th H.! A.scope "col" $ "Number of pages"
                    H.tbody $ do
                        concatM [\_ -> itemOption file | file <- files] ()
            H.div H.! A.style "right: 0px; position: absolute" $ do
                H.button H.! A.class_ "btn btn-primary" H.! A.onclick "window.location.href = '/UploadFile'" $ "Upload"
    where 
        itemOption file = do 
            H.tr $ do
                H.td $ do
                    H.a H.! A.href ("/PrintField/" <> (H.toValue.file_name $ file)) $ H.toHtml.pack.file_name $ file    
                H.td $ do
                    H.toHtml.show.numPages $ file

uploadFileView :: H.Html
uploadFileView = baseView $ do
    noBackgroundView $ do
        buttonBar ["Home", "Print", "History", "Purchase"]
        ribbonView
        contentSpacing $ do
            H.h1 "Upload file"
            H.form H.! A.method "POST" H.! A.action "/UploadFile" $ do
                H.div H.! A.class_ "form-group" $ do
                    H.label $ "File path"
                    H.input H.! A.type_ "text" H.! A.class_ "form-control" H.! A.name "filepath" H.! A.required "true"
                    H.label $ "Number of pages"
                    H.input H.! A.type_ "number" H.! A.class_ "form-control" H.! A.name "numPages" H.! A.required "true"
                    H.button H.! A.class_ "btn btn-primary" $ "Submit"
                
paperManagementSuccessView :: Int -> String -> H.Html
paperManagementSuccessView number datetime = baseView $ do
    noBackgroundView $ do
        buttonBar ["Home", "Print", "History", "System"]
        ribbonView
        contentSpacing $ do
            H.h1 "Success"
            H.h6 $ "Pages number: " <> (H.toHtml.pack.show $ number)
            H.h6 $ "Next providing date: " <> (H.toHtml.pack $ datetime)

printFieldView :: String -> Int -> UserData -> H.Html
printFieldView file pages user_data = baseView $ do
    noBackgroundView $ do
        buttonBar ["Home", "Print", "History", "Purchase"]
        ribbonView
        contentSpacing $ do
            H.h1 "Print files"
            H.h6 $ "File: " <> (H.toHtml.pack $ file)
            H.h6 $ "Account balance: " <> (H.toHtml.pack.show $ account_balance user_data)
            H.form H.! A.method "POST" H.! A.action ("/PrintingSuccess?path=" <> (H.toValue.pack $ file) <> "&&num_page=" <> (H.toValue $ pages)) $ do
                H.div H.! A.class_ "form-group" $ do
                    H.label $ "Number of copies"
                    H.input H.! A.type_ "number" H.! A.class_ "form-control" H.! A.name "copies"
                    H.button H.! A.class_ "btn btn-primary" $ "Submit"

printSuccessView :: H.Html
printSuccessView = baseView $ do
    noBackgroundView $ do
        buttonBar ["Home", "Print", "History", "Purchase"]
        ribbonView
        contentSpacing $ do
            H.h1 "Printing Success"

historyView :: [PrintData] -> H.Html
historyView print_list = baseView $ do
    noBackgroundView $ do
        buttonBar ["Home", "Print", "History", "Purchase"]
        ribbonView
        contentSpacing $ do
            H.h1 "History"
            H.table H.! A.class_ "table" $ do
                H.tr $ do
                    H.td $ "File"
                    H.td $ "Print time"
                    H.td $ "Copies"
                concatM [\_ -> itemDiv print_data | print_data <- print_list] ()
        
        where 
            itemDiv print_data = H.tr $ do
                H.td $ H.toHtml $ filepath print_data
                H.td $ H.toHtml $ printTime print_data
                H.td $ H.toHtml $ numCopies print_data

printerView :: PrinterData -> H.Html
printerView printer_data = baseView $ do
    noBackgroundView $ do
        buttonBar ["Home", "Print", "History", "System"]
        ribbonView
        contentSpacing $ do
            H.h1 "Printer"
            H.h6 $ "Name: " <> (H.toHtml.pack.printer_name $ printer_data)
            H.h6 $ "Activated: " <> (H.toHtml.pack.show.printer_activated $ printer_data)
            H.h6 $ "Location: " <> (H.toHtml.pack.printer_location $ printer_data)
            H.form H.! A.method "POST" H.! A.action ("/UpdatePrinter?printer_name=" <> (H.toValue.pack.printer_name $ printer_data)) $ do
                H.div H.! A.class_ "form-check" $ do
                    H.input H.! A.type_ "radio" H.! A.class_ "form-check-input" H.! A.name "activation" H.! A.value "Activate"
                    H.label H.! A.class_ "form-check-label" $ "Activation"
                H.div H.! A.class_ "form-check" $ do
                    H.input H.! A.type_ "radio" H.! A.class_ "form-check-input" H.! A.name "activation" H.! A.value "Deactivate"
                    H.label H.! A.class_ "form-check-label" $ "Deactivation"
                H.button H.! A.class_ "btn btn-primary" $ "Submit"

printerInfoFormView :: H.Html
printerInfoFormView = baseView $ do
    noBackgroundView $ do
        buttonBar ["Home", "Print", "History", "System"]
        ribbonView
        contentSpacing $ do
            H.h1 "Printer Info"
            H.form H.! A.method "POST" H.! A.action "/AddPrinter" $ do
                H.div H.! A.class_ "form-group" $ do
                    H.label $ "Printer name"
                    H.input H.! A.type_ "text" H.! A.class_ "form-control" H.! A.name "printer_name" H.! A.required "true"
                    H.label $ "Printer location"
                    H.input H.! A.type_ "text" H.! A.class_ "form-control" H.! A.name "printer_location" H.! A.required "true"
                    H.button H.! A.class_ "btn btn-primary" $ "Submit"

purchaseView :: H.Html
purchaseView = baseView $ do
    noBackgroundView $ do
        buttonBar ["Home", "Print", "History", "Purchase"]
        ribbonView
        contentSpacing $ do
            H.h1 "Purchase"
            H.form H.! A.method "POST" H.! A.action "/Purchase" $ do
                H.div H.! A.class_ "form-group" $ do
                    H.label $ "Amount"
                    H.input H.! A.type_ "number" H.! A.class_ "form-control" H.! A.name "amount" H.! A.required "true"
                    H.button H.! A.class_ "btn btn-primary" $ "Submit"

baffleView :: H.Html
baffleView = baseView $ do
    noBackgroundView $ do
        contentSpacing $ do
            H.h1 "Baffle, don't know where to go"

-- Utility objects
ribbonView :: H.Html
ribbonView = H.html $ do
    H.div H.! A.style "width: 1440px; height: 128px; left: 0px; top: 0px; position: absolute" $ do
        H.div H.! A.style "width: 1550px; height: 128px; left: 0px; top: 0px; position: absolute; background: #030391" $ ""
        H.div H.! A.style "width: 79px; height: 83px; left: 1249px; top: 23px; position: absolute" H.! A.onclick "window.location.href = '/logout'" $ do 
            H.img H.! A.style "width: 65.83px; height: 69.17px; left: 6.58px; top: 6.92px; position: absolute; background: white" H.! A.src "https://static.vecteezy.com/system/resources/thumbnails/046/357/631/small/grab-this-amazing-icon-of-power-button-shutdown-button-vector.jpg"
            H.button H.! A.onclick "window.location.href = '/logout'" H.! A.style "left: 100px; top: 24px; position: absolute; color: white; font-size: 24px; font-family: Inter; font-weight: 400; word-wrap: break-word; background: #1488D8;" $ "Logout"
        H.img H.! A.style "width: 82.26px; height: 96.28px; left: 26px; top: 10px; position: absolute" H.! A.src "https://lms.hcmut.edu.vn/pluginfile.php/3/theme_academi/logo/1725955904/logoBK.png"
        H.div H.! A.style "width: 895px; height: 77px; left: 128px; top: 29px; position: absolute; color: white; font-size: 39px; font-family: Roboto; font-weight: 700; line-height: 24px; letter-spacing: 0.50px; word-wrap: break-word" $ "Ho Chi Minh University of Technology"

ribbonViewWithLogin :: H.Html
ribbonViewWithLogin = H.html $ do
    H.div H.! A.style "width: 1440px; height: 128px; left: 0px; top: 0px; position: absolute" $ do
        H.div H.! A.style "width: 1550px; height: 128px; left: 0px; top: 0px; position: absolute; background: #030391" $ ""
        H.div H.! A.style "width: 79px; height: 83px; left: 1249px; top: 23px; position: absolute; background: #030391" H.! A.onclick "window.location.href = '/login'" $ do 
            H.a H.! A.href "/login" H.! A.style "left: 100px; top: 24px; position: absolute; color: white; font-size: 24px; font-family: Inter; font-weight: 400; word-wrap: break-word; background: #030391;" $ "Login"
        H.img H.! A.style "width: 82.26px; height: 96.28px; left: 26px; top: 10px; position: absolute" H.! A.src "https://lms.hcmut.edu.vn/pluginfile.php/3/theme_academi/logo/1725955904/logoBK.png"
        H.div H.! A.style "width: 895px; height: 77px; left: 128px; top: 29px; position: absolute; color: white; font-size: 39px; font-family: Roboto; font-weight: 700; line-height: 24px; letter-spacing: 0.50px; word-wrap: break-word" $ "Ho Chi Minh University of Technology"


guestRibbonView :: H.Html
guestRibbonView = H.html $ do
    H.div H.! A.style "width: 1440px; height: 128px; left: 0px; top: 0px; position: absolute" $ do
        H.div H.! A.style "width: 1550px; height: 128px; left: 0px; top: 0px; position: absolute; background: #030391" $ ""
        H.img H.! A.style "width: 82.26px; height: 96.28px; left: 26px; top: 10px; position: absolute" H.! A.src "https://lms.hcmut.edu.vn/pluginfile.php/3/theme_academi/logo/1725955904/logoBK.png"
        H.div H.! A.style "width: 895px; height: 77px; left: 128px; top: 29px; position: absolute; color: white; font-size: 39px; font-family: Roboto; font-weight: 700; line-height: 24px; letter-spacing: 0.50px; word-wrap: break-word" $ "Ho Chi Minh University of Technology"


backgroundView :: H.Html -> H.Html
backgroundView = do 
    elementView "width: 100%; height: 100%; position: relative; background-image: url('https://lms.hcmut.edu.vn/pluginfile.php/3/theme_academi/slide1image/1725955904/slbk.jpg'); background-repeat: no-repeat;"

noBackgroundView :: H.Html -> H.Html
noBackgroundView = do
    elementView "width: 100%; height: 100%; position: relative; background-color: white;"

elementView :: H.AttributeValue -> H.Html -> H.Html
elementView s = H.div H.! A.style s

buttonBar :: [String] -> H.Html
buttonBar x = do 
    H.div H.! A.style "width: 220px; height: 522px; left: 0px; top: 127px; position: absolute; background-color: #1488D8" $ do
        concatM [\ _ -> button_click row name | (row, name) <- zip [0..] x] ()

    where 
        button_click row name = do
            H.div H.! A.id (H.toValue name) H.! A.style ("width: 215px; height: 79px; left: 0px; top:" <> (H.toValue $ show ((row :: Int) * 80 + 130)) <> "px; position: absolute") $ do
                H.button H.! A.style "width: 215px; height: 79px; left: 0px; top: 0px; position: absolute; background: #030391; border: 1px #1488D8 solid" H.! A.onclick (H.toValue $ "window.location.href = '/" <> H.toValue name <> "'") $ do
                    H.div H.! A.style "width: 215px; left: 0px; top: 25px; position: absolute; text-align: center; color: white; font-size: 24px; font-family: Inter; font-weight: 400; word-wrap: break-word" $ H.toHtml name

baseView :: H.Html -> H.Html
baseView inner = do
    H.html $ do
        H.head $ do
            H.link H.! A.rel "stylesheet" H.! A.href "https://cdn.jsdelivr.net/npm/bootstrap@4.3.1/dist/css/bootstrap.min.css"
            H.script H.! A.src "https://code.jquery.com/jquery-3.3.1.slim.min.js" $ ""
        H.body $ do
            inner

contentSpacing :: H.Html -> H.Html
contentSpacing = H.div H.! A.style "left: 300px; top: 150px; width: 800px; position: absolute"
        

