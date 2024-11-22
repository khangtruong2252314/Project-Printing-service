module Model(
    toRole,
    Role(..), 
    PrintData(..), 
    FileData(..), 
    PrinterData(..),
    UserData(..)) where

data Role = Student | SPSO | Guest deriving (Show, Eq)

toRole :: String -> Role
toRole "student" = Student
toRole "spso" = SPSO
toRole _ = Guest

data PrintData = PrintData {
    filepath :: String,
    printTime :: String,
    numCopies :: Int
} deriving (Show, Eq)

data FileData = FileData {
    file_name :: String,
    numPages :: Int
} deriving (Show, Eq)

data PrinterData = PrinterData {
    printer_name :: String,
    printer_location :: String,
    printer_available :: Bool,
    printer_activated :: Bool
} deriving (Show, Eq)

data UserData = UserData {
    account_username :: String,
    account_password :: String,
    account_role :: Role,
    account_balance :: Int
} deriving (Show, Eq)



