module Model(
    toRole,
    Role(..), 
    PrintData(..), 
    FileData(..), 
    PrinterData(..)) where

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
    available :: Bool,
    activated :: Bool
}


