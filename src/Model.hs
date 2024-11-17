module Model(Role(..), toRole, PrintData(..)) where

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


