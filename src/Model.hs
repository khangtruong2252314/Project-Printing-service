module Model(Role(..), toRole) where

data Role = Student | SPSO | Guest deriving (Show, Eq)

toRole :: String -> Role
toRole "student" = Student
toRole "spso" = SPSO
toRole _ = Guest


