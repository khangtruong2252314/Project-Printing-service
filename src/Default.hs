module Default(
    defaultPrintData,
    defaultFileData,
    defaultPrinterData,
    defaultUserData
) where

import Model

defaultPrintData :: PrintData
defaultFileData :: FileData
defaultPrinterData :: PrinterData
defaultUserData :: UserData


defaultPrintData = PrintData "rg.txt" "11/08/2024" 3
defaultFileData = FileData "rg.txt" 2
defaultPrinterData = PrinterData "Printer 1" "A1" True True
defaultUserData = UserData "demo_spso" "123" SPSO 0

