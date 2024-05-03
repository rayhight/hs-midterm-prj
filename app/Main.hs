module Main where
import System.IO ()
import Data.List ()

data Customer = Customer {customerId :: Int, 
                            firstName :: String, 
                            lastName :: String, 
                            age :: Int, 
                            email :: String,
                            balance :: Float, 
                            riskNote :: String} 
                            deriving (Show, Read)
                            
type CustomerList = [Customer]

mainMenu :: IO ()
mainMenu = do
    putStrLn "\
    \ ________________________________\n\
    \|Choose any option:              |\n\
    \|    1.List all the Customers    |\n\
    \|<<<<<Customer Data Analysis>>>>>|\n\
    \|    2.Customer Avarage:         |\n\
    \|      2.1 Age                   |\n\
    \|      2.2 Balance               |\n\
    \|      2.3 Risk (Floar)          |\n\
    \|      2.4 Risk (Nominal/Enum)   |\n\
    \|      2.5 Display N cusomers    |\n\
    \|<<<<<<<<Risk Analysis>>>>>>>>>>>|\n\
    \|    3.List High Risk Customers  |\n\
    \|    4.Exit                      |\n\
    \|________________________________|"
    i <- getChar
    _ <- getChar                    -- ingone '\n' whitch for some reason are reading 
    chInputFirst i

chInputFirst :: Char -> IO()
chInputFirst inputChar
    | inputChar == 'q' = return ()
    | inputChar == '1' = putStrLn "First Option\n" >> mainMenu
    | inputChar == '2' = putStrLn "Second Option\n" >> subMenu
    | inputChar == '3' = putStrLn "Third Option\n" >> subMenu
    | otherwise        = putStrLn "Wrong Input\n" >> mainMenu

subMenu :: IO ()
subMenu = do
    putStrLn "\
    \ ________________________________ \n\
    \|                                |\n\
    \|    Customer Avarage:           |\n\
    \|      2.1 Age                   |\n\
    \|      2.2 Balance               |\n\
    \|      2.3 Risk (Floar)          |\n\
    \|      2.4 Risk (Nominal/Enum)   |\n\
    \|      2.5 Display N cusomers    |\n\
    \|________________________________|"
    i <-getChar
    _ <-getChar
    chInputSecond i

chInputSecond :: Char -> IO()
chInputSecond inputChar
    | inputChar == 'b' = mainMenu
    | inputChar == '1' = putStrLn "Second First Option\n" >> mainMenu
    | inputChar == '2' = putStrLn "Second Second Option\n" >> mainMenu
    | inputChar == '3' = putStrLn "Second Third Option\n" >> mainMenu
    | inputChar == '4' = putStrLn "Second Fourth Option\n" >> mainMenu
    | inputChar == '5' = putStrLn "Second Fifth Option\n" >> mainMenu
    | otherwise        = putStrLn "Wrong Input\n" >> subMenu

-- printCustomer :: Customer -> String
-- printCustomer Customer = let str 

strToCustomer :: String -> Customer
strToCustomer str =
    -- read ("Customer " ++ dropWhile (/= '{') str)
    read (dropWhile (/= '{') str)

main :: IO ()
main = do
    strLines <- readFile "CustomerData.txt"
    let customeList = map strToCustomer (lines strLines)
    print customeList
    -- mainMenu
