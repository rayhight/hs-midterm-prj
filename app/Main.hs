module Main where
import System.IO ()

type CustomerId     = Int
type FirstName      = String
type LastName       = String
type Age            = Int
type Email          = String
type Balance        = Double
type RiskNote       = String
type Customer       = (CustomerId, FirstName, LastName, Age, Email, Balance, RiskNote)
type CustomerList   = [Customer]

stripStr :: String -> String
stripStr xs = [ x | x <- xs, x `notElem` "{}[],?!-:;\n\r\"" ]

stripStrList :: [String] -> [String]
stripStrList = map stripStr

readData :: FilePath -> IO [String]
readData filepath = do
    file <- readFile filepath
    let fileStrLines = lines file
    return fileStrLines

mainLoop :: IO ()
mainLoop = do
    putStrLn "\n\
    \ ________________________________\n\
    \|Choose any option:              |\n\
    \|    1.List all the Customers    |\n\
    \|<<<<<Customer Data Analysis>>>>>|\n\
    \|  Customer Avarage:             |\n\
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
    | inputChar == '1' = putStrLn "First Option\n" >> mainLoop
    | inputChar == '2' = putStrLn "Second Option\n" >> mainLoop
    | otherwise        = putStrLn "Wrong Input\n" >> mainLoop


main :: IO ()
main = do
    strLines <- readData "CustomerData.txt"
    -- mapM_ putStrLn (stripStrList strLines)
    mainLoop

