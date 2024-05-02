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
    putStrLn "Enter [ 1 | 2 | q ]: "
    i <- getChar
    _ <- getChar                    -- ingone '\n' whitch for some reason are reading 
    chInputFirst i
chInputFirst :: Char -> IO()
chInputFirst i 
    | i == 'q' = return ()
    | i == '1' = putStrLn "First Option\n" >> mainLoop
    | i == '2' = putStrLn "Second Option\n" >> mainLoop
    | otherwise = putStrLn "Wrong Input\n" >> mainLoop


main :: IO ()
main = do
    strLines <- readData "CustomerData.txt"
    -- mapM_ putStrLn (stripStrList strLines)
    mainLoop

