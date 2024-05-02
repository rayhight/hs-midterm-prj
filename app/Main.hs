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

main :: IO ()
main = do
    flines <- readData "CustomerData.txt"
    mapM_ putStrLn (stripStrList flines)

