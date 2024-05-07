module UserManagement where

data Customer = Customer {customerId :: Int, firstName :: String, lastName :: String, age :: Int,email :: String,balance :: Double, riskNote :: String} deriving (Show, Read)

type CustomerList = [Customer]

formatId :: Int -> String
formatId n
    | n < 10    = "00" ++ show n
    | n < 100   = "0" ++ show n
    |otherwise  = show n

formatStr :: String -> Int -> String
formatStr s n
    | ogl < fdl = s ++ replicate (fdl - ogl) ' '
    | otherwise = s
    where
        ogl = length s
        fdl = n

conCustomerList :: Customer -> String
conCustomerList (Customer id fn ln ag em ba rs) =
    "| "++
    formatId id ++ " | " ++
    formatStr (fn ++ " " ++ ln) (length  "Customer Full Name ") ++ " | " ++
    show ag ++ "  | " ++
    formatStr em 26 ++ " | " ++
    formatStr (show ba) (length "Customer Balance ") ++ " | " ++
    formatStr rs (length "Risk Note ") ++ " |"

conCustomerTable :: CustomerList -> String
conCustomerTable x = "\
    \ _______________________________________________________________________________________________\n\
    \| IDs | Customer Full Name  | Age | Customer Email Address     | Customer Balance  | Risk Note  |\n\
    \|-----|---------------------|-----|----------------------------|-------------------|------------|\n"
    ++ unlines str  ++
    "|_______________________________________________________________________________________________|"
    where str = map conCustomerList x

readDataToCList::IO CustomerList
readDataToCList = do 
    str <- readFile "CustomerData.txt"
    let customersList = read str :: [Customer]
    return customersList