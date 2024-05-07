module Main where
import UserManagement
import DataAnalysis
import RiskManagement

mainMenu :: CustomerList -> IO ()
mainMenu c = do
    putStrLn "\
    \ ________________________________\n\
    \|Choose any option:              |\n\
    \|    1.List all the Customers    |\n\
    \|<<<<<Customer Data Analysis>>>>>|\n\
    \|    2.Customer Avarage:         |\n\
    \|      2.1 Age                   |\n\
    \|      2.2 Balance               |\n\
    \|      2.3 Risk                  |\n\
    \|      2.4 Risk Actual           |\n\
    \|      2.5 Display N cusomers    |\n\
    \|<<<<<<<<Risk Analysis>>>>>>>>>>>|\n\
    \|    3.List High Risk Customers  |\n\
    \|    4.Exit                      |\n\
    \|________________________________|"
    i <- getChar
    _ <- getChar
    chInputFirst i c

chInputFirst :: Char -> CustomerList -> IO()
chInputFirst inputChar c
    | inputChar == 'q' || inputChar == 'e' || inputChar == '4' = return ()
    | inputChar == '1' = putStrLn (conCustomerTable c) >> mainMenu c
    | inputChar == '2' = putStrLn ""  >> subMenu c
    | inputChar == '3' =
        putStrLn  ("\n <<<<<<<<<<<<<<<<<<<<<<<<<DISPLAYING CUSTOMERS WITH \"RISK VALUE\" = HIGH>>>>>>>>>>>>>>>>>>>>>>>>> \n"
                    ++ conCustomerTable (riskNoteFilter c "High")) >> mainMenu c
    | otherwise        = putStrLn "Wrong Input\n" >> mainMenu c

subMenu :: CustomerList -> IO ()
subMenu c = do
    putStrLn "\
    \ ________________________________ \n\
    \|                                |\n\
    \|    Customer Avarage:           |\n\
    \|      2.1 Age                   |\n\
    \|      2.2 Balance               |\n\
    \|      2.3 Risk (Float)          |\n\
    \|      2.4 Risk (Nominal/Enum)   |\n\
    \|      2.5 Display N cusomers    |\n\
    \|________________________________|"
    i <-getChar
    _ <-getChar
    chInputSecond i c

chInputSecond :: Char -> CustomerList -> IO()
chInputSecond inputChar c
    | inputChar == 'b' || inputChar == 'e' = mainMenu c
    | inputChar == '1' = print (retriveAvarageInt c)>> subMenu c
    | inputChar == '2' = print (retriveAvarageDouble c) >> subMenu c
    | inputChar == '3' = print (retriveAvarageRisk c ) >> subMenu c
    | inputChar == '4' = putStrLn (backToNominal $ retriveAvarageRisk c ) >> subMenu c
    | inputChar == '5' = do
        putStrLn "Enter the number of Customers?"
        i <- getLine 
        let n = read i::Int 
        if n < length c && n > 0 
            then do 
                putStrLn "Enter threshold: "
                t <- getLine
                let tn = read t::Double
                putStrLn (conCustomerTable $ sliceBackandSort (thresholdFilter c tn) n) >> subMenu c
            else putStrLn "Wrong Input" >> subMenu c
    | otherwise        = putStrLn "Wrong Input\n" >> subMenu c

main :: IO ()
main = do 
    customerList <- readDataToCList 
    mainMenu customerList
