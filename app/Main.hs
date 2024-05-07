module Main where
import UserManagement
import DataAnalysis
import RiskManagement

mainMenu :: CustomerList -> IO ()
mainMenu c = do
    putStrLn "\
    \ _______________________________________________________________________________________________\n\
    \|                  FinTech Customer \"Data Analysis\" Choose any option:                          |\n\
    \|                      1 - List all the Customers                                               |\n\
    \|------------------------------------CUSTOMERS DATA ANALYSIS------------------------------------|\n\
    \|                      2 - Customer Avarage:                                                    |\n\
    \|                          2.1 Age                                                              |\n\
    \|                          2.2 Balance                                                          |\n\
    \|                          2.3 Risk                                                             |\n\
    \|                          2.4 Risk Actual                                                      |\n\
    \|                          2.5 Display N cusomers                                               |\n\
    \|------------------------------------CUSTOMERS RISK ANALYSIS------------------------------------|\n\
    \|                      3 - List High Risk Customers                                             |\n\
    \|                      4 - Exit                                                                 |\n\
    \|_______________________________________________________________________________________________|"
    i <- getChar
    _ <- getChar
    chInputFirst i c

chInputFirst :: Char -> CustomerList -> IO()
chInputFirst inputChar c
    | inputChar == 'q' || inputChar == 'e' || inputChar == '4' = return ()
    | inputChar == '1' = putStrLn (conCustomerTable c) >> mainMenu c
    | inputChar == '2' = putStrLn ""  >> subMenu c
    | inputChar == '3' = do
        putStrLn "Enter threshold: "
        t <- getLine
        let tn = read t::Double
        putStrLn  ("\n<<<<<<<<<<<<<<<<<DISPLAYING CUSTOMERS WITH \"RISK VALUE\" = HIGH && Balance < " ++ t ++ ">>>>>>>>>>>>>>>>>\n"
                    ++ conCustomerTable (thresholdFilter (riskNoteFilter c "High") tn)) >> mainMenu c
    | otherwise = putStrLn "Wrong Input\n" >> mainMenu c

subMenu :: CustomerList -> IO ()
subMenu c = do
    putStrLn ("\
    \ _______________________________________________________________________________________________\n\
    \|                                                                                               |\n\
    \|       Customer Avarage:            == Number of Customres: "++ n ++"                                |\n\
    \|          2.1 Age                   == Display Arage Age of All Customers         - Input 1    |\n\
    \|          2.2 Balance               == Avarage Balance of All Customres           - Input 2    |\n\
    \|          2.3 Risk (Float)          == Avarage Risk of All Customres              - Input 3    |\n\
    \|          2.4 Risk (Nominal/Enum)   == Nominal Avarage Risk(\"High\"/\"Meium\"/\"Low\") - Input 4    |\n\
    \|          2.5 Display N Cusomers    == From highs to Lowes BALANCE                - Input 5    |\n\
    \|       To go back to the previous menu input (b/e/q/6)                                         |\n\
    \|_______________________________________________________________________________________________|")
    i <-getChar
    _ <-getChar
    chInputSecond i c
    where n = show $ length c

chInputSecond :: Char -> CustomerList -> IO()
chInputSecond inputChar c
    | inputChar == 'b' || inputChar == 'e' || inputChar == 'q' || inputChar == '6' = mainMenu c
    | inputChar == '1' = putStrLn ("The Avarage Age of All Customers is: " ++ retriveAvarageInt c) >> subMenu c
    | inputChar == '2' = putStrLn ("The Avarage Balance of All Customers is: " ++ retriveAvarageDouble c) >> subMenu c
    | inputChar == '3' = putStrLn ("The Avarage Risk of All Customers is: " ++ show (retriveAvarageRisk c)) >> subMenu c
    | inputChar == '4' = putStrLn ("The Avarage Risk of All Customers is: " ++ backToNominal (retriveAvarageRisk c)) >> subMenu c
    | inputChar == '5' = do
        putStrLn "Enter the number of Customers?"
        i <- getLine
        let n = read i::Int
        if n < length c && n > 0
            then putStrLn (conCustomerTable $ sliceBackandSort c n) >> subMenu c
            else putStrLn "Wrong Input" >> subMenu c
    | otherwise        = putStrLn "Wrong Input\n" >> subMenu c
    
main :: IO ()
main = do
    customerList <- readDataToCList
    mainMenu customerList
