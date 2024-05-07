module DataAnalysis where

import Data.List (sortOn)
import UserManagement

riskNoteFilter :: CustomerList -> String -> CustomerList
riskNoteFilter (x:xc) s = [x | x<-xc, riskNote x == s ]

retriveAvarageInt :: CustomerList -> Int
retriveAvarageInt c  = sum (map age c) `div` cl
    where cl = length c

retriveAvarageDouble :: CustomerList -> Double
retriveAvarageDouble c  = sum (map balance c) / cl
    where cl = fromIntegral $ length c

retriveAvarageRisk :: CustomerList -> Double
retriveAvarageRisk cs =
    let rc = [if riskNote c == "High" then 3.0 else if riskNote c == "Low" then 1.0 else 2.0 | c <- cs]
        ra = sum rc / fromIntegral (length rc)
    in ra

backToNominal:: Double -> String
backToNominal d
    | d <= 1.4 && d > 0 = "Low"
    | d <= 2.4 && d >= 1.5 = "Medium"
    | d <= 3.0 && d >= 2.5 = "High"
    | otherwise = "Error"



sliceBackandSort:: CustomerList -> Int -> CustomerList
sliceBackandSort xs n = take n $ reverseCustomerList $ sortOn balance xs

reverseCustomerList :: CustomerList -> CustomerList
reverseCustomerList [] = []
reverseCustomerList (x:xs) = reverseCustomerList xs ++ [x]