module DataAnalysis where

import Data.List (sortOn)
import UserManagement

retriveAvarageInt :: CustomerList -> String
retriveAvarageInt c =
    let g = sum (map age c) `div` cl
    in show g
    where cl = length c


retriveAvarageDouble :: CustomerList -> String
retriveAvarageDouble c = 
    let r = sum (map balance c) / cl
    in show r
    where cl = fromIntegral $ length c

retriveAvarageRisk :: CustomerList -> Double
retriveAvarageRisk cs =
    let rc = [if riskNote c ==  "High" then 3.0 else if riskNote c == "Low" then 1.0 else 2.0 | c <- cs]
        ra = sum rc / fromIntegral (length rc)
    in ra

backToNominal:: Double -> String
backToNominal d
    | d <= 1.4 && d > 0 = "Low"
    | d <= 2.4 && d >= 1.5 = "Medium"
    | d <= 3.0 && d >= 2.5 = "High"
    | otherwise = "Error"

sliceBackandSort:: CustomerList -> Int -> CustomerList
sliceBackandSort xs n = take n $ reqRevCL $ sortOn balance xs

reqRevCL :: [a] -> [a]
reqRevCL [] = []
reqRevCL (x:xs) = reqRevCL xs ++ [x]
