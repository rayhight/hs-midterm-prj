module RiskManagement where
import UserManagement

riskNoteFilter :: CustomerList -> String -> CustomerList
riskNoteFilter (x:xc) s = [x | x<-xc, riskNote x == s]

thresholdFilter :: CustomerList -> Double -> CustomerList
thresholdFilter (x:xc) d = [x | x<-xc, balance x < d]
