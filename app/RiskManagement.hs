module RiskManagement where
import UserManagement

thresholdFilter::CustomerList->Double->CustomerList
thresholdFilter (x:xc) d = [x | x<-xc, balance x < d]