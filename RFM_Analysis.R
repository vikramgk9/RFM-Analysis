rm(list=ls())

installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}
needed <- c('didrooRFM', 'dplyr', 'lubridate')      
installIfAbsentAndLoad(needed)

data<-read.csv("eCommerce_data.csv",header = TRUE)
data <- na.omit(data, cols = 'CustomerID')
data$amount <- data$UnitPrice * data$Quantity
# Selecting relevant columns for RFM
# Under select statements mention the column names (For eg. CustomerID ID, Eevnt Date), Amount
rfm <- data %>% select(InvoiceNo,CustomerID,InvoiceDate,amount)
rfm$InvoiceDate <- mdy(rfm$InvoiceDate)
rfm$CustomerID <- as.character(rfm$CustomerID)

TransNo <- rfm$InvoiceNo
CustomerID <- rfm$CustomerID
DateofPurch <- rfm$InvoiceDate
Amount <- rfm$amount
customerData <- data.frame(TransNo,CustomerID,DateofPurch,Amount)
rfmoutput <- findRFM(customerData)
customerdata_aggr <- customerData %>% group_by(CustomerID) %>% summarize(Total_Amount = sum(Amount), Number_Transactions = n())
customer_final <- merge(customerdata_aggr, rfmoutput, by = "CustomerID")
#write.csv(customer_final,file = "customer_final.csv",row.names = FALSE)

####################################Generate Report for Power BI Analysis#################################
report <- customer_final %>% group_by(FinalCustomerClass) %>% summarize(Count = n(), Total_Sum = sum(Total_Amount)) %>% mutate(Cum_Perc_of_TotalSum = cumsum(Total_Sum/sum(Total_Sum)))
report$Cum_Perc_of_TotalSum <- round(report$Cum_Perc_of_TotalSum,4)
report$Perc_of_TotalSpend <- round(report$Total_Sum/sum(report$Total_Sum), 4)
report$Perc_of_Class <- round(report$Count/sum(report$Count), 4)
#write.csv(report, file = 'report.csv', row.names = F)