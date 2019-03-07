##########################################################################################
#TS Modelling
TSTraindata<-read.csv("E:/Study data/R/Vidhya/Time Series Analysis/Train_SU63ISt.csv")
#There is not working Time format "IST" so took "EDT"
TSTraindata$Datetime <- strptime(TSTraindata$Datetime, format = "%d-%m-%Y %H:%M",tz='America/New_York') 
head(TSTraindata)
sum(is.na(TSTraindata))
TSTestdata<-read.csv("E:/Study data/R/Vidhya/Time Series Analysis/Test_0qrQsBZ.csv")
#-----------------------------------------------------------------------------------

tsdata <- apply(TSTraindata, 1, function(x){any(is.na(x))})
tstrain <- TSTraindata[!tsdata,]

#---------------------------------------------------------------------------------------
library(xts)
x.ts <- xts(tstrain$Count, order.by = as.Date(tstrain$Datetime))
ts <- ts(x.ts, freq = 168)
#------------------------------------------------------------------------------------
library(forecast)

#Final Model
fcdshw <- dshw(ts, period1 = 24, period2 = 168, lambda = TRUE, armethod = T, h = 5112)

plot(fcdshw)
summary(fcdshw)
#Fitted vs observed
Final_fore <- data.frame(ID = tstrain$ID, fitted = fcdshw$fitted, observed = tstrain$Count)
plot(Final_fore$fitted, Final_fore$observed)

# Final Submittion data set
sub <- data.frame(ID = TSTestdata$ID, Count = fcdshw$mean)
write.csv(sub, "ForeTS14.csv",row.names = F)
#-----------------------------------------------------------------------------------------
getwd()
