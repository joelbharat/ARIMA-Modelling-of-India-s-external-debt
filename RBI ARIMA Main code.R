library(tseries)
library(magrittr)
library(dplyr)
library(forecast)
library(urca)
library(ggfortify)
library(data.table)
library(fUnitRoots)

# Read the RBI dataset
df <- read.csv("C:/MSc (BDA)/01 MVS Project/1 project/Abridged_RBI_dataset.csv")

# key_features <- select(df, c(I_Total_Multilateral, II_Total_Bilateral, III_International_Monetary_Fund, 
#                              IV_Total_Trade_Credit, V_Total_Commercial_Borrowing, VI_NRI_FC_Deposits,
#                              VII_Total_Rupee_Debt, VIII_Total_Long_Term_Debt, IX_Total_Short_term_Debt,
#                              Gross_Total_Debt, Debt_Stock_GDP_Ratio,
#                              Debt_Service_Ratio))

########################################################################################################
# I_Total_Multilateral

#EXTRACT TEST COLUMN  
multilat<-pull(df, I_Total_Multilateral)

#convert test column into time series datatype AND CARRY OUT adf test
multilat_ts<-ts(multilat, start=1991)
adf.test(multilat_ts)
multilat_train <- window(multilat_ts, start=1991, end=2014)
adf.test(multilat_train)
multilat_test <- window(multilat_ts, start=2015, end=2020)

#Plot the column
# autoplot(multilat_ts) +
#   ggtitle("TOTAL MULTILATERAL DEBT OF RBI") +
#   xlab("Year") +
#   ylab("in Rupees Crore")

#calculate the number of differencings that need to be applied and apply it
multilat_diffed<-diff(multilat_train, differences = 2)

#carry out ADF test once again to confirm Stationarity
adf.test(multilat_diffed) 

autoplot(multilat_diffed) +
  ggtitle("TOTAL MULTILATERAL DEBT OF RBI") +
  xlab("Year") +
  ylab("in Rupees Crore")
acf(multilat_diffed, lag.max=30)
pacf(multilat_diffed, lag.max=30)

#ARIMA  Modelling
mult_arima <- auto.arima(multilat_train, seasonal=FALSE, stepwise=FALSE, approximation=FALSE )
mult_arima
#mult_arima <- Arima(multilat_train, c(0,1,0))
tsdisplay(residuals(mult_arima), lag.max=45, main='(0,1,0) Model Residuals') 
fcast_mult <- forecast(mult_arima, h=6)

#Accuracy check
plot(fcast_mult, type="b", lwd=5, col='blue', main="TOTAL MULTILATERAL DEBT OF RBI", ylab="Rupees in Crores")
lines(multilat_test, col="red", lwd=3)
legend("topleft",lty=1,bty = "n",col=c("red", "blue"),c("Actual","Predicted"))

fcast<-forecast(multilat_ts, h=30)
plot(fcast, type="b", lwd=5, col='blue', main="FORECAST OF TOTAL MULTILATERAL DEBT OF RBI ", ylab="Rupees in Crores")
legend("topleft",lty=1,bty = "n",col=c("blue"),c("Forecast"))
#####################################################################################################
# II_Total_Bilateral

#EXTRACT TEST COLUMN  
bilat<-pull(df, II_Total_Bilateral)

#convert test column into time series datatype AND CARRY OUT adf test
bilat_ts<-ts(bilat, start=1991)
adf.test(bilat_ts)
bilat_train <- window(bilat_ts, start=1991, end=2014)
adf.test(bilat_ts)
bilat_test <- window(bilat_ts, start=2015, end=2020)

#Plot the column
# autoplot(bilat_ts) +
#   ggtitle("Total Bilateral Debt of RBI") +
#   xlab("YEAR") +
#   ylab("RUPEES (CRORES")

#calculate the number of differencings that need to be applied and apply it
bilat_diffed<-diff(bilat_train, differences = 3)

#carry out ADF test once again to confirm Stationarity
adf.test(bilat_diffed) 

autoplot(bilat_diffed) +
  ggtitle("TOTAL BILATERAL DEBT OF RBI") +
  xlab("YEAR") +
  ylab("RUPEES (CRORES")
acf(bilat_diffed, lag.max=30)
pacf(bilat_diffed, lag.max=30)

#ARIMA  Modelling
bilat_arima <- auto.arima(bilat_train, seasonal=FALSE, stepwise=FALSE, approximation=FALSE )
bilat_arima
bilat_arima<-Arima(bilat_train,c(1,2,0)) # better model
tsdisplay(residuals(bilat_arima), lag.max=45, main='(1,2,0) Model Residuals') 
fcast_bilat <- forecast(bilat_arima, h=6)

#Accuracy check
plot(fcast_bilat, type="b", lwd=5, col='blue', main="TOTAL BILATERAL DEBT OF RBI", ylab="RUPEES (CRORES)")
lines(bilat_test, col="red", lwd=3)
legend("topleft",lty=1,bty = "n",col=c("red", "blue"),c("Actual","Predicted"))

fcast<-forecast(bilat_ts, h=30)
plot(fcast, type="b", lwd=5, col='blue', main="FORECAST OF TOTAL BILATERAL DEBT OF RBI", ylab="RUPEES (CRORES)")
legend("topleft",lty=1,bty = "n",col=c("blue"),c("Forecast"))
####################################################################################################
# III_International_Monetary_Fund

#EXTRACT TEST COLUMN  
imf<-pull(df, III_International_Monetary_Fund)

#convert test column into time series datatype AND CARRY OUT adf test
imf_ts<-ts(imf, start=1991)
adf.test(imf_ts)
imf_train <- window(imf_ts, start=1991, end=2014)
adf.test(imf_train)
imf_test <- window(imf_ts, start=2015, end=2020)

#Plot the column
# autoplot(imf_ts) +
#   ggtitle("IMF DEBT OF RBI") +
#   xlab("Year") +
#   ylab("in Rupees Crore")

#calculate the number of differencings that need to be applied and apply it
imf_diffed<-diff(imf_train, differences = 2)

#carry out ADF test once again to confirm Stationarity
adf.test(imf_diffed) 

autoplot(imf_diffed) +
  ggtitle("IMF DEBT OF RBI") +
  xlab("YEAR") +
  ylab("RUPEES (CRORE)")
acf(imf_diffed, lag.max=30)
pacf(imf_diffed, lag.max=30)

#ARIMA  Modelling
imf_arima <- auto.arima(imf_train, seasonal=FALSE, stepwise=FALSE, approximation=FALSE )
imf_arima
tsdisplay(residuals(imf_arima), lag.max=45, main='(0,1,0) Model Residuals') 
fcast_mult <- forecast(imf_arima, h=6)

#Accuracy check
plot(fcast_mult, type="b", lwd=5, col='blue', main="IMF DEBT OF RBI", ylab="Rupees in Crores")
lines(imf_test, col="red", lwd=3)
legend("topleft",lty=1,bty = "n",col=c("red", "blue"),c("Actual","Predicted"))

fcast<-forecast(imf_ts, h=5)
plot(fcast, type="b", lwd=5, col='blue', main="FORECAST OF IMF DEBT OF RBI", ylab="Rupees in Crores")
legend("topleft",lty=1,bty = "n",col=c("blue"),c("Forecast"))
####################################################################################################
# IV_Total_Trade_Credit

#EXTRACT TEST COLUMN  
ttc<-pull(df, IV_Total_Trade_Credit)

#convert test column into time series datatype AND CARRY OUT adf test
ttc_ts<-ts(ttc, start=1991)
adf.test(ttc_ts)
ttc_train <- window(ttc_ts, start=1991, end=2014)
adf.test(ttc_train)
ttc_test <- window(ttc_ts, start=2015, end=2020)

#Plot the column
# autoplot(ttc_ts) +
#   ggtitle("TOTAL TRADE CREDIT OF RBI") +
#   xlab("Year") +
#   ylab("in Rupees Crore")

#calculate the number of differencings that need to be applied and apply it
ttc_diffed<-diff(ttc_train, differences = 3)

#carry out ADF test once again to confirm Stationarity
adf.test(ttc_diffed) 

autoplot(ttc_diffed) +
  ggtitle("TOTAL TRADE CREDIT OF  RBI") +
  xlab("Year") +
  ylab("in Rupees Crore")
acf(ttc_diffed, lag.max=30)
pacf(ttc_diffed, lag.max=30)

#ARIMA  Modelling
ttc_arima <- auto.arima(ttc_train, seasonal=FALSE, stepwise=FALSE, approximation=FALSE )
ttc_arima
ttc_arima <- Arima(ttc_train, c(0,2,0))
tsdisplay(residuals(ttc_arima), lag.max=45, main='(0,2,0) Model Residuals') 
fcast_ttc <- forecast(ttc_arima, h=6)

#Accuracy check
plot(fcast_ttc, type="b", lwd=5, col='blue', main="TOTAL TRADE CREDIT OF RBI", ylab="Rupees in Crores")
lines(ttc_test, col="red", lwd=3)
legend("topleft",lty=1,bty = "n",col=c("red", "blue"),c("Actual","Predicted"))

fcast<-forecast(ttc_ts, h=30)
plot(fcast, type="b", lwd=5, col='blue', main="FORECAST OF TOTAL TRADE CREDIT OF RBI", ylab="Rupees in Crores")
legend("topleft",lty=1,bty = "n",col=c("blue"),c("Forecast"))
####################################################################################################
# V_Total_Commercial_Borrowing

#EXTRACT TEST COLUMN  
tcb<-pull(df, V_Total_Commercial_Borrowing)

#convert test column into time series datatype AND CARRY OUT adf test
tcb_ts<-ts(tcb, start=1991)
adf.test(tcb_ts)
tcb_train <- window(tcb_ts, start=1991, end=2014)
adf.test(tcb_train)
tcb_test <- window(tcb_ts, start=2015, end=2020)

#Plot the column
# autoplot(tcb_ts) +
#   ggtitle("TOTAL COMMERCIAL BORROWINGS OF RBI") +
#   xlab("Year") +
#   ylab("in Rupees Crore")

#calculate the number of differencings that need to be applied and apply it
tcb_diffed<-diff(tcb_train, differences = 2)

#carry out ADF test once again to confirm Stationarity
adf.test(tcb_diffed) 

autoplot(tcb_diffed) +
  ggtitle("TOTAL COMMECIAL BORROWINGS OF RBI") +
  xlab("YEAR") +
  ylab("RUPEES (CRORES)")
acf(tcb_diffed, lag.max=30)
pacf(tcb_diffed, lag.max=30)

#ARIMA  Modelling
tcb_arima <- auto.arima(tcb_train, seasonal=FALSE, stepwise=FALSE, approximation=FALSE )
tcb_arima
tsdisplay(residuals(tcb_arima), lag.max=45, main='(0,2,0) Model Residuals') 
fcast_tcb <- forecast(tcb_arima, h=6)

#Accuracy check
plot(fcast_tcb, type="b", lwd=5, col='blue', main="TOTAL COMMERCIAL BORROWINGS OF RBI", ylab="Rupees in Crores")
lines(tcb_test, col="red",lwd=3)
legend("topleft",lty=1,bty = "n",col=c("red", "blue"),c("Actual","Predicted"))

fcast<-forecast(tcb_ts, h=30)
plot(fcast, type="b", lwd=5, col='blue', main="FORECAST OF TOTAL COMMERCIAL BORROWINGS OF RBI", ylab="Rupees in Crores")
legend("topleft",lty=1,bty = "n",col=c("blue"),c("Forecast"))
####################################################################################################
# VI_NRI_FC_Deposits

#EXTRACT TEST COLUMN  
nri<-pull(df, VI_NRI_FC_Deposits)

#convert test column into time series datatype AND CARRY OUT adf test
nri_ts<-ts(nri, start=1991)
adf.test(nri_ts)
nri_train <- window(nri_ts, start=1991, end=2014)
adf.test(nri_train)
nri_test <- window(nri_ts, start=2015, end=2020)

#Plot the column
# autoplot(nri_ts) +
#   ggtitle("NRI FOREIGN CURRENCY DEPOSITS INTO RBI") +
#   xlab("Year") +
#   ylab("in Rupees Crore")

#calculate the number of differencings that need to be applied and apply it
nri_diffed<-diff(nri_train, differences = 5)

#carry out ADF test once again to confirm Stationarity
adf.test(nri_diffed) 

autoplot(nri_diffed) +
  ggtitle("NRI FOREIGN CURRENCY DEPOSITS INTO RBI") +
  xlab("YEAR") +
  ylab("RUPEES (CRORES)")
acf(nri_diffed, lag.max=30)
pacf(nri_diffed, lag.max=30)

#ARIMA  Modelling
nri_arima <- auto.arima(nri_train, seasonal=FALSE, stepwise=FALSE, approximation=FALSE )
nri_arima
nri_arima <- Arima(nri_train,c(0,7,0))
tsdisplay(residuals(nri_arima), lag.max=45, main='(0,7,0) Model Residuals') 
fcast_nri <- forecast(nri_arima, h=6)

#Accuracy check
plot(fcast_nri, type="b", lwd=5, col='blue', main="NRI FOREIGN CURRENCY DEPOSITS INTO RBI", ylab="Rupees in Crores")
lines(nri_test, col="red", lwd=3)
legend("topleft",lty=1,bty = "n",col=c("red", "blue"),c("Actual","Predicted"))

fcast<-forecast(nri_ts, h=5)
plot(fcast, type="b", lwd=5, col='blue', main="FORECAST OF NRI FOREIGN CURRENCY DEPOSITS INTO RBI", ylab="Rupees in Crores")
legend("topleft",lty=1,bty = "n",col=c("blue"),c("Forecast"))
####################################################################################################
# VII_Total_Rupee_Debt

#EXTRACT TEST COLUMN  
trd<-pull(df, VII_Total_Rupee_Debt)

#convert test column into time series datatype AND CARRY OUT adf test
trd_ts<-ts(trd, start=1991)
adf.test(trd_ts)
trd_train <- window(trd_ts, start=1991, end=2014)
adf.test(trd_train)
trd_test <- window(trd_ts, start=2015, end=2020)

#Plot the column
# autoplot(trd_ts) +
#   ggtitle("TOTAL RUPEE DEBT OF RBI") +
#   xlab("Year") +
#   ylab("in Rupees Crore")

#calculate the number of differencings that need to be applied and apply it
trd_diffed<-diff(trd_train, differences = 2)

#carry out ADF test once again to confirm Stationarity
adf.test(trd_diffed) 

autoplot(trd_diffed) +
  ggtitle("TOTAL RUPEE DEBT OF RBI") +
  xlab("YEAR") +
  ylab("RUPEES (CRORES)")
acf(trd_diffed, lag.max=30)
pacf(trd_diffed, lag.max=30)

#ARIMA  Modelling
trd_arima <- auto.arima(trd_train, seasonal=FALSE, stepwise=FALSE, approximation=FALSE )
trd_arima
tsdisplay(residuals(trd_arima), lag.max=45, main='(1,1,0) Model Residuals') 
fcast_trd <- forecast(trd_arima, h=6)

#Accuracy check
plot(fcast_trd, type="b", lwd=5, col='blue', main="TOTAL RUPEE DEBT OF RBI", ylab="Rupees in Crores")
lines(trd_test, col="red", lwd=3)
legend("bottomleft",lty=1,bty = "n",col=c("red", "blue"),c("Actual","Predicted"))

fcast<-forecast(trd_ts, h=30)
plot(fcast, type="b", lwd=5, col='blue', main="TOTAL RUPEE DEBT OF RBI", ylab="Rupees in Crores")
legend("bottomleft",lty=1,bty = "n",col=c("blue"),c("Forecast"))
####################################################################################################
# VIII_Total_Long_Term_Debt

#EXTRACT TEST COLUMN  
tltd<-pull(df, VIII_Total_Long_Term_Debt)

#convert test column into time series datatype AND CARRY OUT adf test
tltd_ts<-ts(tltd, start=1991)
adf.test(tltd_ts)
tltd_train <- window(tltd_ts, start=1991, end=2014)
adf.test(tltd_train)
tltd_test <- window(tltd_ts, start=2015, end=2020)

#Plot the column
# autoplot(tltd_ts) +
#   ggtitle("TOTAL LONG TERM DEBT OF RBI") +
#   xlab("Year") +
#   ylab("in Rupees Crore")

#calculate the number of differencings that need to be applied and apply it
tltd_diffed<-diff(tltd_train, differences = 3)

#carry out ADF test once again to confirm Stationarity
adf.test(tltd_diffed) 

autoplot(tltd_diffed) +
  ggtitle("TOTAL LONG TERM DEBT OF RBI") +
  xlab("Year") +
  ylab("in Rupees Crore")
acf(tltd_diffed, lag.max=30)
pacf(tltd_diffed, lag.max=30)

#ARIMA  Modelling
tltd_arima <- auto.arima(tltd_train, seasonal=FALSE, stepwise=FALSE, approximation=FALSE )
tltd_arima
tltd_arima <- Arima(tltd_train, c(1,4,0))
tsdisplay(residuals(tltd_arima), lag.max=45, main='(1,4,0) Model Residuals') 
fcast_tltd <- forecast(tltd_arima, h=6)

#Accuracy check
plot(fcast_tltd, type="b", lwd=5, col='blue', main="TOTAL LONG TERM DEBT OF RBI", ylab="Rupees in Crores")
lines(tltd_test, col="red", lwd=3)
legend("topleft",lty=1,bty = "n",col=c("red", "blue"),c("Actual","Predicted"))

fcast<-forecast(tltd_ts, h=30)
plot(fcast, type="b", lwd=5, col='blue', main="FORECAST OF TOTAL LONG TERM DEBT OF RBI", ylab="Rupees in Crores")
legend("topleft",lty=1,bty = "n",col=c("blue"),c("Forecast"))
####################################################################################################
# IX_Total_Short_term_Debt

#EXTRACT TEST COLUMN  
tstd<-pull(df, IX_Total_Short_term_Debt)

#convert test column into time series datatype AND CARRY OUT adf test
tstd_ts<-ts(tstd, start=1991)
adf.test(tstd_ts)
tstd_train <- window(tstd_ts, start=1991, end=2014)
adf.test(tstd_train)
tstd_test <- window(tstd_ts, start=2015, end=2020)

#Plot the column
# autoplot(tstd_ts) +
#   ggtitle("TOTAL SHORT TERM DEBT OF RBI") +
#   xlab("Year") +
#   ylab("in Rupees Crore")

#calculate the number of differencings that need to be applied and apply it
tstd_diffed<-diff(tstd_train, differences = 2)

#carry out ADF test once again to confirm Stationarity
adf.test(tstd_diffed) 

autoplot(tstd_diffed) +
  ggtitle("TOTAL SHORT TERM DEBT OF RBI") +
  xlab("Year") +
  ylab("in Rupees Crore")
acf(tstd_diffed, lag.max=30)
pacf(tstd_diffed, lag.max=30)

#ARIMA  Modelling
tstd_arima <- auto.arima(tstd_train, seasonal=FALSE, stepwise=FALSE, approximation=FALSE )
tstd_arima
tstd_arima <- Arima(tstd_train, c(0,3,1))
tsdisplay(residuals(tstd_arima), lag.max=45, main='(0,3,1) Model Residuals') 
fcast_tstd <- forecast(tstd_arima, h=6)

#Accuracy check
plot(fcast_tstd, type="b", lwd=5, col='blue', main="TOTAL SHORT TERM DEBT OF RBI", ylab="Rupees in Crores")
lines(tstd_test, col="red", lwd=3)
legend("topleft",lty=1,bty = "n",col=c("red", "blue"),c("Actual","Predicted"))

fcast<-forecast(tstd_ts, h=30)
plot(fcast, type="b", lwd=5, col='blue', main="FORECAST OF TOTAL SHORT TERM DEBT OF RBI", ylab="Rupees in Crores")
legend("topleft",lty=1,bty = "n",col=c("blue"),c("Forecast"))
####################################################################################################
# Gross_Total_Debt

#EXTRACT TEST COLUMN  
gtd<-pull(df, Gross_Total_Debt)

#convert test column into time series datatype AND CARRY OUT adf test
gtd_ts<-ts(gtd, start=1991)
adf.test(gtd_ts)
gtd_train <- window(gtd_ts, start=1991, end=2014)
adf.test(gtd_train)
gtd_test <- window(gtd_ts, start=2015, end=2020)

#Plot the column
# autoplot(gtd_ts) +
#   ggtitle("GROSS TOTAL DEBT OF RBI") +
#   xlab("Year") +
#   ylab("in Rupees Crore")

#calculate the number of differencings that need to be applied and apply it
gtd_diffed<-diff(gtd_train, differences = 2)

#carry out ADF test once again to confirm Stationarity
adf.test(gtd_diffed) 

autoplot(gtd_diffed) +
  ggtitle("GROSS TOTAL DEBT OF RBI") +
  xlab("Year") +
  ylab("in Rupees Crore")
acf(gtd_diffed, lag.max=30)
pacf(gtd_diffed, lag.max=30)

#ARIMA  Modelling
gtd_arima <- auto.arima(gtd_train, seasonal=FALSE, stepwise=FALSE, approximation=FALSE )
gtd_arima
gtd_arima <- Arima(gtd_train, c(0,3,0))
tsdisplay(residuals(gtd_arima), lag.max=45, main='(0,3,0) Model Residuals') 
fcast_gtd <- forecast(gtd_arima, h=6)

#Accuracy check
plot(fcast_gtd, type="b", lwd=5, col='blue', main="GROSS TOTAL DEBT OF RBI", ylab="Rupees in Crores")
lines(gtd_test, col="red", lwd=3)
legend("topleft",lty=1,bty = "n",col=c("red", "blue"),c("Actual","Predicted"))

fcast<-forecast(gtd_ts, h=30)
plot(fcast, type="b", lwd=5, col='blue', main="GROSS TOTAL DEBT OF RBI", ylab="Rupees in Crores")
legend("topleft",lty=1,bty = "n",col=c("blue"),c("Forecast"))
####################################################################################################
# Debt_Stock_GDP_Ratio

#EXTRACT TEST COLUMN  
dsgratio<-pull(df, Debt_Stock_GDP_Ratio)

#convert test column into time series datatype AND CARRY OUT adf test
dsgratio_ts<-ts(dsgratio, start=1991)
adf.test(dsgratio_ts)
dsgratio_train <- window(dsgratio_ts, start=1991, end=2014)
adf.test(dsgratio_train)
dsgratio_test <- window(dsgratio_ts, start=2015, end=2020)

#Plot the column
# autoplot(dsgratio_ts) +
#   ggtitle("Debt to GDP Ratio") +
#   xlab("Year") +
#   ylab("in Rupees Crore")

#calculate the number of differencings that need to be applied and apply it
dsgratio_diffed<-diff(dsgratio_train, differences = 2)

#carry out ADF test once again to confirm Stationarity
adf.test(dsgratio_diffed)

autoplot(dsgratio_diffed) +
  ggtitle("Debt to GDP Ratio") +
  xlab("Year") +
  ylab("")
acf(dsgratio_diffed, lag.max=30)
pacf(dsgratio_diffed, lag.max=30)

#ARIMA  Modelling
dsgratio_arima <- auto.arima(dsgratio_train, seasonal=FALSE, stepwise=FALSE, approximation=FALSE )
dsgratio_arima
tsdisplay(residuals(dsgratio_arima), lag.max=45, main='(0,1,0) Model Residuals') 
fcast_dsgratio <- forecast(dsgratio_arima, h=6)

#Accuracy check
plot(fcast_dsgratio, type="b", lwd=5, col='blue', main="Debt to GDP Ratio", ylab="Rupees in Crores")
lines(dsgratio_test, col="red", lwd=3)
legend("bottomleft",lty=1,bty = "n",col=c("red", "blue"),c("Actual","Predicted"))

fcast<-forecast(dsgratio_ts, h=30)
plot(fcast, main=" ")
legend("bottomleft",lty=1,bty = "n",col=c("blue"),c("Forecast"))
####################################################################################################
# Debt_Service_Ratio

#EXTRACT TEST COLUMN  
dsratio<-pull(df, Debt_Service_Ratio)

#convert test column into time series datatype AND CARRY OUT adf test
dsratio_ts<-ts(dsratio, start=1991)
adf.test(dsratio_ts)
dsratio_train <- window(dsratio_ts, start=1991, end=2014)
adf.test(dsratio_train)
dsratio_test <- window(dsratio_ts, start=2015, end=2020)

#Plot the column
# autoplot(dsratio_ts) +
#   ggtitle("Debt to GDP Ratio") +
#   xlab("Year") +
#   ylab("in Rupees Crore")

#calculate the number of differencings that need to be applied and apply it
dsratio_diffed<-diff(dsratio_train, differences = 2)

#carry out ADF test once again to confirm Stationarity
adf.test(dsratio_diffed) 

autoplot(dsratio_diffed) +
  ggtitle("Debt to GDP Ratio") +
  xlab("Year") 
acf(dsratio_diffed, lag.max=30)
pacf(dsratio_diffed, lag.max=30)

#ARIMA  Modelling
dsratio_arima <- auto.arima(dsratio_train, seasonal=FALSE, stepwise=FALSE, approximation=FALSE )
dsratio_arima
dsratio_arima <- Arima(dsratio_train, c(0,1,0))
tsdisplay(residuals(dsratio_arima), lag.max=45, main='(0,1,0) Model Residuals') 
fcast_dsratio <- forecast(dsratio_arima, h=6)

#Accuracy check
plot(fcast_dsratio, type="b", lwd=5, col='blue', main="Debt Service Ratio")
lines(dsratio_test, col="red", lwd=3)
legend("bottomleft",lty=1,bty = "n",col=c("red", "blue"),c("Actual","Predicted"))

fcast<-forecast(dsratio_ts, h=30)
plot(fcast, type="b", lwd=5, col='blue', main="Debt Service Ratio forecast")
legend("bottomleft",lty=1,bty = "n",col=c("blue"),c("Forecast"))
####################################################################################################






# cbind(a,c) %>%
#   autoplot(facets=TRUE) +
#   xlab("Year") + ylab("") +
#   ggtitle("Antidiabetic drug sales")
# autoplot(c)
# 
# 
# library(pracma)
# moving.avg <- ts(movavg(a, n=5, type="s"))
# plot(moving.avg)
# 
# #Auto Correlation function
# multilat_acf=acf(multilat_ts, lag.max = 3)
# multilat_pacf=pacf(multilat_ts, lag.max = 36)
# plot(cbind(multilat_ts, multilat_diffed,multilat_pacf), main = "", yax.flip = TRUE)
# 
# #Carrying out the KPSS test
# a %>% ur.kpss() %>% summary()
# a %>% diff(2) %>% ur.kpss() %>% summary()
