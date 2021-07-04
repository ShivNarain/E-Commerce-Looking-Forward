#ECON5337
#Group 2 Project - Conrad Andos, David Jackson, Zakary Morrow, Shiv Narain, Cuong Vu

#Load and prepare our data

library(urca)
library(ggplot2)
library(forecast)

ecommerce <- read.csv("/Users/conradandos/Downloads/ecommerce1.csv")
ecommerce

ecommHOLD = ts(ecommerce[,2], start=c(2008,1), end = c(2019,10),frequency = 12)
ggseasonplot(ecommHOLD)

#easier
ecoHOLD = ts(ecommerce[,2], start=c(2008,1), end = c(2019,10),frequency = 12)
eco = ts(ecommerce[,2], start=c(2008,1),frequency = 12)
eco
ecoHOLD

#Add some plots
autoplot (eco, main = "E-Commerce Sales 2008-2020", ylab="Millions of Dollars",colour = "RED")

seasonplot(window(ecoHOLD, start=c(2018,1), end=c(2018,12)),
           main= "Volume by month indicates large sesional variation", ylab="Millions of Dollars", col="BLUE")

ecoHOLD %>% tsdisplay(main="Autocorrelation, Partial Autocorrelation and Trend")

autoplot(ecoHOLD)

#BoxCox Transformations

lambECO1=BoxCox.lambda(ecoHOLD)
lambECO1

#part h portion
lambECO2=BoxCox.lambda(eco)
lambECO2

transECO1=BoxCox(ecoHOLD, lambECO1)
autoplot(transECO1)


transECO2=BoxCox(eco, lambECO2)

#Unit Root tests

summary(ur.df(transECO1, type=("trend"), lags = 25, selectlags = "AIC"))
summary(ur.df(diff(transECO1), type=("trend"), lags = 25, selectlags = "AIC"))
summary(ur.df(diff(diff(transECO1)), type=("trend"), lags = 25, selectlags = "AIC"))

#Determining Appropriate Model

ggtsdisplay(diff(diff(transECO1, 12)))

#Try out different models

Arima(transECO1, order=c(2,1,3), seasonal = c(0,1,0))
#AIC=-389.5   AICc=-388.81   BIC=-372.34

Arima(transECO1, order=c(2,1,4), seasonal = c(0,1,0))
#AIC=-406.56   AICc=-405.63   BIC=-386.54

Arima(transECO1, order=c(2,1,5), seasonal = c(0,1,0))
#AIC=-410.07   AICc=-408.87   BIC=-387.19

Arima(transECO1, order=c(2,1,6), seasonal = c(0,1,0))
#AIC=-408.08   AICc=-406.57   BIC=-382.35

modelTRANS=Arima(transECO1, order=c(2,1,5), seasonal = c(0,1,0))

#Look at residuals***************

ggtsdisplay(modelTRANS$residuals)
checkresiduals(modelTRANS)

#Ljung-Box Test***************

Box.test(modelTRANS$residuals, lag=30, type = "Ljung-Box")


#forecast model

foreECO1=forecast(modelTRANS,h=4,lambda=lambECO1,biasadj=TRUE)
foreECO1

accuracy(foreECO1, window(eco,start=c(2019, 11)))

#plot chart
upper=ts(foreECO1$upper[,2], start = c(2019,11), frequency = 12)
lower=ts(foreECO1$lower[,2], start = c(2019,11), frequency = 12)

#window
winECO1=window(eco, start=c(2019,6))

#plot
plot(cbind(winECO1, foreECO1$mean, upper, lower), plot.type = "single", ylab = "Sales in Millions $", col = c("BLACK", "BLUE", "RED", "RED"))
legend("topleft",legend=c("E-commerce Sales","Forecast","Upper","Lower"),col=c("BLACK","BLUE","RED", "RED"),lty=c("solid","solid","solid","solid"),text.font=2)


#Artificial Neural Network

#First, let's get the number of AR pieces, without MA pieces
(auto.arima(transECO1,max.q=0,max.Q=0))
# That gave us 2 non seasonal AR and 0 seasonal AR pieces

# Now we will try the manual way-using our SARIMA values for p and P
#  Note: 2*SSR + 2*k to get AIC
nnetar(ecoHOLD,p=2,P=0, lambda=0)
nnmodel=nnetar(ecoHOLD,p=2,P=0, scale.inputs = TRUE)
nnfore=forecast(nnmodel, h=4, PI=TRUE)
nnfore
accuracy(nnfore,eco) # RMSE 15679.85

nnetar(ecoHOLD,p=2,P=1, lambda=0)
nnmodel=nnetar(ecoHOLD,p=2,P=0, scale.inputs = TRUE)
nnfore=forecast(nnmodel, h=4, PI=TRUE)
nnfore
accuracy(nnfore,eco) # RMSE 15,549.72

nnetar(ecoHOLD,p=2,P=2, lambda=0)
nnmodel=nnetar(ecoHOLD,p=2,P=0, scale.inputs = TRUE)
nnfore=forecast(nnmodel, h=4, PI=TRUE)
nnfore
accuracy(nnfore,eco) # RMSE 15,996

nnetar(ecoHOLD,p=2,P=2, lambda=0)
nnmodel=nnetar(ecoHOLD,p=2,P=0, scale.inputs = TRUE)
nnfore=forecast(nnmodel, h=4, PI=TRUE, biasadj=TRUE)
nnfore
accuracy(nnfore,eco) # RMSE 16,230

nnetar(ecoHOLD,p=2,P=2, lambda=0)
nnmodel=nnetar(ecoHOLD,p=2,P=0, lambd=0)
nnfore=forecast(nnmodel, h=4, PI=TRUE, biasadj=TRUE)
nnfore
accuracy(nnfore,eco) # RMSE 14546

nnetar(ecoHOLD,p=2,P=2, lambda=0)
nnmodel=nnetar(ecoHOLD,p=2,P=0, lambd=0)
nnfore=forecast(nnmodel, h=4, PI=TRUE, biasadj=FALSE)
nnfore
accuracy(nnfore,eco) # RMSE 14632


# Try the automated way
nnetar(ecoHOLD, lambda=0)
nnautomod=nnetar(ecoHOLD, lambda=0)
nnautofore=forecast(nnautomod, h=4, PI=TRUE)
nnautofore
autoplot(nnautofore)
accuracy (nnautofore, eco) #RMSE-7181.878

# The automated way scaling inputs  #  This is best model
nnetar(ecoHOLD, lambda=0, scale.inputs=TRUE) #sig*2 is .11456
nnautomod=nnetar(ecoHOLD, lambda=0, scale.inputs=TRUE)
nnautofore=forecast(nnautomod, h=4, PI=TRUE)
nnautofore
autoplot(nnautofore)
accuracy (nnautofore, eco) #RMSE-7034.47

#First try the automated way scaling inputs then unscaling
#nnetar(ecoHOLD, lambda=0, scale.inputs=TRUE)
#nnautomod=nnetar(ecoHOLD, lambda=0, scale.inputs=TRUE)
#nnautofore=forecast(nnautomod, h=4, PI=TRUE, lambda=0, biasadj=TRUE)
#nnautofore
#autoplot(nnautofore)
#accuracy (nnautofore, eco) #RMSE-7235.81

autoplot(nnautofore, PI=TRUE, main="Neural Net Forecast of E-Commerce Data", ylab="Millions of Dollars")
plot(nnautofore, main="Neural Net Forecast of E-Commerce Data", ylab="Millions of Dollars")
nnautofore=forecast(nnautomod, h=10, PI=TRUE)

autoplot(eco) +
  autolayer(nnautofore$mean, series = "nnetar Forecast", linetype = "dashed") +
  theme_minimal() +
  ylab("millions of dollars")

NNetModel<- nnautofore$mean
ArimaModel <-foreECO1$mean
autoplot(nnautofore, main="Neural Net Forecast of E-Commerce Data", ylab="Millions of Dollars")
Actual=window(eco, start=c(2019,1))
all=cbind(NNetModel, ArimaModel,Actual)
autoplot(all, main="Comparison of Model Types", ylab="Millions of Dollars")







#PART H

lambECO2=BoxCox.lambda(eco)
lambECO2

transECO2=BoxCox(eco, lambECO2)

modelH=Arima(transECO2, order=c(2,1,5), seasonal=c(0,1,0))

foreH=forecast(modelH,h=6,lambda=lambECO2,biasadj=TRUE)
foreH

upperH=ts(foreH$upper[,2], start = c(2020,3), frequency = 12)
lowerH=ts(foreH$lower[,2], start = c(2020,3), frequency = 12)

winECO2=window(eco, start=c(2019,4))

plot(cbind(winECO2, foreH$mean, upperH, lowerH), plot.type = "single", main="SARIMA Forecast of E-Commerce Sales", ylab = "Sales in Millions $", col = c("BLACK", "BLUE", "RED", "RED"))
legend("topleft",legend=c("E-commerce Sales","Forecast","Upper","Lower"),col=c("BLACK","BLUE","RED", "RED"),lty=c("solid","solid","solid","solid"),text.font=2, cex = 0.75)

