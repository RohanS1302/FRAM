##Daily
## PART 1 ##
library(readxl)
library(quantmod)
library(forecast)

## Data Collection ##
NSE <- getSymbols.yahoo("^NSEI", from = "2020-10-31", to = "2022-11-1", verbose = F, auto.assign = F)
NSE <- na.omit(NSE)

ultracemco <- getSymbols.yahoo("ULTRACEMCO.NS", from = "2020-10-31", to = "2022-11-1", verbose = F, auto.assign = F)
ultracemco <- na.omit(ultracemco)


## Close Prices Data ##
close <- cbind(NSE$NSEI.Close, ultracemco$ULTRACEMCO.NS.Close)

## Data Manipulation ##
T_Bill <- read_excel("/Users/rohan/Downloads/FRAM")
T_Bill <- as.data.frame(T_Bill)
T_Billxts <- xts(T_Bill[,-1], order.by = as.Date(T_Bill$Date))

## Return Calculation ##
returns <- as.xts(tail(data.frame(close),-1)/head(data.frame(close),-1) -1)
head(returns)


## Regression Model ##
regression1 <- lm(ULTRACEMCO.NS.Close ~ NSEI.Close, data.frame(returns1[]))
summary(regression1)


## PART 2 ##

library(tseries)

## Return Calculation ##
returns_ultracemco <- as.xts(tail(data.frame(ultracemco$ULTRACEMCO.NS.Close),-1)/head(data.frame(ultracemco$ULTRACEMCO.NS.Close),-1)-1, frequency = 365)
returns_ultracemco <- na.omit(returns_ultracemco)


## Data Manipulation ##
colnames(returns_ultracemco) <- "returns_ultracemco"
mean(returns_ultracemco)
var(returns_ultracemco)

## Data Visualization ##
plot(ultracemco$ULTRACEMCO.NS.Close)
plot(returns_ultracemco)

## model identification AR & MA ##
adf.test(returns_ultracemco, alternative = "stationary")
plot(acf(returns_ultracemco , lag.max = 10))
plot(pacf(returns_ultracemco, lag.max = 10))
auto.arima(returns_ultracemco)
auto.arima(returns_ultracemco,ic = "bic")
arima_final1 <- arima(returns_ultracemco, order= c(0,0,0))
arima_final1
predicted <- predict(arima_final1, n.ahead = 10)
predicted
tsdiag(arima_final1)

## PART 3 ##

library(quantmod)
library(rugarch)
library(rmgarch)

## Data Collection ##
ultracemco1 <- getSymbols("ULTRACEMCO.NS", from = "2020-10-31", to = "2022-11-1")
ultracemco1 <- na.omit(ultracemco1)


## Return Calculation ##
R.ultracemco <- dailyReturn(ULTRACEMCO.NS)

## Implementing Univariate GARCH ##
ug_spec = ugarchspec()
ug_spec

## Implementing EGARCH ##
eg_spec = ugarchspec(variance.model = list(model="eGARCH"))
eg_spec

#Estimating the models
ugfit1 = ugarchfit(spec = ug_spec, data = R.ultracemco) 
ugfit1

#Forecasting
ugforecast1 = ugarchforecast(ugfit1, n.ahead=10) 
ugforecast1

#Weekly
## PART 1 ##
library(readxl)
library(quantmod)
library(forecast)

## Data Collection ##
NSE <- getSymbols.yahoo("^NSEI", from = "2020-10-31", to = "2022-11-1", verbose = F, auto.assign = F, periodicity = "weekly")
NSE <- na.omit(NSE)

ultracemco <- getSymbols.yahoo("ULTRACEMCO.NS", from = "2020-10-31", to = "2022-11-1", verbose = F, auto.assign = F, periodicity = "weekly")
ultracemco <- na.omit(ultracemco)


## Close Prices Data ##
close1 <- cbind(NSE$NSEI.Close, ultracemco$ULTRACEMCO.NS.Close)

## Data Manipulation ##
T_Bill <- read_excel("/Users/rohan/Downloads/FRAM")
T_Bill <- as.data.frame(T_Bill)
T_Billxts <- xts(T_Bill[,-1], order.by = as.Date(T_Bill$Date))

## Return Calculation ##
returns1 <- as.xts(tail(data.frame(close1),-1)/head(data.frame(close1),-1) -1)
head(returns1)

## Regression Model ##
regression1 <- lm(ULTRACEMCO.NS.Close ~ NSEI.Close, data.frame(returns1[]))
summary(regression1)

## PART 2 ##

library(tseries)

## Return Calculation ##
returns_ultracemco <- as.xts(tail(data.frame(ultracemco$ULTRACEMCO.NS.Close),-1)/head(data.frame(ultracemco$ULTRACEMCO.NS.Close),-1)-1, frequency = 365)
returns_ultracemco <- na.omit(returns_ultracemco)

## Data Manipulation ##
colnames(returns_ultracemco) <- "returns_ultracemco"
mean(returns_ultracemco)
var(returns_ultracemco)

## Data Visualization ##
plot(ultracemco$ULTRACEMCO.NS.Close)
plot(returns_ultracemco)

## model identification AR & MA ##
adf.test(returns_ultracemco, alternative = "stationary")
plot(acf(returns_ultracemco , lag.max = 10))
plot(pacf(returns_ultracemco , lag.max = 10))
auto.arima(returns_ultracemco)
auto.arima(returns_ultracemco,ic = "bic")
arima_final1 <- arima(returns_ultracemco, order= c(0,0,0))
arima_final1
predicted <- predict(arima_final1, n.ahead = 10)
predicted
tsdiag(arima_final1)


## PART 3 ##

library(quantmod)
library(rugarch)
library(rmgarch)

## Data Collection ##
ultracemco1 <- getSymbols("ULTRACEMCO.NS", from = "2020-10-31", to = "2022-11-1", periodicity = "weekly")
ultracemco1 <- na.omit(ultracemco1)


## Return Calculation ##
R.ultracemco1 <- dailyReturn(ULTRACEMCO.NS)

## Implementing Univariate GARCH ##
ug_spec = ugarchspec()
ug_spec

## Implementing EGARCH ##
eg_spec = ugarchspec(variance.model = list(model="eGARCH"))
eg_spec

#Estimating the models
ugfit1 = ugarchfit(spec = ug_spec, data = R.ultracemco) 
ugfit1

#Forecasting
ugforecast1 = ugarchforecast(ugfit1, n.ahead=10) 
ugforecast1

#Monthly 
## PART 1 ##
library(readxl)
library(quantmod)
library(forecast)

## Data Collection ##
NSE <- getSymbols.yahoo("^NSEI", from = "2020-10-31", to = "2022-11-1", verbose = F, auto.assign = F, periodicity = "monthly")
NSE <- na.omit(NSE)

ultracemco <- getSymbols.yahoo("ULTRACEMCO.NS", from = "2020-10-31", to = "2022-11-1", verbose = F, auto.assign = F, periodicity = "monthly")
ultracemco <- na.omit(ultracemco)


## Close Prices Data ##
close1 <- cbind(NSE$NSEI.Close, ultracemco$ULTRACEMCO.NS.Close)

## Data Manipulation ##
T_Bill <- read_excel("/Users/rohan/Downloads/FRAM")
T_Bill <- as.data.frame(T_Bill)
T_Billxts <- xts(T_Bill[,-1], order.by = as.Date(T_Bill$Date))

## Return Calculation ##
returns1 <- as.xts(tail(data.frame(close1),-1)/head(data.frame(close1),-1) -1)
head(returns1)

## Regression Model ##
regression1 <- lm(ULTRACEMCO.NS.Close ~ NSEI.Close, data.frame(returns1[]))
summary(regression1)

## PART 2 ##

library(tseries)

## Return Calculation ##
returns_ultracemco <- as.xts(tail(data.frame(ultracemco$ULTRACEMCO.NS.Close),-1)/head(data.frame(ultracemco$ULTRACEMCO.NS.Close),-1)-1, frequency = 365)
returns_ultracemco <- na.omit(returns_ultracemco)

## Data Manipulation ##
colnames(returns_ultracemco) <- "returns_ultracemco"
mean(returns_ultracemco)
var(returns_ultracemco)

## Data Visualization ##
plot(ultracemco$ULTRACEMCO.NS.Close)
plot(returns_ultracemco)

## model identification AR & MA ##
adf.test(returns_ultracemco, alternative = "stationary")
plot(acf(returns_ultracemco , lag.max = 10))
plot(pacf(returns_ultracemco , lag.max = 10))
auto.arima(returns_ultracemco)
auto.arima(returns_ultracemco,ic="bic")
arima_final1 <- arima(returns_ultracemco, order= c(0,0,0))
arima_final1
predicted <- predict(arima_final1, n.ahead = 10)
predicted
tsdiag(arima_final1)

## PART 3 ##

library(quantmod)
library(rugarch)
library(rmgarch)

## Data Collection ##
ultracemco1 <- getSymbols("ULTRACEMCO.NS", from = "2020-10-31", to = "2022-11-1", periodicity = "monthly")
ultracemco1 <- na.omit(ultracemco1)

## Return Calculation ##
R.ultracemco <- dailyReturn(ULTRACEMCO.NS)

## Implementing Univariate GARCH ##
ug_spec = ugarchspec()
ug_spec

## Implementing EGARCH ##
eg_spec = ugarchspec(variance.model = list(model="eGARCH"))
eg_spec

#Estimating the models
ugfit1 = ugarchfit(spec = ug_spec, data = R.ultracemco) 
ugfit1

#Forecasting
ugforecast1 = ugarchforecast(ugfit1, n.ahead=10) 
ugforecast1
