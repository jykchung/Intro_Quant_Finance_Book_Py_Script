# Chapter 1  *******************************************************************
# Plotting time series, Moving Averages and VaR==================================================

## Using zoo package, zoo ts bojext,  Simple return, 
## Continuous Compound Return, Histogram, Naieve VaR

install.packages("zoo")
library(zoo)

# Set working directory
setwd("D:/CUHK MSc Econ/My CUHK Courses/5420 Finanical Economics/Quantitative Finance Intro R Very Useful/R for Quantitative Finance Data Set/Introduction to R for Quantitative Finance Data/data_sets/Chapter 1")
# Load .csv file as "zoo" time series object. Note the date format "%Y-%m-%d"
aapl <- read.zoo("aapl.csv", sep=",", header = TRUE, format = "%Y-%m-%d")
head(aapl)
# Plot the ts data
plot(aapl, main="APPLE Closing Prices on NASDAQ", xlab= "Date", ylab="Price (USD)", c="red")

## To find the all time high or low data points
aapl[which.max(aapl)]   
aapl[which.min(aapl)]

# Normally more interested in returns instead of prices. 
# This is because returns are usually stationary. 
# Calculations for simple or continuously compounded returns (in %):

ret_simple <- diff(aapl) / lag(aapl, k = -1) * 100
ret_cont <- diff(log(aapl)) * 100
# Get summary
summary(coredata(ret_simple))
# We see from summary that the minimum return is -51.9%. This is how to call up the date that corresponds to this min return
ret_simple[which.min(ret_simple)]  # returns 2000-09-29 
                                   #          -51.85888

# Get a better understanding of the relative frequency of daily returns, we can plot the histogram
ret_hist = hist(ret_simple, breaks=100, main = "Histogram of Simple Returns", xlab="%")

# To isolate a window in the timeseries, say the year of 2013, then see the peak price in 2013
aapl_2013 <- window(aapl, start = '2013-01-01', end = '2013-12-31')
aapl_2013[which.max(aapl_2013)]     # returns 2013-01-02 545.85


# We can, for example, easily determine the 1 day 99% Value-at-Risk (VaR) using a naive historical approach.
# Naive historical approach is assuming past data has predictive power for future
quantile(ret_simple, probs = 0.01)    # returns 1%   -7.042678


#************************************************************************************
# Moving Average and Decompose Data into Seasonal Trends=================================================================

# Data is in D:\CUHK MSc Econ\My CUHK Courses\5121C Econometrics\My R training material 2017\Quantitative Finance Intro R Very Useful\Bikes Data Set

library('ggplot2') 
library('forecast') 
library('tseries') 

#Step-1: load day.csv 
setwd("D:/CUHK MSc Econ/My CUHK Courses/5121C Econometrics/My R training material 2017/Quantitative Finance Intro R Very Useful/Bikes Data Set")

daily_data = read.csv('day.csv', header=TRUE, stringsAsFactors=FALSE)
head(daily_data)

# Step-2: Transform the dteday column into a proper Data veriable using the as.Date()
# Then plot the time series for inspection
daily_data$Date = as.Date(daily_data$dteday)

ggplot(daily_data, aes(Date, cnt)) + geom_line() + scale_x_date('month')+ ylab("Daily Bike Checkouts")+ xlab("")

# Step-3: The bicycle counts have outliers that we need to clean, so we use the the 
# ts() command to create a time series object count_ts to pass to tsclean(), we end up 
# with a clean_cnt column in our dataframe.
count_ts = ts(daily_data[, c('cnt')]) 
daily_data$clean_cnt = tsclean(count_ts) 

ggplot() + geom_line(data = daily_data, aes(x = Date, y = clean_cnt)) + ylab('Cleaned Bicycle Count')

# using the clean count, clean_cnt, with no outliers, and what we are doing overall is to create 
# moving average variables in the dataframe called cnt_ma and cnt_ma30, for weekly and monthly respectively.  
# Then we plot the original and the moving average lines onto the same chart.

daily_data$cnt_ma = ma(daily_data$clean_cnt, order=7) 
daily_data$cnt_ma30 = ma(daily_data$clean_cnt, order=30) 

head(daily_data)

ggplot() + 
  geom_line(data = daily_data, aes(x = Date, y = clean_cnt, colour = "Counts")) + 
  geom_line(data = daily_data, aes(x = Date, y = cnt_ma, colour = "Weekly Moving Average")) + 
  geom_line(data = daily_data, aes(x = Date, y = cnt_ma30, colour = "Monthly Moving Average")) + 
  ylab('Bicycle Count')

# Decompose the data to see seasonality, trends and cycle


count_ma = ts(na.omit(daily_data$cnt_ma), frequency=30)
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)


#************************************************************************************
#Linear Time Series =================================================================

#Linear time series modeling and forecasting (ARIMA), "forecast" package
install.packages("xts")
install.packages("forecast") # need xts package before installing forecast
install.packages("tseries") 
library(forecast)
library(tseries)
library(xts)

# To change the date column of the UKHP.csv which is in 1991-01 format to %Y-%m format (e.g. Jan 1991) we include  
# format="Y-%m", FUN=as.yearmon arguments in read.zoo(). Then we chack confirm 12 subperiods (called months) in a period (called year).   
hp <- read.zoo("UKHP.csv", sep = ",", header = TRUE, format = "%Y-%m", FUN = as.yearmon)

frequency(hp)

# We change the house prices to simple returns because it will be likely stationary 
hp_ret <- diff(hp) / lag(hp, k = -1) * 100
# Inspect visually that hp_ret is indeed stationary and confirm using ADF test
plot(hp_ret, xlab="Time Periods", ylab="Simple Return", main="UK House Prices Simple Return")

adf.test(hp_ret, alternative = "stationary")

# Plot ACF and PACF to determine the orders (p,d,q)
Acf(hp_ret)
Pacf(hp_ret)

# The number of lags in ACF before a significant drop = p, i.e. in the above looks like p = 1 or 2 
#The spikes in PACF are 1 and 2, i.e. q = 1 or 2 
# So basically, we have to fit all the combinations of p, d, and q to find the model with the lowest AIC value!!

mod0 <- arima(x = hp_ret, order = c(1,0,0))
summary(mod0)
mod1 <- arima(x = hp_ret, order = c(1,0,1))
summary(mod1)
mod2 <- arima(x = hp_ret, order = c(1,0,2))
summary(mod2)
mod3 <- arima(x = hp_ret, order = c(2,0,0))
summary(mod3)
mod4 <- arima(x = hp_ret, order = c(2,0,1))
summary(mod4)
mod5 <- arima(x = hp_ret, order = c(2,0,2))
summary(mod5)

# Confirm our fitting using auto.arima function provided by the forecast package in one step. 
#stationary = TRUE, we restrict the search to stationary models.  
#seasonal = FALSE restricts the search to non-seasonal models. 
#ic="aic" select the Akaike information criteria (AIC) as the measure of relative quality 
mod_auto <- auto.arima(hp_ret, stationary = TRUE, seasonal = FALSE, ic="aic")
summary(mod_auto)

## They are all significant at the 5% level since the respective confidence intervals do 
# not contain zero:
confint(mod_auto)  # or confint("our best model from individual fitting")

# A quick way to validate the choosen model is to plot diagnostics diagrams using the tsdiag() command:
# Look for these 3 features in the charts: 
# 1)The standardized residuals don't show volatility clusters
# 2)No significant autocorrelations between the residuals according to the ACF plot 
# 3)The Ljung-Box test for autocorrelation shows high p-values, so cannot reject the null hypothesis of independent residuals.
# If get an error message, change margins.
par(mar=c(4,4,3,3))

tsdiag(mod_auto)  # This returns 3 charts (standardized Residuals, ACF, and p-value of Ljung Box Statistic)

# To assess how well the model represents the sample data, plot the raw monthly returns (the thin black solid line) versus the fitted values (the thick red dotted line).
plot(mod_auto$x, lty = 1, main = "UK house prices: raw data vs. fitted values", ylab = "Return in percent", xlab = "Date") 
lines(fitted(mod_auto), lty = 2, lwd = 2, col = "red")

# Furthermore, we can calculate common measures of accuracy (mean error, root mean squared error, mean absolute
#error, mean percentage error, mean absolute percentage error, and mean absolute
#scaled error.)
accuracy(mod_auto)

# Forecasting: To predict the monthly returns for the next 48 months (April 2013 to Mar 2017)
fcast48 <- forecast(mod_auto, h=48)
# To plot the forecast with standard errors, we can use the following command:
plot(forecast48)

# Back Testing technique from https://www.datascience.com/blog/introduction-to-forecasting-with-arima-in-r-learn-data-science-tutorials
# Get a sense of how the model's future performance? One method is to reserve a portion of our data as a "hold-out" set, fit the model, 
# and then compare the forecast to the actual observed values

length(hp_ret)     # first find the number of time series data points, say 266 points
hold <- window(ts(hp), start=206)  # Let's say we hold 60 data points from the end for comparison with our prediction 
fit_no_holdout = arima(ts(hp_ret[-c(206:266)]), order=c(2,0,0)) # fit model by excluding the hold data points using -c(range)
fcast_no_holdout <- forecast(fit_no_holdout, h=60) # forecast ahead 60 data points from the start of our hold
plot(fcast_no_holdout, main="Forecast Comparison", xlab="Date", ylab="hp_ret")  # plot the forecast 
lines(ts(hp_ret)) # add back the hold data points for comparison


#************************************************************************************
#Cointegration, page 15=======================================================================

install.packages("zoo")
install.packages("urca")
library("zoo")
library("urca")   # urca library has unit root test and cointegration relationship

# Load some jet fuel ts data
prices <- read.zoo("JetFuelHedging.csv", sep = ",", FUN = as.yearmon, format = "%Y-%m", header = TRUE)

# Here we demonstrate the Classical method of fitting a linear model. The beta
# coefficient of that regression is the optimal hedge ratio. 
simple_mod <- lm(diff(prices$JetFuel) ~ diff(prices$HeatingOil)+0)
summary(simple_mod)  # It showed beta coeff = 0.891 and residual stand error=0.0846
                     # so the hedge is not perfect.

# We try to improve the hedging ratio by by using an existing long-run relationship
# between the levels of jet fuel and heating oil futures prices.
plot(prices$JetFuel, main = "Jet Fuel and Heating Oil Prices", xlab = "Date", ylab = "USD")
lines(prices$HeatingOil, col = "red")

# We use Engle and Granger's two-step estimation technique. Firstly, both time series
# are tested for a unit root (non-stationarity) using the augmented Dickey-Fuller test.
# We find that the null hypothesis of non-stationarity (jet fuel time series contains a unit root)
# cannot be rejected at the 1% significance level

jf_adf <- ur.df(prices$JetFuel, type = "drift")
summary(jf_adf)


ho_adf <- ur.df(prices$HeatingOil, type = "drift")
summary(ho_adf)


# Now use the residuals from a linear fit of the raw ts data instead, and test for stationarity
mod_static <- summary(lm(prices$JetFuel ~ prices$HeatingOil))
error <- residuals(mod_static)
error_cadf <- ur.df(error, type = "none")
summary(error_cadf)


# The test statistic obtained is -8.912 and the critical value for a sample size of 200 
# at the 1% level is -4.00; hence we reject the null hypothesis of non-stationarity. 
# We have thus discovered two cointegrated variables and can proceed with the second 
# step; that is, the specification of an Error-Correction Model (ECM). The ECM represents 
# a dynamic model of how (and how fast) the system moves back to the static equilibrium 
# estimated earlier and is stored in the mod_static variable.
djf <- diff(prices$JetFuel)
dho <- diff(prices$HeatingOil)
error_lag <- lag(error, k = -1)
mod_ecm <- lm(djf ~ dho + error_lag)
summary(mod_ecm)

## p 19
library("zoo")
intc <- read.zoo("intc.csv", header = TRUE, sep = ",", format = "%Y-%m", FUN = as.yearmon)
                 
plot(intc, main = "Monthly returns of Intel Corporation", xlab = "Date", ylab = "Return in percent")
                 
## p 20
Box.test(coredata(intc^2), type = "Ljung-Box", lag = 12)
                 
install.packages("FinTS")
library("FinTS")
ArchTest(coredata(intc))
                 
## p 21
install.packages("rugarch")
                 
library("rugarch")
                 
intc_garch11_spec <- ugarchspec(variance.model = list(garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0)))
                 
intc_garch11_fit <- ugarchfit(spec = intc_garch11_spec, data = intc)
                 
## p 22
intc_garch11_roll <- ugarchroll(intc_garch11_spec, intc, n.start = 120, refit.every = 1, refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE, VaR.alpha = 0.01, keep.coef = TRUE)
                 
report(intc_garch11_roll, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)
                 
## p 23
intc_VaR <- zoo(intc_garch11_roll@forecast$VaR[, 1])
                 
index(intc_VaR) <- as.yearmon(rownames(intc_garch11_roll@forecast$VaR))
                 
                 intc_actual <- zoo(intc_garch11_roll@forecast$VaR[, 2])
                 index(intc_actual) <-
                   as.yearmon(rownames(intc_garch11_roll@forecast$VaR))
                 
                 plot(intc_actual, type = "b", main = "99% 1 Month VaR Backtesting", xlab = "Date", ylab = "Return/VaR in percent")
                 lines(intc_VaR, col = "red")
                 legend("topright", inset=.05, c("Intel return","VaR"), col =
                          c("black","red"), lty = c(1,1))
                 
                 ## p 24
                 intc_garch11_fcst <- ugarchforecast(intc_garch11_fit, n.ahead = 12)
                 intc_garch11_fcst
                 