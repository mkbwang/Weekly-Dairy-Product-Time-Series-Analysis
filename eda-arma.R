# EDA
# ARMA and SARMA
library(ggplot2)
library(reshape2)
library(gridExtra)
library(lubridate)

price = read.csv("https://raw.githubusercontent.com/skybullbobby/Weekly-Dairy-Product-Time-Series-Analysis/EDA-ARMA/cleaned_data.csv")
price$date =as.Date(price$date, format="%Y-%m-%d")

ggplot(price, aes(x=date, y=butter))+geom_line()


# spectral analysis for butter price fluctuation in short term
date = seq(1,length(price$butter))
buttertime =ts(price$butter, start=decimal_date(ymd("2012-03-03")), frequency = 365.25/7)
butter_low <- ts(loess(price$butter~date,span=0.5)$fitted,
                 start=decimal_date(ymd("2012-03-03")), frequency = 365.25/7)
butter_hi <- ts(price$butter - loess(price$butter~date,span=0.1)$fitted,
                start=decimal_date(ymd("2012-03-03")), frequency = 365.25/7)
butter_cycles <- buttertime - butter_low - butter_hi
bspectrum = spectrum(butter_cycles, spans=c(3,5,3), main="Smoothed Spectrum for Butter Prices")
plot(ts.union(buttertime, butter_low, butter_hi, butter_cycles), main="Butter price trend")

butterdensities = cbind(bspectrum$freq, bspectrum$spec)


# for the long trend, see if linear fit is good
butter_long = as.vector(butter_low)
slr = lm(butter_long~date)
summary(slr)
slrresiduals = slr$residuals
# check the autocorrelation of the residuals
acf(slr$residuals, lag.max=52)

# grid search function for ARIMA models with difference one
aic_table <- function(data ,P, Q){
  table <- matrix(NA,(P+1),(Q+1))
  for(p in 0:P){
    for(q in 0:Q){
      table[p+1, q+1] <- arima(data,order=c(p,1,q))$aic
    }
  }
  dimnames(table) <- list(paste("AR",0:P, sep=""),paste("MA",0:Q,sep=""))
  table
}

# search for optimal parameter
butter_long_aic <- aic_table(slrresiduals, 4, 5)

# 211 turns out be the optimal model
model <- arima(slrresiduals, order=c(2,1,1))

# check the acf
acf(model$residuals, lag.max=52)

# I notice a potential seasonality of 18, given that 
# lag 26 has a different sign I try arma(2,0,0) for the seasonality
model2 <- arima(slrresiduals, order=c(2,1,1),
  seasonal=list(order=c(2,0,0), period=18))

# check acf again, much better
acf(model2$residuals, lag.max=52)

model2
