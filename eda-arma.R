# EDA
# ARMA and SARMA
library(ggplot2)
library(reshape2)
library(gridExtra)

price = read.csv("https://raw.githubusercontent.com/skybullbobby/Weekly-Dairy-Product-Time-Series-Analysis/EDA-ARMA/cleaned_data.csv")
price$date =as.Date(price$date, format="%Y-%m-%d")
# first explore the price trend
pricelong <- melt(price, id="date")

colnames(pricelong) = c("Date", "Product", "Price")
# plot all the four products in one figure
ggplot(pricelong, aes(x=Date, y=Price, colour=Product))+geom_line()

# grid search function for ARMA models
aic_table <- function(data,P,Q){
  table <- matrix(NA,(P+1),(Q+1))
  for(p in 0:P){
    for(q in 0:Q){
      table[p+1,q+1] <- arima(data,order=c(p,0,q),
                              seasonal=list(order=c(1,0,0),period=10))$aic# period yet to be decided
    }
  }
  dimnames(table) <- list(paste("AR",0:P, sep=""),paste("MA",0:Q,sep=""))
  table
}


# spectral analysis for butter
date = seq(1,length(price$butter))
buttertime =ts(price$butter, start=decimal_date(ymd("2012-03-03")), frequency = 365.25/7)
butter_low <- ts(loess(price$butter~date,span=0.5)$fitted,
                 start=decimal_date(ymd("2012-03-03")), frequency = 365.25/7)
butter_hi <- ts(price$butter - loess(price$butter~date,span=0.1)$fitted,
                start=decimal_date(ymd("2012-03-03")), frequency = 365.25/7)
butter_cycles <- buttertime - butter_low # - butter_hi
bspectrum = spectrum(butter_cycles, spans=c(3,5,3), main="Smoothed Spectrum for Butter Prices")
plot(ts.union(buttertime, butter_low, butter_hi, butter_cycles), main="Butter price trend")

butterdensities = cbind(bspectrum$freq, bspectrum$spec)


# spectral analysis for cheese
cheesetime = ts(price$cheese, start=decimal_date(ymd("2012-03-03")), frequency = 365.25/7)
cheese_low = ts(loess(price$cheese~date,span=0.5)$fitted,
                start=decimal_date(ymd("2012-03-03")), frequency = 365.25/7)
cheese_hi = ts(price$cheese-loess(price$cheese~date,span=0.1)$fitted,
                start=decimal_date(ymd("2012-03-03")), frequency = 365.25/7)
cheese_cycles =cheesetime - cheese_low # - cheese_hi

plot(ts.union(cheesetime, cheese_low, cheese_hi, cheese_cycles), main="Cheese price trend")
cspectrum = spectrum(cheese_cycles,span=c(3,5,3), main="Smoothed Spectrum for Cheese Prices")

cheesedensities = cbind(cspectrum$freq, cspectrum$spec)


