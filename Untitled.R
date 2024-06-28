# Data of Australian monthly gas production.

library(forecast)
data()


 data("gas")
 View(gas)
 gas
Gas <-  ts(gas, frequency = 12, start = c(1956, 1))

# Understanding the data

plot(Gas)

 # Log transform to fix-up the variation
 
Gas_2 <- log(Gas)
plot(Gas_2)

# Decompose

Dgas <- decompose(Gas_2)
Dgas$figure 
plot(Dgas$figure, 
       type = 'b',
       xlab = "Month",
       ylab = "Seasonality Index", 
       col = "blue",
       las = 2
)

plot(Dgas)
 


# Model 
# ARIMA (p, d, q)
library(forecast)
 fitmodel <- auto.arima(Gas_2)
 fitmodel
 
 
 # Residuals plot
 
 hist(fitmodel$residuals, 
      main = "residuals plot",
      xlab = "error",
      col = "blue",
      freq = F)
  
 lines(density(fitmodel$residuals), col="red", lw =2) 

 
 # Forecast for next 10 years
  pred <- forecast(fitmodel, 10*12)
library(ggplot2) 
autoplot(pred) 
 
 
accuracy(pred)
 
 
 
 


