# Problem set 2

library(zoo)
library(ggplot2)
library(ggpubr)
library(forecast)

# Taking the data of Australian monthly Gas production from 1956 to 1995 with frequency 12.
# I have taken the data from the forecast package of data function. 
data("gas")
gas

Gas_Production <- ts(gas, start = c(1956, 1), freq = 12)
Gas_Production_log <- ts(log(gas), start = c(1956, 1), freq = 12)

fig_1 <- autoplot.zoo(Gas_Production) + 
  xlab("Year")+
  ylab("Gas production")
ggtitle("Monthly gas production in Australia")
xlim(1956, 1995+8/12)
plot(fig_1)

fig_2 <- autoplot.zoo(Gas_Production_log) + 
  xlab("Year")+
  ylab("Gas_Production_Log Transformed")
ggtitle("Monthly gas production in Australia")
xlim(1956, 1995+8/12)
plot(fig_2)

plot_a <- ggarrange(fig_1, fig_2, nrow = 2, ncol = 1, align = "v")
plot_a


# There are notable cherasteristics in this given time series . there appears to be a trend in the series,
# as the overall level of the observation changes over time. 
# The variation in the data for log transformed is constant at all observation point except 1970



# question (b)

library(smoots)



ts_zoo <- zoo(Gas_Production_log)
rate <- as.ts(ts_zoo)

plot_rate <- autoplot.zoo(rate) +
  xlab("Year") +
  ylab("Log-transformed Gas production rate") +
  ggtitle("Monthly log transformed Gas production rate")
plot(plot_rate)  


est <- msmooth(rate)
bwidth <- est$b0
bwidth

trend <- fitted(est)
df <- data.frame(
  t = time(rate),
  trend = trend
)

plot_trend <- plot_rate +
  geom_line(data = df, aes(x = t, y = trend), color = "red", linewidth = 0.8) +
  ggtitle("The observed series (black) together with the estimated local line trend")

plot_trend


# Problem c

library(forecast)

res <- resid(est)
plot_residuals <- autoplot.zoo(res) +
  xlab("Year") +
  ylab("Residuals Values")+
ggtitle("The residuals series")+
  geom_hline(yintercept = 0, color ="red")
plot_residuals

acf <- ggAcf(as.numeric(res))+
  ggtitle("Correlogram of the deterended series")

acf



# Problem d


p_max <- q_max <- 2
p <- 0:p_max
q <- 0:q_max
bic <- matrix(NA, nrow = p_max + 1, ncol = q_max + 1)
rownames(bic) <- paste0("p=", p)
colnames(bic) <-  paste0("q=", q)
n <- length(res)

for (p0 in p) {
  for (q0 in q) {
    arma <- arima(res, order = c(p0, 0, q0), include.mean = FALSE)
    bic[(p0 + 1), (q0 + 1)] <- AIC(arma, k = log(n))
  }
  
}
bic

pq_opt <- unname(which(bic == min(bic), arr.ind = TRUE) - 1)
p_opt <- pq_opt[[1]]
q_opt <- pq_opt[[2]]
p_opt
q_opt


arma_opt <- arima(res,order = c(p_opt, 0, q_opt), include.mean = FALSE)
arma_opt


# Problem e

library(tseries)

jarque.bera.test(res)

# A bootstrap requires setting a seed for reproducibility

set.seed(123)

#Forecast for the parametic part
fc_para <- forecast(arma, h = 5, bootstrap = TRUE, level = 95)


# Forecast for the non-parametic part
fc_trend <- tail(est, 1) + (1:2) * diff(tail(est,2))

# Forecast according to the complete model
fc_point <- exp(fc_para$mean + fc_trend)
fc_low <- exp(fc_para$lower + fc_trend)
fc_up <- exp(fc_para$upper + fc_trend)

# data with the results

df <- data.frame(
   Year = c(time(fc_point)),
            fc_point = c(fc_point),
            fc_low = c(fc_low),
            fc_up = c(fc_up)
          )
df

# Plot the original series with point and interval forecast at the end

autoplot.zoo(Gas_Production_log) +
  geom_ribbon(
    data = df,
    aes(x= Year, ymin = fc_low, ymax = fc_up),
    fill = alpha("red", 0.2)
  )+
  geom_line(data = df, aes(x = Year, y = fc_point), color = "red") +
  ggtitle("Forecast of the Gas production in Austrilia") +
  xlab("Year") +
  ylab("Gas production")

fc_mc <- exp(modelCast(est, p = p_opt, q = q_opt, h = 5,
                       mrthod = "boot"))
fc_mc








