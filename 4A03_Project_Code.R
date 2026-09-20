#STATS 4A03: Time Series
#Henry Nguyen 400475938
#Time Series Analysis of Monthly Ontario Energy Demand

#libraries
library(forecast)
library(tseries)
library(TSA)
library(ggplot2)
library(xts)
library(gridExtra)

### DATA LOADING / PREPROCESSING
energy = read.csv("C:/Users/hnguy/OneDrive/Desktop/STATS4A03/STATS4A03/Project/energy.csv") 

#missing values
anyNA(energy$Observation.value)
nrow(energy$Observation.value)

#demand as int
energy$demand = as.integer(gsub(",","",energy$Observation.value))

#date time breaks for aggregating
energy$date.time = as.POSIXct(strptime(energy$Time.period..UTC.,
                                       format="%Y-%m-%dT%H:%M:%S",
                                       tz="UTC"))

energy$month = as.Date(cut(energy$date.time,breaks='month'))

#monthly aggregation
month.data = aggregate(demand~month,data=energy,mean)

#time series object
energy.month.ts = ts(month.data$demand, start=c(2010,1), end=c(2026,3), frequency=12)

#subsetting to 2014-2026
month.sub = window(energy.month.ts,start=c(2014,1),end=c(2026,3))

#plot
autoplot(as.xts(month.sub))+
  labs(x="Time",y='Energy Demand (MW)',title="Ontario Energy Demand by Month")

### ACF/PACF plots

#ACF, PACF plots of raw data
Acf(month.sub)
Pacf(month.sub)

#seasonal diff D=1
Acf(diff(month.sub,lag=12))
Pacf(diff(month.sub,lag=12))

#diff, seasonal diff d=1, D=1
Acf(diff(diff(month.sub,lag=12)))
Pacf(diff(diff(month.sub,lag=12)))

### DIAGNOSTICS

#Candidate 1
fit.mo1 = Arima(
  month.sub,
  order=c(1,0,1),
  seasonal=c(1,1,1)
)
summary(fit.mo1)
checkresiduals(fit.mo1)

#Candidate 2
fit.mo2 <- Arima(
  month.sub,
  order = c(3, 1, 1),
  seasonal = c(1, 1, 1)
)
summary(fit.mo2)
checkresiduals(fit.mo2)

### PERFORMANCE

#Rolling origin CV
sarima.fit = function(y,h){
  fit = Arima(y,order=c(3,1,1),seasonal=c(1,1,1))
  return(forecast(fit,h=h))
}
rmse=list()
mae=list()
for (a in c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)){
  cv = tsCV(month.sub, sarima.fit, h=a, initial=84)
  rmse[a]=sqrt(mean(cv^2,na.rm=T))
  mae[a]=mean(abs(cv),na.rm=T)
}

#ROCV plot by h=1:24
metrics = data.frame(hor=seq(1,24),rm=unlist(rmse),mae=unlist(mae))
par(mfrow=c(1,2))
p1=ggplot(data=metrics,mapping=aes(x=hor,y=rm))+geom_point()+
  labs(x='Horizon',y='RMSE',title='RMSE at Various Horizons (h)')+
  scale_x_continuous(breaks=seq(0,24,2),limits = c(1,24))
p2=ggplot(data=metrics,mapping=aes(x=hor,y=mae))+geom_point()+
  labs(x='Horizon',y='MAE',title='MAE at Various Horizons (h)')+ 
  scale_x_continuous(breaks=seq(0,24,2),limits = c(1,24)) 
grid.arrange(p1, p2, ncol=2)

#Fitted vs Observed, 2-Year Forecast
par(mfrow=c(1,2))
plot(month.sub,
     type = "l",
     main = "SARIMA(3,1,1)(1,1,1)[12]: Observed vs Fitted")
lines(fitted(fit.mo2),
      col = "blue",
      type = "o")
fc <- forecast(fit.mo2, h = 24)
plot(fc,type = "o",
     main = "2-Year Forecast from SARIMA Model")