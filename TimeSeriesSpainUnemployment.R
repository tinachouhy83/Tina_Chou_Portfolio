install.packages("tidyverse")
install.packages("corrgram")
install.packages("tseries")
install.packages("readxl")
install.packages("urca")
install.packages("forecast")
install.packages("trend")
install.packages("zoo")
install.packages("reshape")
install.packages("conflicted")

library(conflicted)
library(tidyverse)
library(readxl)
library(corrgram)
library(tseries)
library(urca)
library(forecast)
library(trend)
library(zoo)
library(reshape)
library(plyr)
library(dplyr)

setwd("~/Downloads")
Unemployment<-read_excel("~/Desktop/MacroData.xlsx",sheet="Sheet2") #unemployment rate (down at 400 something)
Inflation<-read_excel("~/Desktop/MacroData.xlsx",sheet="Sheet3") #comsumer price index (down at 1000 something)
GDP<-read_excel("~/Desktop/MacroData.xlsx",sheet="Sheet7") #gross domestic product, column 4 (rows 3-432)


View(GDP)
#select variables from the larger dataset

Year<-Unemployment[1,c('Column6','Column7','Column8','Column9','Column10','Column11','Column12','Column13','Column14','Column15','Column16','Column17','Column18','Column19','Column20','Column21','Column22','Column23','Column24','Column25','Column26','Column27','Column28','Column29','Column30','Column31','Column32','Column33','Column34','Column35','Column36','Column37','Column38','Column39','Column40','Column41','Column42','Column43','Column44','Column45','Column46','Column47','Column48','Column49','Column50','Column51','Column52','Column53','Column54','Column55','Column56','Column57','Column58','Column59','Column60','Column61','Column62','Column63','Column64')]

SpainUn<-Unemployment[466,c('Column6','Column7','Column8','Column9','Column10','Column11','Column12','Column13','Column14','Column15','Column16','Column17','Column18','Column19','Column20','Column21','Column22','Column23','Column24','Column25','Column26','Column27','Column28','Column29','Column30','Column31','Column32','Column33','Column34','Column35','Column36','Column37','Column38','Column39','Column40','Column41','Column42','Column43','Column44','Column45','Column46','Column47','Column48','Column49','Column50','Column51','Column52','Column53','Column54','Column55','Column56','Column57','Column58','Column59','Column60','Column61','Column62','Column63','Column64')]

SpainInf<-Inflation[1082,c('Column6','Column7','Column8','Column9','Column10','Column11','Column12','Column13','Column14','Column15','Column16','Column17','Column18','Column19','Column20','Column21','Column22','Column23','Column24','Column25','Column26','Column27','Column28','Column29','Column30','Column31','Column32','Column33','Column34','Column35','Column36','Column37','Column38','Column39','Column40','Column41','Column42','Column43','Column44','Column45','Column46','Column47','Column48','Column49','Column50','Column51','Column52','Column53','Column54','Column55','Column56','Column57','Column58','Column59','Column60','Column61','Column62','Column63','Column64')]

SpainGDP<-GDP[542,c('Column6','Column7','Column8','Column9','Column10','Column11','Column12','Column13','Column14','Column15','Column16','Column17','Column18','Column19','Column20','Column21','Column22','Column23','Column24','Column25','Column26','Column27','Column28','Column29','Column30','Column31','Column32','Column33','Column34','Column35','Column36','Column37','Column38','Column39','Column40','Column41','Column42','Column43','Column44','Column45','Column46','Column47', 'Column48','Column49','Column50','Column51','Column52','Column53','Column54','Column55','Column56','Column57','Column58','Column59','Column60','Column61','Column62','Column63','Column64')]

View(Year)
#transpose the columns to observations

t_SpainGDP<-t(SpainGDP) #it is still data frame
t_SpainGDP<-as.numeric(t_SpainGDP) #we change it into "numbers", so like list instead of dataframe

View(t_SpainGDP)

t_SpainInf<-t(SpainInf)
t_SpainInf<-as.numeric(t_SpainInf)
View(t_SpainInf)


t_SpainUnem<-t(SpainUn)
t_SpainUnem<-as.numeric(t_SpainUnem)
View(t_SpainUnem)

t_Year<-t(Year)
t_Year<-as.numeric(t_Year)
View(t_Year)


TimeSeriesSpain<-cbind(t_Year, t_SpainGDP, t_SpainInf, t_SpainUnem) #put the transposed data altogether
View(TimeSeriesSpain)



TimeSeriesSpain<-as.data.frame(TimeSeriesSpain)
conflict_prefer("rename", "plyr")
TimeSeriesSpain <- rename(TimeSeriesSpain, c(t_Year="Years"))
View(TimeSeriesSpain)


TimeSeriesSpain <- rename(TimeSeriesSpain, c(t_SpainGDP="GrossDP"))

TimeSeriesSpain <- rename(TimeSeriesSpain, c(t_SpainInf="Inflation"))

TimeSeriesSpain <- rename(TimeSeriesSpain, c(t_SpainUnem="Unemploy"))

View(TimeSeriesSpain)


#tell R this is time series data#
tsUR<-zoo(TimeSeriesSpain$Unemploy, order.by = TimeSeriesSpain$Years)
tsIF<-zoo(TimeSeriesSpain$Inflation, order.by = TimeSeriesSpain$Years)
tsGDP<-zoo(TimeSeriesSpain$GrossDP, order.by = TimeSeriesSpain$Years)



#create plots of the timeseries#

ggplot(data = TimeSeriesSpain, aes(x = TimeSeriesSpain$Years, y = TimeSeriesSpain$GrossDP)) + geom_point()
ggplot(data = TimeSeriesSpain, aes(x = TimeSeriesSpain$Years, y = TimeSeriesSpain$Inflation))+ geom_point()
ggplot(data = TimeSeriesSpain, aes(x = TimeSeriesSpain$Years, y = TimeSeriesSpain$Unemploy))+ geom_point()


#test for stationarity, if it's stationary, then you don't have to do the kpss test cus it is near flat
#if the p-value is smaller than 0.05, then it is stationary
adf.test(tsGDP)
adf.test(tsIF)
adf.test(tsUR)

?kpss.test
?adf.test

#Warning message:In kpss.test(TimeSeriesSpain$Inflation, null = "Trend")#
#if the p-value is smaller than 0.05, then it is NOT trend stationary
kpss.test(TimeSeriesSpain$GrossDP, null = "Trend") #null is trend-stationarity; alternative is non-stationarity
kpss.test(TimeSeriesSpain$Inflation, null = "Trend")
kpss.test(TimeSeriesSpain$Unemploy, null = "Trend")

#check the correlograms
#How many times the errors are significantly correlated with the previous ones. Non-stationarity is the correlation of the errors (the errors higher than the blue line is correlated)
#errors are something that are not predictable variables which we need to remove so that we can do modeling
#so being positively and negatively correlated is still correlation
acf(tsUR)
acf(tsIF)
acf(tsGDP)

pacf(tsUR)
pacf(tsIF)
pacf(tsGDP)

#don't do the box tests (it's a way to do the analysis without looking at the pictures)
Box.test(tsUR)
Box.test(tsIF)
Box.test(tsGDP)

#transform the data#

#differencing#
InflDiff=diff(tsIF)
InflDiff2=diff(tsIF, differences=16)
URdiff=diff(tsUR)
URDiff2=diff(tsUR, differences=9)
GDPdiff=diff(tsGDP)
GDPDiff2=diff(tsGDP, differences=15)
YearDiff=diff(TimeSeriesSpain$Years)

kpss.test(InflDiff2, null = "Trend")
kpss.test(URDiff2, null = "Trend")
kpss.test(GDPDiff2, null = "Trend")

acf(URDiff2)
acf(InflDiff2)
acf(GDPDiff2)

pacf(URDiff2)
pacf(InflDiff2)
pacf(GDPDiff2)

#detrending #EXPLAIN THISSSS!!!!
m<-lm(coredata(GDPdiff)~index(GDPdiff))
GDPdetrend<-zoo(resid(m),index(GDPdiff))

plot(GDPdetrend)


#model data: hard cutoff at PACF needs to have the Arima
pacf(URDiff2)
pacf(InflDiff2)
pacf(GDPDiff2)
acf(URDiff2)
acf(InflDiff2)
acf(GDPDiff2)

#first is the AR, second is the difference, third is the moving average
#try a bunch of models with different numbers and pick the one with the lowest AIC
#Unemployment Rate
Arima(URDiff2, order = c(1, 0, 0),
      include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
      method = "ML")


Arima(URDiff2, order = c(1, 0, 1),
      include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
      method = "ML")

Arima(tsUR, order = c(1, 0, 0),
      include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
      method = "ML")

Arima(tsUR, order = c(1, 0, 1),
      include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
      method = "ML")

Arima(tsUR, order = c(2, 0, 1),
      include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
      method = "ML")

#InflationRate
Arima(InflDiff, order = c(1, 0, 0),
      include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
      method = "ML")

Arima(InflDiff1, order = c(1, 0, 1),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")

Arima(InflDiff, order = c(0, 0, 0),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")

Arima(InflDiff, order = c(0, 0, 7),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")

Arima(InflDiff, order = c(0, 0, 1),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")

Arima(InflDiff, order = c(0, 0, 2),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")

#GDP
Arima(GDPDiff2, order = c(1, 0, 0),
      include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
      method = "ML")

Arima(GDPDiff2, order = c(2, 0, 2),
      include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
      method = "ML")

Arima(GDPdiff, order = c(1, 0, 1),
      include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
      method = "ML")

Arima(GDPdiff, order = c(2, 0, 1),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")

Arima(tsGDP, order = c(2, 0, 1),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")


UmData<-Arima(tsUR, order = c(2, 0, 1),
              include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
              method = "ML")

InfData<-Arima(InflDiff, order = c(1, 0, 0),
               include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
               method = "ML")

GDPData<-Arima(GDPdiff, order = c(1, 0, 1),
              include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
              method = "ML")

plot(forecast(UmData,h=10)) #h is how many time periods you wanna do the forecast (in years)
plot(forecast(InfData,h=10))
plot(forecast(GDPData,h=10))

