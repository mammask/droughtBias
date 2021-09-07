library(data.table)
library(gamlss)
library(gamlss.dist)
library(ggplot2)
library(zoo)
library(Kendall)

stationId = 210
s.scale = 12

dt_gamma = readRDS("../outputs/bias measurement/Gamma_training_bias_12.RDS")
setDT(dt_gamma)
records = dt_gamma[updatedBasins == stationId, rawSpi[[1]]]

# Compute accumulated precipitation
system.time({
ts_object = ts(records[,Rainfall], start = records[,min(Date)], frequency = 12)
accum_precip = c(rep(NA, s.scale-1),rowSums(embed(ts_object,s.scale),na.rm=FALSE))
records[, AccumRainfall := as.numeric(accum_precip)]
records = records[, .(Date = as.yearmon(Date), Status, Rainfall, `SPI-TVT`, AccumRainfall)]

model_object = records[complete.cases(records)]
model_object[, Trend := 1:.N]

model = gamlss(AccumRainfall ~ Trend, data = model_object, family = GA)
pred = predictAll(model, model_object[, .(AccumRainfall, Trend)], type = 'response')

model_object[, mu := pred$mu]
model_object[, sigma := pred$sigma]

model_object[, ecdfm := gamlss.dist::pGA(AccumRainfall, mu = mu, sigma = sigma)]
model_object[, NSPI := qnorm(ecdfm)]
})
ggplot(data = model_object) + geom_line(aes(x = Date, y = `SPI-TVT`, color = 'Stationary SPI')) +
  geom_line(aes(x = Date, y = `NSPI`, color = 'Non-Stationary SPI')) +
  labs(colour="SPI Version")


# MannKendall(model_object[,`SPI-TVT`])
# MannKendall(model_object[,NSPI])
