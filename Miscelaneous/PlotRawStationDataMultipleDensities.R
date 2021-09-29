# stationRawData = function(spiData, spiScales, stationId = "3357"){
library(data.table)
library(ggplot2)
library(zoo)

spiScales = 12
stationId = "3357"


# normalTr = readRDS(paste0("../outputs/bias measurement/Normal_training_bias_",spiScales,".RDS"))
# normalTr = normalTr[updatedBasins == stationId,rawSpi[[1]]]
# normalTr[, Distribution := "Normal"]
# 
# lognormalTr = readRDS(paste0("../outputs/bias measurement/LogNormal_training_bias_",spiScales,".RDS"))
# lognormalTr = lognormalTr[updatedBasins == stationId,rawSpi[[1]]]
# lognormalTr[, Distribution := "Lognormal"]

gammaTr = readRDS(paste0("../outputs/bias measurement/Gamma_training_bias_",spiScales,".RDS"))
gammaTr = gammaTr[updatedBasins == stationId,rawSpi[[1]]]
gammaTr[, Distribution := 'Stationary SPI']

nsgammaTr = readRDS(paste0("../outputs/bias measurement/NSGamma_training_bias_",spiScales,".RDS"))
nsgammaTr = nsgammaTr[updatedBasins == stationId,rawSpi[[1]]]
nsgammaTr[, Distribution := "Non-stationary SPI"]

allDist = rbindlist(list(gammaTr, nsgammaTr))
allDist[, Distribution := factor(Distribution, levels = c("Stationary SPI", "Non-stationary SPI"))]

print(paste0("Extracting data for station ",stationId))
# Obtain records for the specific station
# stationRecords <- copy(spiData[spiScales == spiScales & updatedBasins == stationId][,rawSpi[[1]]])

# Convert date to year month day format
allDist[, Date:= as.yearmon(Date)]


allDist[Status == 'Train' & Distribution == 'Non-stationary SPI', .(MEAN = mean(Rainfall), VAR = var(Rainfall))]
allDist[Status != 'Train' & Distribution == 'Non-stationary SPI', .(MEAN = mean(Rainfall), VAR = var(Rainfall))]


p2 <- ggplot(allDist[complete.cases(allDist)][Status %in% c("Train")]) +
  geom_line(aes(x = Date, y = `SPI-TVT`), size = 0.2, color = "red") +
  geom_line( aes(x = Date, y = `SPI-T`, size = 1), color = "black", size = 0.2) +
  ylab(paste0("SPI-",spiScales,""))+ theme_light() +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        legend.title=element_text(size=8),
        legend.text=element_text(size=7),
        legend.position = "top",
        strip.text.x = element_text(size = 6)) +
  facet_wrap(~Distribution, nrow = 2) +
  scale_x_yearmon()



ggsave(plot = p2, filename = paste0("../outputs/all_dist_station_",stationId,"_scale_",spiScales,".jpg"),
       units="cm", width=12, height=6, dpi = 1000)

