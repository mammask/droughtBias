# GenerateBoxPlotsPerClGradient = function(...){
# Load configuration files
library(PupillometryR)

config = configr::read.config(file = "../config.cfg")

# Load multiple libraries
lapply(config$dependencies$libraries, require, character.only = TRUE)

# Load KÃ¶ppen Classification
koppen = fread("../data/mapper_sweden_Koppen.csv")
koppen = koppen[is.na(GRIDCODE), GRIDCODE:= 0]
koppen[GRIDCODE == 42, GRIDCODE := 43]
koppen[, Station := as.character(SUBIDnew)]

# Load Basins locations
basins = readOGR("c:/Users/komammas.EUROPE/Downloads/drive-download-20210222T094357Z-001/SHYPE2012_version_1_2_0_polygons_smallglomma_wgs84.dbf")


print("Loading all simulations")
bias3  <- readRDS("../outputs/bias measurement/NSGamma_training_bias_3.RDS")
bias6  <- readRDS("../outputs/bias measurement/NSGamma_training_bias_6.RDS")
bias9  <- readRDS("../outputs/bias measurement/NSGamma_training_bias_9.RDS")
bias12 <- readRDS("../outputs/bias measurement/NSGamma_training_bias_12.RDS")
bias24 <- readRDS("../outputs/bias measurement/NSGamma_training_bias_24.RDS")

setDT(bias3)
setDT(bias6)
setDT(bias9)
setDT(bias12)
setDT(bias24)
print("Simulations loaded successfully")

# Compute the mean absolute error per basin
maper_Metrics3 = bias3[, rbindlist(rawSpi)][Status %in% c("Train")]
maper_Metrics3 = maper_Metrics3[complete.cases(maper_Metrics3)]
maper_Metrics3 = maper_Metrics3[, .(Version = "SPI(3)",
                                    MAPE_T_TVT = MLmetrics::MAPE(y_true = `SPI-T`, y_pred = `SPI-TVT`),
                                    RMSE_T_TVT = MLmetrics::RMSE(y_true = `SPI-T`, y_pred = `SPI-TVT`),
                                    MAE_T_TVT = MLmetrics::MAE(y_true = `SPI-T`, y_pred = `SPI-TVT`)
), by = .(Station)
]


maper_Metrics6 = bias6[, rbindlist(rawSpi)][Status %in% c("Train")]
maper_Metrics6 = maper_Metrics6[complete.cases(maper_Metrics6)]
maper_Metrics6 = maper_Metrics6[, .(Version = "SPI(6)",
                                    MAPE_T_TVT = MLmetrics::MAPE(y_true = `SPI-T`, y_pred = `SPI-TVT`),
                                    RMSE_T_TVT = MLmetrics::RMSE(y_true = `SPI-T`, y_pred = `SPI-TVT`),
                                    MAE_T_TVT = MLmetrics::MAE(y_true = `SPI-T`, y_pred = `SPI-TVT`)
), by = .(Station)
]

maper_Metrics9 = bias9[, rbindlist(rawSpi)][Status %in% c("Train")]
maper_Metrics9 = maper_Metrics9[complete.cases(maper_Metrics9)]
maper_Metrics9 = maper_Metrics9[, .(Version = "SPI(9)",
                                    MAPE_T_TVT = MLmetrics::MAPE(y_true = `SPI-T`, y_pred = `SPI-TVT`),
                                    RMSE_T_TVT = MLmetrics::RMSE(y_true = `SPI-T`, y_pred = `SPI-TVT`),
                                    MAE_T_TVT = MLmetrics::MAE(y_true = `SPI-T`, y_pred = `SPI-TVT`)
), by = .(Station)
]


maper_Metrics12 = bias12[, rbindlist(rawSpi)][Status %in% c("Train")]
maper_Metrics12 = maper_Metrics12[complete.cases(maper_Metrics12)]
maper_Metrics12 = maper_Metrics12[, .(Version = "SPI(12)",
                                      MAPE_T_TVT = MLmetrics::MAPE(y_true = `SPI-T`, y_pred = `SPI-TVT`),
                                      RMSE_T_TVT = MLmetrics::RMSE(y_true = `SPI-T`, y_pred = `SPI-TVT`),
                                      MAE_T_TVT = MLmetrics::MAE(y_true = `SPI-T`, y_pred = `SPI-TVT`)
), by = .(Station)
]

maper_Metrics24 = bias24[, rbindlist(rawSpi)][Status %in% c("Train")]
maper_Metrics24 = maper_Metrics24[complete.cases(maper_Metrics24)]
maper_Metrics24 = maper_Metrics24[, .(Version = "SPI(24)",
                                      MAPE_T_TVT = MLmetrics::MAPE(y_true = `SPI-T`, y_pred = `SPI-TVT`),
                                      RMSE_T_TVT = MLmetrics::RMSE(y_true = `SPI-T`, y_pred = `SPI-TVT`),
                                      MAE_T_TVT = MLmetrics::MAE(y_true = `SPI-T`, y_pred = `SPI-TVT`)
), by = .(Station)
]


metrics = rbindlist(list(maper_Metrics3,maper_Metrics6,maper_Metrics9,
                         maper_Metrics12,maper_Metrics24))



basinsMap = st_as_sf(basins, quiet = TRUE)
basinsMap <- base::merge(basinsMap, metrics, by.x = "SUBIDnew",by.y = "Station")
basinsMap <- base::merge(basinsMap, koppen[,.(Station = Station,
                                              `Köppen-Geiger class` = factor(str_to_title(MAINCLASS),
                                                                             levels = c("Warm Temperate", "Snow", "Polar")),
                                              Lat= Lat,
                                              Lon = Lon
)
],
by.x = "SUBIDnew",by.y = "Station")


setDT(basinsMap)
basinsMap[, Version := factor(Version, levels = c("SPI(3)","SPI(6)",
                                                  "SPI(9)","SPI(12)",
                                                  "SPI(24)"))]

basinsMap[`Köppen-Geiger class` == "Warm Temperate", `Köppen-Geiger class` := "Warm \n Temperate"]

print("Computing box plots per climatic gradient and spi scale")
# p = ggplot(data = basinsMap[`Köppen-Geiger class` != ""],aes(x = `Köppen-Geiger class`, y = MAE_T_TVT)) +
#   geom_boxplot() + facet_wrap(~Version, nrow = 1) + 
#   xlab("Köppen-Geiger class") +
#   ylab("MAD (per basin)") +
#   theme_bw() + 
#   theme(axis.text.x = element_text(size = 5),
#         axis.text.y = element_text(size = 8),
#         axis.title.x = element_text(size = 8),
#         axis.title.y = element_text(size = 8),
#         strip.text.x = element_text(size = 8, colour = "black", angle = 0)
#   )


basinsMap[,`Köppen-Geiger class` := factor(`Köppen-Geiger class`, levels = c("Warm \n Temperate","Snow","Polar"))]
basinsMap[, Version := gsub("SPI","NSPI",Version)]
basinsMap[, Version := factor(Version, levels = c("NSPI(3)","NSPI(6)","NSPI(9)","NSPI(12)","NSPI(24)"))]

g = ggplot(data =  basinsMap[`Köppen-Geiger class` != ""], aes(x = `Köppen-Geiger class`, y = MAE_T_TVT, fill = `Köppen-Geiger class`)) +
  scale_fill_manual(values = c("#91091e","#111d5e","#23689b"))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  # geom_point(aes(y = MAE_T_TVT, color = `Köppen-Geiger class`), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5)+ facet_wrap(~Version, nrow = 1)+
  theme_bw() + 
  theme(
        legend.position="bottom",
        legend.text=element_text(size=5),
        legend.title= element_text(size=5),
        plot.title = element_text(size=8, hjust=0.5),
        strip.text = element_text(size=8),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        strip.text.x = element_text(size = 6)
  ) + 
  # scale_fill_manual(values = c("#91091e","#111d5e","#23689b"))+
    xlab("Köppen-Geiger class") +
    ylab("MAD (per basin)")


ggsave(filename = "../outputs/nspi_box_plot_mad-koppen.jpeg", plot = g,
       units="cm", width=12, height=7, dpi = 1000)

print("boxplots saved successfully")


# }
