library(data.table)
library(ggplot2)
library(stringr)
library(gridExtra)

# Load Köppen Classification
koppen = fread("../data/mapper_sweden_Koppen.csv")
koppen = koppen[is.na(GRIDCODE), GRIDCODE:= 0]
koppen[GRIDCODE == 42, GRIDCODE := 43]
koppen[, Station := as.character(SUBIDnew)]

getError = function(x,spiScale, distribution){
  
  x      <- x[,rbindlist(rawSpi)]
  x      <- x[Station %in% koppen[MAINCLASS != "", Station]]
  x      <- x[Status %in% c("Train")]
  x      <- x[complete.cases(x)]
  x      <- x[, .(Version = paste0("SPI(",spiScale,")"),
                  Distribution = str_to_sentence(distribution),
                  Scale = spiScale,
                  MAPE_T_TVT = MLmetrics::MAPE(y_true = `SPI-T`, y_pred = `SPI-TVT`),
                  RMSE_T_TVT = MLmetrics::RMSE(y_true = `SPI-T`, y_pred = `SPI-TVT`),
                  MAE_T_TVT = MLmetrics::MAE(y_true = `SPI-T`, y_pred = `SPI-TVT`)
  )]
  return(x)
}

allMetrics = list()
missClass = list()
for (spiScale in c(3,6,9,12,24)){
  for (distribution in c("Gamma","NSGamma")){
    
    tempDF = readRDS(paste0("../outputs/bias measurement/",distribution,"_training_bias_",spiScale,".RDS"))
    allMetrics = append(allMetrics, list(getError(x = copy(tempDF),spiScale, distribution)))
    
    transitions = tempDF[, rbindlist(transitions_T_TVT), by = .(spiScales,updatedBasins)]
    transitions[, Same := ifelse(`SPI-T-Class`==`SPI-TVT-Class`,T,F)]
    transitions = transitions[, .(N = sum(N), Misclassifications = sum(N[Same == FALSE])), by = spiScales]
    transitions[, Percentage := round(100*Misclassifications/N,1)]
    transitions[, Distribution := distribution]
    missClass = append(missClass, list(transitions))
    rm(transitions)
  }
}

overall = rbindlist(allMetrics)
overall <- melt.data.table(overall,id.vars = c("Version","Distribution","Scale"),variable.name = "Metric",value.name = "Value")
overall[,Metric := gsub("_","",gsub("T_TVT","",Metric))]
overall[Metric == "MAE", Metric:= "MAD"]
overall[Metric == "RMSE", Metric:= "RMSD"]
overall[Metric == "MAPE", Metric:= "MAPD"]
overall[Distribution == 'Nsgamma', Distribution := "SPI"]
overall = overall[Metric =="MAD"]
overall[, Distribution := ifelse(Distribution == 'Gamma','SPI','NSPI')]
overall[, Distribution := factor(Distribution, levels = c("SPI","NSPI"))]

p1 = ggplot(data = overall) + geom_line(aes(x = Scale, y = Value, color = Distribution), size = 0.5) +
  geom_point(aes(x = Scale, y = Value, color = Distribution), size = 0.5) +
  # facet_grid(~Metric) +
  theme_bw() + xlab("SPI Scale") + ylab("MAD") +
  theme(
    legend.key.width = unit(0.1, "cm"), 
    legend.key.height = unit(0.1, "cm"),
    legend.title = element_blank(),
    legend.position = "top",
    legend.text=element_text(size=6),
    plot.title = element_text(size=6, hjust=0.5),
    strip.text = element_text(size=6),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    axis.title.x = element_text(size = 6),
    axis.title.y = element_text(size = 6),
    axis.ticks.length.x = unit(0.2, "cm")
    
  ) +
  scale_x_continuous(breaks=c(3,6,9,12,24)) +
  scale_color_manual(values = c("#f55c47","#4aa96c","#564a4a", "#f1ca89"))


overallTransitions = rbindlist(missClass)
overallTransitions[, Distribution := ifelse(Distribution == 'Gamma','SPI','NSPI')]
overallTransitions[, Distribution := factor(Distribution, levels = c("SPI","NSPI"))]

p2 = ggplot(data = overallTransitions) + geom_line(aes(x = spiScales, y = Percentage, color = Distribution), size = 0.5) +
  geom_point(aes(x = spiScales, y = Percentage, color = Distribution), size = 0.5) +
  # facet_grid(~Metric) +
  theme_bw() + xlab("SPI Scale") + ylab("% Miss-classifications") +
  theme(
    legend.key.width = unit(0.1, "cm"), 
    legend.key.height = unit(0.1, "cm"),
    legend.title = element_blank(),
    legend.position = "top",
    legend.text=element_text(size=6),
    plot.title = element_text(size=6, hjust=0.5),
    strip.text = element_text(size=6),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    axis.title.x = element_text(size = 6),
    axis.title.y = element_text(size = 6),
    axis.ticks.length.x = unit(0.2, "cm")
  ) +
  scale_x_continuous(breaks=c(3,6,9,12,24)) +
  scale_color_manual(values = c("#f55c47","#4aa96c","#564a4a", "#f1ca89"))

p=grid.arrange(p1,p2, ncol=2)

ggsave(plot = p, filename = paste0("../outputs/overall_metrics.jpg"),
       units="cm", width=12, height=7, dpi = 1000)

# `bias3_gamma      <- bias3_gamma[,rbindlist(rawSpi)]
# bias3_gamma      <- bias3_gamma[Station %in% koppen[MAINCLASS != "", Station]]
# bias3_gamma      <- bias3_gamma[Status %in% c("Train")]
# bias3_gamma      <- bias3_gamma[complete.cases(bias3_gamma)]
# bias3_gamma  <- bias3_gamma[, .(Version = "SPI(3)",
#                                 Distribution = 'Gamma',
#                                 Scale = 3,
#                                 MAPE_T_TVT = MLmetrics::MAPE(y_true = `SPI-T`, y_pred = `SPI-TVT`),
#                                 RMSE_T_TVT = MLmetrics::RMSE(y_true = `SPI-T`, y_pred = `SPI-TVT`),
#                                 MAE_T_TVT = MLmetrics::MAE(y_true = `SPI-T`, y_pred = `SPI-TVT`)
#                                 )
# ]
# 
# 
# bias3_normal     <- readRDS("../outputs/bias measurement/Normal_training_bias_3.RDS")
# bias3_gamma      <- bias3_gamma[,rbindlist(rawSpi)]
# bias3_gamma      <- bias3_gamma[Station %in% koppen[MAINCLASS != "", Station]]
# bias3_gamma      <- bias3_gamma[Status %in% c("Train")]
# bias3_gamma      <- bias3_gamma[complete.cases(bias3_gamma)]
# bias3_gamma  <- bias3_gamma[, .(Version = "SPI(3)",
#                                 Distribution = 'Gamma',
#                                 Scale = 3,
#                                 MAPE_T_TVT = MLmetrics::MAPE(y_true = `SPI-T`, y_pred = `SPI-TVT`),
#                                 RMSE_T_TVT = MLmetrics::RMSE(y_true = `SPI-T`, y_pred = `SPI-TVT`),
#                                 MAE_T_TVT = MLmetrics::MAE(y_true = `SPI-T`, y_pred = `SPI-TVT`)
# )
# ]
# 
# 
# 
# 
# 
# 
# bias3_lognormal  <- readRDS("../outputs/bias measurement/LogNormal_training_bias_3.RDS")
# bias3_nsgamma    <- readRDS("../outputs/bias measurement/NSGamma_training_bias_3.RDS")
# 
# bias6_gamma      <- readRDS("../outputs/bias measurement/Gamma_training_bias_6.RDS")
# bias6_normal     <- readRDS("../outputs/bias measurement/Normal_training_bias_6.RDS")
# bias6_lognormal  <- readRDS("../outputs/bias measurement/LogNormal_training_bias_6.RDS")
# bias6_nsgamma    <- readRDS("../outputs/bias measurement/NSGamma_training_bias_6.RDS")
# 
# bias9_gamma      <- readRDS("../outputs/bias measurement/Gamma_training_bias_9.RDS")
# bias9_normal     <- readRDS("../outputs/bias measurement/Normal_training_bias_9.RDS")
# bias9_lognormal  <- readRDS("../outputs/bias measurement/LogNormal_training_bias_9.RDS")
# bias9_nsgamma    <- readRDS("../outputs/bias measurement/NSGamma_training_bias_9.RDS")
# 
# bias12_gamma      <- readRDS("../outputs/bias measurement/Gamma_training_bias_12.RDS")
# bias12_normal     <- readRDS("../outputs/bias measurement/Normal_training_bias_12.RDS")
# bias12_lognormal  <- readRDS("../outputs/bias measurement/LogNormal_training_bias_12.RDS")
# bias12_nsgamma    <- readRDS("../outputs/bias measurement/NSGamma_training_bias_12.RDS")
# 
# bias24_gamma      <- readRDS("../outputs/bias measurement/Gamma_training_bias_24.RDS")
# bias24_normal     <- readRDS("../outputs/bias measurement/Normal_training_bias_24.RDS")
# bias24_lognormal  <- readRDS("../outputs/bias measurement/LogNormal_training_bias_24.RDS")
# bias24_nsgamma    <- readRDS("../outputs/bias measurement/NSGamma_training_bias_24.RDS")
# 
# 
# 
# setDT(bias3)
# setDT(bias6)
# setDT(bias9)
# setDT(bias12)
# setDT(bias24)
# print("Simulations loaded successfully")
# 
# # Compute the mean absolute error per basin
# # maper_Metrics3 <- bias3[!is.na(p.value_T_TVT), rbindlist(rawSpi)]
# maper_Metrics3 = maper_Metrics3[Station %in% koppen[MAINCLASS != "", Station]]
# 
# maper_Metrics3 <- maper_Metrics3[Status %in% c("Train")]
# maper_Metrics3 <-maper_Metrics3[complete.cases(maper_Metrics3)]
# 
# metrics3_T_TVT  <- maper_Metrics3[, .(Version = "SPI(3)",
#                                       Scale = 3,
#                                       MAPE_T_TVT = MLmetrics::MAPE(y_true = `SPI-T`, y_pred = `SPI-TVT`),
#                                       RMSE_T_TVT = MLmetrics::RMSE(y_true = `SPI-T`, y_pred = `SPI-TVT`),
#                                       MAE_T_TVT = MLmetrics::MAE(y_true = `SPI-T`, y_pred = `SPI-TVT`))
# ]
# 
# 
# maper_Metrics6 <- bias6[!is.na(p.value_T_TVT), rbindlist(rawSpi)]
# maper_Metrics6 = maper_Metrics6[Station %in% koppen[MAINCLASS != "", Station]]
# 
# maper_Metrics6 <-maper_Metrics6[complete.cases(maper_Metrics6)]
# 
# metrics6_T_TVT <- maper_Metrics6[, .(Version = "SPI(6)",
#                                      Scale = 6,
#                                      MAPE_T_TVT = MLmetrics::MAPE(y_true = `SPI-T`, y_pred = `SPI-TVT`),
#                                      RMSE_T_TVT = MLmetrics::RMSE(y_true = `SPI-T`, y_pred = `SPI-TVT`),
#                                      MAE_T_TVT = MLmetrics::MAE(y_true = `SPI-T`, y_pred = `SPI-TVT`)
# )
# ]
# 
# maper_Metrics9 <- bias9[!is.na(p.value_T_TVT), rbindlist(rawSpi)]
# maper_Metrics9 = maper_Metrics9[Station %in% koppen[MAINCLASS != "", Station]]
# 
# maper_Metrics9 <- maper_Metrics9[Status %in% c("Train")]
# maper_Metrics9 <-maper_Metrics9[complete.cases(maper_Metrics9)]
# 
# metrics9_T_TVT  <- maper_Metrics9[, .(Version = "SPI(9)",
#                                       Scale = 9,
#                                       MAPE_T_TVT = MLmetrics::MAPE(y_true = `SPI-T`, y_pred = `SPI-TVT`),
#                                       RMSE_T_TVT = MLmetrics::RMSE(y_true = `SPI-T`, y_pred = `SPI-TVT`),
#                                       MAE_T_TVT = MLmetrics::MAE(y_true = `SPI-T`, y_pred = `SPI-TVT`)
# )
# ]
# 
# maper_Metrics12 <- bias12[!is.na(p.value_T_TVT), rbindlist(rawSpi)]
# maper_Metrics12 = maper_Metrics12[Station %in% koppen[MAINCLASS != "", Station]]
# 
# maper_Metrics12 <- maper_Metrics12[Status %in% c("Train")]
# maper_Metrics12 <-maper_Metrics12[complete.cases(maper_Metrics12)]
# 
# metrics12_T_TVT  <- maper_Metrics12[, .(Version = "SPI(12)",
#                                         Scale = 12,
#                                         MAPE_T_TVT = MLmetrics::MAPE(y_true = `SPI-T`, y_pred = `SPI-TVT`),
#                                         RMSE_T_TVT = MLmetrics::RMSE(y_true = `SPI-T`, y_pred = `SPI-TVT`),
#                                         MAE_T_TVT = MLmetrics::MAE(y_true = `SPI-T`, y_pred = `SPI-TVT`)
# )
# ]
# 
# 
# maper_Metrics24 <- bias24[!is.na(p.value_T_TVT), rbindlist(rawSpi)]
# maper_Metrics24 = maper_Metrics24[Station %in% koppen[MAINCLASS != "", Station]]
# 
# maper_Metrics24 <- maper_Metrics24[Status %in% c("Train")]
# maper_Metrics24 <-maper_Metrics24[complete.cases(maper_Metrics24)]
# 
# metrics24_T_TVT <- maper_Metrics24[, .(Version = "SPI(24)",
#                                        Scale = 24,
#                                        MAPE_T_TVT = MLmetrics::MAPE(y_true = `SPI-T`, y_pred = `SPI-TVT`),
#                                        RMSE_T_TVT = MLmetrics::RMSE(y_true = `SPI-T`, y_pred = `SPI-TVT`),
#                                        MAE_T_TVT = MLmetrics::MAE(y_true = `SPI-T`, y_pred = `SPI-TVT`)
# )
# ]
# 
# 
# overall <- rbindlist(list(metrics3_T_TVT,
#                           metrics6_T_TVT,
#                           metrics9_T_TVT,
#                           metrics12_T_TVT,
#                           metrics24_T_TVT))
# 
# 
# 
# 
# 
# 
# 
# 
# filesToLoad <- list.files("../outputs/", pattern = ".csv")
# comparisons <- list()
# for (i in filesToLoad){
#   comparisons <- append(comparisons, list(fread(paste0("../outputs/",i))))
# }
# # 
# comparisons <- rbindlist(comparisons)
# comparisons[, sum(Percentage), by = Scale]
# comparisons = comparisons[, .(Metric = "%-miss-classifications", Value = sum(Percentage)), by = .(Scale)]
# 
# overall <- melt.data.table(overall,id.vars = c("Version","Scale"),variable.name = "Metric",value.name = "Value")
# 
# overall[,Metric := gsub("_","",gsub("T_TVT","",Metric))]
# overall[Metric == "MAE", Metric:= "MAD"]
# overall[Metric == "RMSE", Metric:= "RMSD"]
# overall[Metric == "MAPE", Metric:= "MAPD"]
# 
# 
# # setnames(comparisons,"MAINCLASS","Class")
# # comparisons <- comparisons[Class == "All"][order(Scale)]
# # comparisons <- merge(comparisons[,.(Scale, `% Basins T_TVT`,`% Records T_TVT`)], 
# #                      overall[,.(Scale,MAPE_T_TVT,RMSE_T_TVT,MAE_T_TVT)], by = "Scale"
# # )
# 
# 
# 
# pp3 <- ggplot(data = overall[Metric == "MAD"], aes(x = Scale, y = Value, group = Metric)) + 
#   geom_point(aes(x = Scale, y = Value), size = 0.5) + 
#   geom_line(aes(linetype = Metric),size = 0.5) + 
#   scale_linetype_manual(name = "Metric", values = c(1)) +
#   theme_bw() + xlab("SPI Scale") + ylab("Metric") +
#   theme(
#     legend.key.width = unit(0.1, "cm"), 
#     legend.key.height = unit(0.1, "cm"),
#     legend.title = element_blank(),
#     legend.position = "top",
#     legend.text=element_text(size=6),
#     plot.title = element_text(size=6, hjust=0.5),
#     strip.text = element_text(size=6),
#     axis.text.x = element_text(size = 6),
#     axis.text.y = element_text(size = 6),
#     axis.title.x = element_text(size = 6),
#     axis.title.y = element_text(size = 6)
#   ) +
#   scale_x_continuous(breaks=c(3,6,9,12,24))
# 
# comparisons[Metric == "% Records ", Metric := "% Miss-classification"]
# 
# pp4 <- ggplot(data = comparisons, aes(x = Scale, y = Value, group = Metric)) + 
#   geom_point(aes(x = Scale, y = Value), size = 0.5) + 
#   geom_line(aes(linetype = Metric),size = 0.5) + 
#   scale_linetype_manual(name = "Metric", values = c(1)) +
#   theme_bw() + xlab("SPI Scale") + ylab("Metric") +
#   theme(
#     legend.key.width = unit(0.1, "cm"), 
#     legend.key.height = unit(0.1, "cm"),
#     legend.title = element_blank(),
#     legend.position = "top",
#     legend.text=element_text(size=6),
#     plot.title = element_text(size=6, hjust=0.5),
#     strip.text = element_text(size=6),
#     axis.text.x = element_text(size = 6),
#     axis.text.y = element_text(size = 6),
#     axis.title.x = element_text(size = 6),
#     axis.title.y = element_text(size = 6)
#   ) +
#   scale_x_continuous(breaks=c(3,6,9,12,24))
# 
# pall = gridExtra::grid.arrange(pp3, pp4, nrow = 1)
# ggsave(plot = pall, filename = paste0("../outputs/overall_metrics.jpg"),
#        units="cm", width=14.5, height=4, dpi = 1000)
# print("Bias plota saved successfully")
