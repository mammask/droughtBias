GenerateBoxPlotsPerClGradient = function(...){
  
  print("Loading all simulations")
  bias3  <- readRDS("../outputs/bias measurement/training_bias_3.RDS")
  bias6  <- readRDS("../outputs/bias measurement/training_bias_6.RDS")
  bias9  <- readRDS("../outputs/bias measurement/training_bias_9.RDS")
  bias12 <- readRDS("../outputs/bias measurement/training_bias_12.RDS")
  bias24 <- readRDS("../outputs/bias measurement/training_bias_24.RDS")
  
  setDT(bias3)
  setDT(bias6)
  setDT(bias9)
  setDT(bias12)
  setDT(bias24)
  print("Simulations loaded successfully")
  
  # Compute the mean absolute error per basin
  maper_Metrics3 = bias3[!is.na(p.value_T_TVT), rbindlist(rawSpi)][Status %in% c("Train")]
  maper_Metrics3 = maper_Metrics3[complete.cases(maper_Metrics3)]
  maper_Metrics3 = maper_Metrics3[, .(Version = "SPI(3)",
                                      MAPE_T_TVT = MLmetrics::MAPE(y_true = `SPI-T`, y_pred = `SPI-TVT`),
                                      RMSE_T_TVT = MLmetrics::RMSE(y_true = `SPI-T`, y_pred = `SPI-TVT`),
                                      MAE_T_TVT = MLmetrics::MAE(y_true = `SPI-T`, y_pred = `SPI-TVT`)
  ), by = .(Station)
  ]
  
  
  maper_Metrics6 = bias6[!is.na(p.value_T_TVT), rbindlist(rawSpi)][Status %in% c("Train")]
  maper_Metrics6 = maper_Metrics6[complete.cases(maper_Metrics6)]
  maper_Metrics6 = maper_Metrics6[, .(Version = "SPI(6)",
                                      MAPE_T_TVT = MLmetrics::MAPE(y_true = `SPI-T`, y_pred = `SPI-TVT`),
                                      RMSE_T_TVT = MLmetrics::RMSE(y_true = `SPI-T`, y_pred = `SPI-TVT`),
                                      MAE_T_TVT = MLmetrics::MAE(y_true = `SPI-T`, y_pred = `SPI-TVT`)
  ), by = .(Station)
  ]
  
  maper_Metrics9 = bias9[!is.na(p.value_T_TVT), rbindlist(rawSpi)][Status %in% c("Train")]
  maper_Metrics9 = maper_Metrics9[complete.cases(maper_Metrics9)]
  maper_Metrics9 = maper_Metrics9[, .(Version = "SPI(9)",
                                      MAPE_T_TVT = MLmetrics::MAPE(y_true = `SPI-T`, y_pred = `SPI-TVT`),
                                      RMSE_T_TVT = MLmetrics::RMSE(y_true = `SPI-T`, y_pred = `SPI-TVT`),
                                      MAE_T_TVT = MLmetrics::MAE(y_true = `SPI-T`, y_pred = `SPI-TVT`)
  ), by = .(Station)
  ]
  
  
  maper_Metrics12 = bias12[!is.na(p.value_T_TVT), rbindlist(rawSpi)][Status %in% c("Train")]
  maper_Metrics12 = maper_Metrics12[complete.cases(maper_Metrics12)]
  maper_Metrics12 = maper_Metrics12[, .(Version = "SPI(12)",
                                        MAPE_T_TVT = MLmetrics::MAPE(y_true = `SPI-T`, y_pred = `SPI-TVT`),
                                        RMSE_T_TVT = MLmetrics::RMSE(y_true = `SPI-T`, y_pred = `SPI-TVT`),
                                        MAE_T_TVT = MLmetrics::MAE(y_true = `SPI-T`, y_pred = `SPI-TVT`)
  ), by = .(Station)
  ]
  
  maper_Metrics24 = bias24[!is.na(p.value_T_TVT), rbindlist(rawSpi)][Status %in% c("Train")]
  maper_Metrics24 = maper_Metrics24[complete.cases(maper_Metrics24)]
  maper_Metrics24 = maper_Metrics24[, .(Version = "SPI(24)",
                                        MAPE_T_TVT = MLmetrics::MAPE(y_true = `SPI-T`, y_pred = `SPI-TVT`),
                                        RMSE_T_TVT = MLmetrics::RMSE(y_true = `SPI-T`, y_pred = `SPI-TVT`),
                                        MAE_T_TVT = MLmetrics::MAE(y_true = `SPI-T`, y_pred = `SPI-TVT`)
  ), by = .(Station)
  ]
  
  
  metrics = rbindlist(list(maper_Metrics3,maper_Metrics6,maper_Metrics9,
                           maper_Metrics12,maper_Metrics24))
  
  metrics_T_TVT_STATION = base::merge(metrics,
                                      koppen[,.(Station,MAINCLASS)],
                                      by = "Station"
  )
  
  metrics_T_TVT_STATION[, Version := factor(Version, levels = c("SPI(3)","SPI(6)",
                                                                "SPI(9)","SPI(12)",
                                                                "SPI(24)"))]
  metrics_T_TVT_STATION[, MAINCLASS := str_to_title(MAINCLASS)]
  metrics_T_TVT_STATION[MAINCLASS == "Warm Temperate", MAINCLASS := "Warm \n Temperate"]
  
  print("Computing box plots per climatic gradient and spi scale")
  p = ggplot(data = metrics_T_TVT_STATION[MAINCLASS != ""],aes(x = MAINCLASS, y = MAE_T_TVT)) +
    geom_boxplot() + facet_wrap(~Version, nrow = 1) + 
    xlab("Koppen Class") +
    ylab("MAD (per basin)") +
    theme_bw() + 
    theme(axis.text.x = element_text(size = 5),
          axis.text.y = element_text(size = 8),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          strip.text.x = element_text(size = 8, colour = "black", angle = 0)
    )
  
  ggsave(filename = "../outputs/box_plot_mad-koppen.jpeg", plot = p,
         units="cm", width=14.5, height=7, dpi = 1000)
  
  print("boxplots saved successfully")
  
  
}
