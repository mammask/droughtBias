BiasTrendGrowth = function(spiData, spiScales){
  
  monthlyRainfall = copy(spiData[!is.na(p.value_T_TVT), rbindlist(rawSpi)])
  growth = monthlyRainfall[, .(MeanTrain = mean(Rainfall[Status == "Train"]),
                               MeanValidTest = mean(Rainfall[Status != "Train"])
  ), by = Station]
  growth[, Growth := round(100*(MeanValidTest - MeanTrain)/MeanTrain,2)]
  
  
  transitions = copy(spiData[!is.na(p.value_T_TVT), rbindlist(transitions_T_TVT), by = .(updatedBasins)])[`SPI-T-Class` != `SPI-TVT-Class`]
  transitions = transitions[, .(MissClassifications = sum(Percentage)), by = .(Station = updatedBasins)]
  total = base::merge(growth, transitions, by = "Station")
  
  total = base::merge(total, koppen[MAINCLASS != "",.(Station = Station,
                                                      `Köppen-Geiger` = factor(str_to_title(MAINCLASS),
                                                                               levels = c("Warm Temperate", "Snow", "Polar")),
                                                      Lat= Lat,
                                                      Lon = Lon
  )
  ],
  by = "Station")
  
  p = ggplot(data = total, aes(x = Growth, y = MissClassifications)) + geom_point(aes(color = `Köppen-Geiger`), size = 0.3, stroke = 0.1) +
    xlab("% growth in monthly precipitation (training vs. validation, test sets)") + ylab("% miss-classifications") +
    geom_smooth(method="loess", se=TRUE, fullrange=FALSE, level=0.95, size = 0.5) +
    geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 0.5) +
    facet_wrap(~`Köppen-Geiger`) +
    stat_cor(method = "pearson", label.x = 3, label.y = 40, size = 2)+
    theme_bw() +
    theme(
      legend.key.width = unit(0.2, "cm"), 
      legend.key.height = unit(0.3, "cm"),
      legend.position = "bottom",
      legend.text=element_text(size=5),
      legend.title= element_text(size=5),
      plot.title = element_text(size=8, hjust=0.5),
      strip.text = element_text(size=8),
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8),
      axis.title.x = element_text(size = 8),
      axis.title.y = element_text(size = 8)
    ) +
    scale_shape(
      guide = guide_legend(
        title.position = "top",
        title.hjust = 0.5
      )
    )+
    scale_color_manual(
      values = c("#91091e","#111d5e","#23689b"),
      name = "Köppen-Geiger class",
      guide = guide_legend(
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5,
        override.aes = list(size = 3)
      )
    )
  
  ggsave(p, filename = paste0("../outputs/biasTrendGrowth_scale",spiScales,".jpg"), width = 14.5, height = 8, dpi = 2000,
            units = "cm") 
  
}