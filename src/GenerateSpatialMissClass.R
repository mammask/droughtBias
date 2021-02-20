GenerateSpatialMissClass = function(...){
  
  print("Loading all simulations")
  plotlist = list()
  scaleId = c(3,6,9,12,24)
  dataList = list()
  
  for (i in c(3,6,9,12,24)){
    results = readRDS(paste0("../outputs/bias measurement/training_bias_",i,".RDS"))
    setDT(results)
    
    discrepancies <- results[!is.na(p.value_T_TVT), rbindlist(transitions_T_TVT_year), by = basins]
    discrepancies <- discrepancies[complete.cases(discrepancies)]
    discrepancies[, `Miss-classification` := ifelse(`SPI-T-Class`==`SPI-TVT-Class`, "No", "Yes")]
    discrepancies <- discrepancies[, .(N = sum(N)), by = .(basins, `Miss-classification`)]
    discrepancies[, Percentage := round(100*N/sum(N),2), by = basins]
    discrepancies[, Scale := paste0("SPI(",i,")")]
    dataList = append(dataList, list(discrepancies))
    
  }
  
  overall = copy(rbindlist(dataList))
  overall[, OBJECTID := as.numeric(gsub("V","",basins))]
  overall <- merge(overall, koppen[,.(OBJECTID,MAINCLASS, Lat, Lon)], by = "OBJECTID")
  overall[, Scale := factor(Scale, levels = c("SPI(3)",
                                              "SPI(6)",
                                              "SPI(9)",
                                              "SPI(12)",
                                              "SPI(24)"))]
  
  setnames(overall, "Percentage", "% Miss-classifications")
  swedishMap <- map_data("world") %>% filter(region=="Sweden")
  print("Generating spatial plots")
  p = ggplot() + 
    geom_point(data = overall[`Miss-classification` == "Yes"], aes(x = Lon, y = Lat,
                                                                   color = `% Miss-classifications`),
               size = 0.3, stroke = 0.05) +
    scale_color_gradient(low="yellow", high="blue")+
    facet_wrap(~Scale, nrow = 1) + 
    geom_polygon(data = swedishMap, aes(x=long, y = lat, group = group), fill="#f8f1f1", alpha=0.1) +
    theme_bw() +
    theme(
      legend.key.width = unit(0.5, "cm"),
      legend.key.height = unit(0.5, "cm"),
      legend.position = "bottom",
      legend.text=element_text(size=5),
      legend.title=element_text(size=5),
      plot.title = element_text(size=5, hjust=0.5),
      strip.text = element_text(size=5),
      axis.text.x = element_text(size = 5),
      axis.text.y = element_text(size = 5),
      axis.title.x = element_text(size = 5),
      axis.title.y = element_text(size = 5)
    )+guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5),
             size = guide_legend(title.position="top", title.hjust = 0.5)) +
    xlab("Longitude") + ylab("Latitude")
  
  
  ggsave(paste0("../outputs/spatialMissclass.jpg"), plot = p, width = 16.5,
         height = 8, dpi = 1000, units = "cm")
  print("Spatial plots generated successfully")
  
}