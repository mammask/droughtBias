CompareDensities = function(spiData, spiScales, stationId){
  
  print("Generating accumulated densities")
  records = copy(spiData[basins == stationId, rawSpi[[1]]])
  
  records[, Year := year(Date)]
  records[, Month := month(Date)]
  records[, Date :=as.yearmon(Date)]
  
  
  accumm_prcp_tvt = rowSums(embed(records[,Rainfall],spiScales),na.rm=FALSE)
  prcp_TVT = data.table("Year"      = records$Year[spiScales:nrow(records)],
                        "Month"     = records$Month[spiScales:nrow(records)],
                        "AccumPrcp" = accumm_prcp_tvt)
  
  prcp_TVT[, MonthAbb :=month.abb[Month]]
  rm(records)
  
  records_train = copy(spiData[basins == stationId, rawSpi[[1]]])[Status == "Train"]
  records_train[, Year := year(Date)]
  records_train[, Month := month(Date)]
  records_train[, Date :=as.yearmon(Date)]
  
  
  accumm_prcp_t = rowSums(embed(records_train[,Rainfall],spiScales),na.rm=FALSE)
  prcp_T = data.table("Year"         = records_train$Year[spiScales:nrow(records_train)],
                      "Month"      = records_train$Month[spiScales:nrow(records_train)],
                      "AccumPrcp"  = accumm_prcp_t)
  
  prcp_T[, MonthAbb :=month.abb[Month]]
  rm(records_train)
  print("Accumulated densities computed successfully")
  
  print("Generating density plots")
  plotList = list()
  for (i in 1:12){
    plotList[[i]] = ggplot() + 
      stat_density(data = prcp_T[Month == i], aes(x=AccumPrcp), geom = "line", position = "identity", bw = 100) +
      stat_density(data = prcp_TVT[Month == i], aes(x=AccumPrcp), geom = "line", position = "identity", color = "red", bw = 100) + 
      xlab(paste0("Accumulated precipitation")) + ylab("Density") +
      scale_fill_npg() +
      theme_bw() +
      ggtitle(paste0(month.abb[i]))+
      theme(
        legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.5, "cm"),
        legend.position = "top",
        legend.text=element_text(size=6),
        plot.title = element_text(size=6, hjust=0.5),
        strip.text = element_text(size=6),
        axis.text.y = element_text(size = 6),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 6),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6)
      ) 
    
  }
  
  combined <- plotList[[1]] + plotList[[2]] + plotList[[3]] + plotList[[4]] + plotList[[5]] +
    plotList[[6]] + plotList[[7]] + plotList[[8]] + plotList[[9]] + plotList[[10]] +
    plotList[[11]] + plotList[[12]] & theme(legend.position = "bottom")
  combined <- combined + plot_layout(guides = "collect", ncol =4)
  
  ggsave(filename = paste0("../outputs/compareAccumDens_station_",stationId,"_scale_",spiScales,".jpg"),
         plot = combined,
         dpi=1000,
         width = 14.5,
         height = 9,
         units = "cm")
  print("Density plota saved successfully")
}