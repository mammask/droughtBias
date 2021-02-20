stationRawData = function(spiData, spiScales, stationId = "V3216"){
  
  print(paste0("Extracting data for station ",stationId))
  # Obtain records for the specific station
  stationRecords <- copy(spiData[spiScales == spiScales & basins == stationId][,rawSpi[[1]]])
  
  # Convert date to year month day format
  stationRecords[, Date:= as.Date(Date)]
  print("Data extracted successfully")
  
  print("Generating plot with raw data")
  # Plot monthly rainfall records
  p1 <- ggplot(stationRecords) + geom_line(aes(x = Date, y = Rainfall),size = 0.2) +
    theme_light() +
    theme(axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          legend.title=element_text(size=8),
          legend.text=element_text(size=8),
          legend.position = "top"
    ) + 
    geom_vline(xintercept = stationRecords[Status == "Validation",min(Date)], color = "red",linetype = "dashed") +
    xlab("Date") + 
    annotate(geom="text", x=stationRecords[Status == "Test",mean(Date)],y=265, label="Validation & Test sets",color="red", size = 2)
  
  # Plot drought index
  p2 <- ggplot(stationRecords[complete.cases(stationRecords)][Status %in% c("Train")]) +
    geom_line(aes(x = Date, y = `SPI-TVT`), size = 0.2, color = "red") +
    geom_line( aes(x = Date, y = `SPI-T`, size = 1), color = "black", size = 0.2) +
    ylab(paste0("SPI(",spiScales,")"))+ theme_light() +
    theme(axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          legend.title=element_text(size=8),
          legend.text=element_text(size=8),
          legend.position = "top") +
    scale_x_date(limits = c(stationRecords[,min(Date)],stationRecords[,max(Date)]))
  
  # Create magnifier plot
  zoomtheme <- theme(legend.position="none", axis.line=element_blank(),axis.text.x=element_blank(),
                     axis.text.y=element_blank(),axis.ticks=element_blank(),
                     axis.title.x=element_blank(),axis.title.y=element_blank(),
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     panel.background = element_rect(color='red', fill="white"),
                     plot.margin = unit(c(0,0,-6,-6),"mm"))
  
  p3 <- ggplot(stationRecords[complete.cases(stationRecords)][Status %in% c("Train")]) +
    geom_line(aes(x = Date, y = `SPI-TVT`), size = 0.2, color = "red") +
    geom_line( aes(x = Date, y = `SPI-T`, size = 1), color = "black", size = 0.2) +
    coord_cartesian(xlim=c(stationRecords[Status %in% c("Train"),Date[50]],
                           stationRecords[Status %in% c("Train"),Date[80]]),
                    ylim=c(-1.5,1)) + 
    ylab(paste0("SPI(",spiScales,")"))+ theme_light() +
    theme(axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          legend.title=element_text(size=8),
          legend.text=element_text(size=8),
          legend.position = "top") +  
    zoomtheme
  
  # Create a grob object
  g <- ggplotGrob(p3)
  
  # Combine spi plot and zoom plot
  p4 <- p2 + annotation_custom(grob = g, 
                               xmin = stationRecords[,Date[40]], xmax = stationRecords[,Date[80]],
                               ymin = -1.7,
                               ymax = -1.75
  )
  
  pall = gridExtra::grid.arrange(p1,p4)
  ggsave(plot = pall, filename = paste0("../outputs/station_",stationId,"_scale_",spiScales,".jpg"),
         units="cm", width=14.5, height=6, dpi = 1000)
  print("Plot with raw data saved successfully")
}