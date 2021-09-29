GenerateSpatialMissClass = function(...){
  
  print("Loading all simulations")
  plotlist = list()
  scaleId = c(3,6,9,12,24)
  dataList = list()
  
  for (i in c(3,6,9,12,24)){
    results = readRDS(paste0("../outputs/bias measurement/",config[['distribution']],"_training_bias_",i,".RDS"))
    setDT(results)
    
    discrepancies <- results[, rbindlist(transitions_T_TVT_year), by = updatedBasins ]
    discrepancies <- discrepancies[complete.cases(discrepancies)]
    discrepancies[, `Miss-classification` := ifelse(`SPI-T-Class`==`SPI-TVT-Class`, "No", "Yes")]
    discrepancies <- discrepancies[, .(N = sum(N)), by = .(updatedBasins , `Miss-classification`)]
    discrepancies[, Percentage := round(100*N/sum(N),2), by = updatedBasins ]
    discrepancies[, Scale := paste0("SPI(",i,")")]
    dataList = append(dataList, list(discrepancies))
    
  }
  
  overall = copy(rbindlist(dataList))
  overall[, Scale := gsub("SPI","NSPI",Scale)]
  
  basinsMap = st_as_sf(basins, quiet = TRUE)
  basinsMap <- base::merge(basinsMap, overall[`Miss-classification` == "Yes"], by.x = "SUBIDnew",by.y = "updatedBasins")
  basinsMap <- base::merge(basinsMap, koppen[MAINCLASS != "",.(Station = Station,
                                                               `Köppen-Geiger` = factor(str_to_title(MAINCLASS),
                                                                                        levels = c("Warm Temperate", "Snow", "Polar")),
                                                               Lat= Lat,
                                                               Lon = Lon
  )
  ],
  by.x = "SUBIDnew",by.y = "Station")
  
  
  setnames(basinsMap, "Percentage", "% class transitions")
  basinsMap[["Scale"]] = factor(basinsMap[["Scale"]], levels = c("NSPI(3)","NSPI(6)","NSPI(9)",
                                                                 "NSPI(12)","NSPI(24)")
                                )
  
    
  p_missclass = tm_shape(basinsMap) + 
    tm_fill("% class transitions",palette="Reds",legend.is.portrait=FALSE) + 
    tm_layout(legend.outside = TRUE,
              legend.outside.position = "bottom",
              legend.title.size = 0.6,
              legend.text.size = 0.5,
              legend.stack = "horizontal",
              legend.position = c(0.4, 0.4),
              legend.outside.size = .25
    ) + tm_facets(by = "Scale", nrow = 1)
  
  tmap_save(p_missclass, filename = "../outputs/spatialMissclass.jpg", width = 12, height = 8, dpi = 2000,
            units = "cm") 
  
  print("Spatial plots generated successfully")
  
}