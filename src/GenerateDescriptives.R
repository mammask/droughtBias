GenerateDescriptivesMaps = function(spiData, koppen, station, basins){
  
  
  # Compute Monthly Rainfall
  print("Computing monthly precipitation")
  monthlyRainfall = copy(spiData[!is.na(p.value_T_TVT), rbindlist(rawSpi)])
  monthlyRainfall = monthlyRainfall[Status == "Train"]
  monthlyRainfall = monthlyRainfall[complete.cases(monthlyRainfall)
  ][, .(`Mean Precipitation` = mean(Rainfall)),
    by = Station
  ]
  
  
  basinsMap = st_as_sf(basins, quiet = TRUE)
  basinsMap <- base::merge(basinsMap, monthlyRainfall, by.x = "SUBIDnew",by.y = "Station")
  basinsMap <- base::merge(basinsMap, koppen[MAINCLASS != "",.(Station = Station,
                                                `Köppen-Geiger class` = factor(str_to_title(MAINCLASS),
                                                                         levels = c("Warm Temperate", "Snow", "Polar")),
                                                Lat= Lat,
                                                Lon = Lon
  )
  ],
  by.x = "SUBIDnew",by.y = "Station")
  
  print("Monthly precipitation computed successfully")
  
  print("Generating spatial distribution of precipitation")
  basinsMap[["Station"]] = "Location"
  
  p_loc = tm_shape(basinsMap) +
    tm_polygons(title = "Stations Location",
                border.col = "black",
                lwd=0.3
                # border.alpha = 0.5
    ) +
    tm_fill("Station") + 
    tm_layout(bg.color = "transparent")+
    tm_compass(type = "4star", 
               size = 2, # set size of the compass
               color.dark = "gray60", # color the compass
               text.color = "gray60", # color the text of the compass
               position = c(0.7, 0.05))   # set position of the compass
  
  p_rain = tm_shape(basinsMap) + 
    tm_fill("Mean Precipitation",palette="Blues") +
    tm_layout(legend.outside = FALSE,
              legend.outside.position = "right",
              legend.title.size = 0.6,
              legend.text.size = 0.4
    )
  
  p_koppen = tm_shape(basinsMap[which(!is.na(basinsMap$`Köppen-Geiger`)),]) + 
    tm_fill("Köppen-Geiger class",
            palette = c("#91091e","#111d5e","#23689b")) +
    tm_layout(legend.outside = FALSE,
              legend.outside.position = "right",
              legend.title.size = 0.6,
              legend.text.size = 0.4
    )
  
  pAll = tmap_arrange(p_loc, p_koppen,p_rain, ncol = 3)
  
  print("Saving maps")
  tmap_save(pAll, filename = "../outputs/Basins_Descriptives.jpg", width = 14.5, height = 8, dpi = 2000,
            units = "cm") 
  print("Maps saved successfully")
}