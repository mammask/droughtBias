GenerateDescriptivesMaps = function(spiData, koppen, station, basins){
  
  # Compute Monthly Rainfall
  print("Computing monthly precipitation")
  monthlyRainfall = copy(spiData[!is.na(p.value_T_TVT), rbindlist(rawSpi)])
  monthlyRainfall = monthlyRainfall[Status == "Train"]
  monthlyRainfall = monthlyRainfall[complete.cases(monthlyRainfall)
  ][, .(`Mean Monthly Precipitation` = mean(Rainfall)),
    by = Station
  ]
  
  # Obtain the location per station
  monthlyRainfall <- base::merge(basins[, .(Station, Lat, Lon)],
                                 monthlyRainfall, by = 'Station', sort = F
  )
  
  # Obtain the Koppen classifications
  monthlyRainfall <- base::merge(monthlyRainfall, koppen[,.(Station,MAINCLASS)],
                                 by = "Station")
  
  setnames(monthlyRainfall, "MAINCLASS", "Köppen-Geiger")
  monthlyRainfall[,`Köppen-Geiger` := str_to_title(`Köppen-Geiger`)]
  monthlyRainfall[, `Köppen-Geiger` := factor(`Köppen-Geiger`, levels = c("Warm Temperate", "Snow", "Polar"))]
  print("Monthly precipitation computed successfully")
  
  print("Generating spatial distribution of precipitation")
  swedishMap <- map_data("world") %>% filter(region=="Sweden")
  p_rain <- ggplot() +
    geom_polygon(data = swedishMap, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
    geom_point(data = monthlyRainfall,
               aes(x = Lon, y = Lat, color = `Mean Monthly Precipitation`),
               size = 0.3, stroke = 0.1) +
    scale_color_gradient()+
    coord_map() +
    theme_bw() +
    scale_fill_manual('Mean Monthly Rainfall (mm)') +
    theme(
      legend.key.width = unit(0.5, "cm"), 
      legend.key.height = unit(0.3, "cm"),
      legend.position = "bottom",
      legend.text=element_text(size=5),
      legend.title=element_text(size=5),
      plot.title = element_text(size=5, hjust=0.5),
      strip.text = element_text(size=5),
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8),
      axis.title.x = element_text(size = 8),
      axis.title.y = element_text(size = 8)
    )+
    xlab("Longitude") +
    ylab("Latitude")+
    guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5),
           size = guide_legend(title.position="top",
                               title.hjust = 0.5)
    )
  print("Spatial distribution of precipitation computed successfully")
  
  print("Computing Koppen Geiger classification map")
  p_koppen <- ggplot() +
    geom_polygon(data = swedishMap, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
    geom_point(data = monthlyRainfall[`Köppen-Geiger` != ""],
               aes(x = Lon, y = Lat, color = `Köppen-Geiger`), size = 0.3, stroke = 0.1) +
    coord_map() +
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
    ) + xlab("Longitude") + ylab("Latitude")+
    scale_shape(
      guide = guide_legend(
        title.position = "top",
        title.hjust = 0.5
      )
    )+
    scale_color_manual(
      values = c("#91091e","#111d5e","#23689b"),
      name = "Köppen-Geiger",
      guide = guide_legend(
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5,
        override.aes = list(size = 3)
      )
    )
  print("Koppen-Geiger classification map computed successfully")
  
  print("Computing graph with station locations")
  monthlyRainfall[, Description := "Station location"]
  p_loc <- ggplot() +
    geom_polygon(data = swedishMap, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
    geom_point(data = monthlyRainfall,
               aes(x = Lon, y = Lat, color = Description), size = 0.1, stroke = 0.1) +
    coord_map() +
    theme_bw() +
    scale_fill_manual('Mean Monthly Rainfall (mm)') +
    theme(
      legend.key.width = unit(0.2, "cm"), 
      legend.key.height = unit(0.3, "cm"),
      legend.title = element_blank(),
      legend.text=element_text(size=5),
      legend.position = "bottom",
      plot.title = element_text(size=8, hjust=0.5),
      strip.text = element_text(size=8),
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8),
      axis.title.x = element_text(size = 8),
      axis.title.y = element_text(size = 8)
    ) + xlab("Longitude") + ylab("Latitude") +
    scale_colour_manual(values = "black") +
    guides(color = guide_legend(override.aes = list(size = 3) ) )
  print("Graph with station locations computed successfully")
  
  print("Saving maps")
  combined =  p_loc + p_koppen + p_rain  & theme(legend.position = "bottom")
  combined = combined + plot_layout(guides = "collect", nrow = 1)
  ggsave(plot = combined, filename = paste0("../outputs/Basins_Descriptives.jpg"),
         units="cm", width=14.5, height=8, dpi = 1000)
  print("Maps saved successfully")
}