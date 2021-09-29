GenerateMissclassifications = function(spiData, spiScales){
  
  print("Generating miss-classifications")
  transitions = copy(spiData[, rbindlist(transitions_T_TVT), by = .(updatedBasins)])
  transitions = transitions[updatedBasins %in% koppen[MAINCLASS != "", Station]]
  
  transitions = transitions[, .(Transitions = sum(N)), by = .(`SPI-T-Class`,`SPI-TVT-Class`)]
  transitions[, Percentage := round(100*Transitions/sum(Transitions),2)]
  transitions = transitions[`SPI-T-Class`!=`SPI-TVT-Class`]
  
  transitions[, `SPI-T-Class` := factor(`SPI-T-Class`, levels = c("Extremely Wet","Very Wet","Moderately Wet","Near Normal",
                                                                  "Moderately Dry", "Very Dry", "Extremely Dry"
  ))]
  transitions[, `SPI-TVT-Class` := factor(`SPI-TVT-Class`, levels = c("Extremely Wet","Very Wet","Moderately Wet","Near Normal",
                                                                      "Moderately Dry", "Very Dry", "Extremely Dry"
  ))]
  transitions[, Scale:= spiScales]
  fwrite(transitions, paste0("../outputs/",config[['distribution']],"_transitions_spi",spiScales,".csv"), row.names = FALSE)
  
  print("Miss-classifications generated successfully")
  print("Generating miss-classifications plot")
  
  p = ggplot(data = transitions, aes(x = `SPI-T-Class`, y = `SPI-TVT-Class`, size = Transitions)) + geom_point() + 
    theme_bw() + xlab(paste0("SPI(",spiScales,") - Training")) + ylab(paste0("SPI(",spiScales,") - Training, Validation, Test")) +
    
    theme(
      legend.key.width = unit(0.3, "cm"), 
      legend.key.height = unit(0.3, "cm"),
      legend.title = element_blank(),
      legend.position = "top",
      legend.text=element_text(size=4),
      plot.title = element_text(size=5, hjust=0.5),
      strip.text = element_text(size=5),
      axis.text.x = element_text(size = 5),
      axis.text.y = element_text(size = 5),
      axis.title.x = element_text(size = 5),
      axis.title.y = element_text(size = 5)
    ) +
    geom_abline(intercept =0 , slope = 1, color = "red")
  
  ggsave(filename = paste0("../outputs/",config[["distribution"]],"_Transition_plot_spi",spiScales,".jpg"),
         plot = p,
         units="cm",
         width=14.5,
         height=5,
         dpi=1000)
  print("Miss-classifications plot saved suffessfully")
}