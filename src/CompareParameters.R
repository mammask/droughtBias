CompareParameters = function(spiData, spiScales){
  
  print("Extracting parameter estimates")
  parameters = copy(spiData[!is.na(p.value_T_TVT), rbindlist(parameters), by = basins])
  mparameter = melt.data.table(data = parameters,
                               id.vars = c("basins","Parameter","Description"),
                               variable.name = "Month", value.name = "Estimate")
  
  mparameter = dcast.data.table(data = mparameter,
                                formula = "basins + Parameter + Month ~ Description",
                                value.var = "Estimate", fun.aggregate = sum)
  print("Parameter estimates extracted successfully")
  print("Genarating comparison plots")
  p = ggplot(data = mparameter[Parameter == "alpha"]) + 
    geom_point(aes(x = `SPI-T`, y = `SPI-TVT`),size = 0.2, stroke = 0.1)+ 
    facet_wrap(~Month, nrow = 3) +
    geom_abline(color = "red") +
    xlab("alpha -Training set") + ylab("alpha -Training, Validation, Test sets") +
    theme_bw() + 
    theme(axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 7),
          axis.title.x = element_text(size = 7),
          axis.title.y = element_text(size = 7),
          strip.text.x = element_text(size = 6)
    )
  
  g = ggplot(data = mparameter[Parameter == "beta"]) + 
    geom_point(aes(x = `SPI-T`, y = `SPI-TVT`),size = 0.2, stroke = 0.1)+ 
    facet_wrap(~Month, nrow = 3) +
    geom_abline(color = "red") +
    xlab("beta -Training set") +
    ylab("beta -Training, Validation, Test sets") +
    theme_bw() + 
    theme(axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 7),
          axis.title.x = element_text(size = 7),
          axis.title.y = element_text(size = 7),
          strip.text.x = element_text(size = 6)
    )
  
  pall = gridExtra::grid.arrange(p, g, ncol = 1)
  ggsave(plot = pall, filename = paste0("../outputs/distribution_parameters",spiScales,".jpg"),
         units="cm", width=12.5, height=14, dpi = 1000)  
  print("Comparison plots saved successfully")
}