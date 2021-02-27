CompareParameters = function(spiData, spiScales){
  
  print("Extracting parameter estimates")
  parameters = copy(spiData[!is.na(p.value_T_TVT), rbindlist(parameters), by = updatedBasins])
  parameters = parameters[updatedBasins %in% koppen[MAINCLASS != "", Station]]
  
  mparameter = melt.data.table(data = parameters,
                               id.vars = c("updatedBasins","Parameter","Description"),
                               variable.name = "Month", value.name = "Estimate")
  
  minalpha = mparameter[Parameter == "alpha" & Description != "SPI-TV", min(Estimate)]
  maxalpha = mparameter[Parameter == "alpha" & Description != "SPI-TV", max(Estimate)]
  
  minbeta = mparameter[Parameter == "beta" & Description != "SPI-TV", min(Estimate)]
  maxbeta = mparameter[Parameter == "beta" & Description != "SPI-TV", max(Estimate)]
  
  
  mparameter = dcast.data.table(data = mparameter,
                                formula = "updatedBasins + Parameter + Month ~ Description",
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
    ) +
    xlim(c(minalpha, maxalpha)) + 
    ylim(c(minalpha, maxalpha))
  
  
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
    )+
    xlim(c(minbeta, maxbeta)) + 
    ylim(c(minbeta, maxbeta))
  
  pall = gridExtra::grid.arrange(p, g, ncol = 1)
  ggsave(plot = pall, filename = paste0("../outputs/distribution_parameters",spiScales,".jpg"),
         units="cm", width=12.5, height=14, dpi = 1000)  
  print("Comparison plots saved successfully")
}