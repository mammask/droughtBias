library(data.table)
library(ggplot2)
spiScales = 24

gammaTr = readRDS(paste0("../outputs/bias measurement/Gamma_training_bias_",spiScales,".RDS"))
# normalTr = readRDS(paste0("../outputs/bias measurement/Normal_training_bias_",spiScales,".RDS"))
# lognormalTr = readRDS(paste0("../outputs/bias measurement/LogNormal_training_bias_",spiScales,".RDS"))
nsgammaTr = readRDS(paste0("../outputs/bias measurement/NSGamma_training_bias_",spiScales,".RDS"))

transitions_gamma = copy(gammaTr[, rbindlist(transitions_T_TVT), by = .(updatedBasins)])
transitions_gamma = transitions_gamma[updatedBasins %in% koppen[MAINCLASS != "", Station]]

transitions_gamma = transitions_gamma[, .(Transitions = sum(N)), by = .(`SPI-T-Class`,`SPI-TVT-Class`)]
transitions_gamma[, Percentage := round(100*Transitions/sum(Transitions),2)]
transitions_gamma = transitions_gamma[`SPI-T-Class`!=`SPI-TVT-Class`]

transitions_gamma[, `SPI-T-Class` := factor(`SPI-T-Class`, levels = c("Extremely Wet","Very Wet","Moderately Wet","Near Normal",
                                                                "Moderately Dry", "Very Dry", "Extremely Dry"
))]
transitions_gamma[, `SPI-TVT-Class` := factor(`SPI-TVT-Class`, levels = c("Extremely Wet","Very Wet","Moderately Wet","Near Normal",
                                                                    "Moderately Dry", "Very Dry", "Extremely Dry"
))]
transitions_gamma[, Scale:= spiScales]
transitions_gamma[, Distribution := "Gamma"]

# --------------------------------------------------------------------------------------

# transitions_normal = copy(normalTr[, rbindlist(transitions_T_TVT), by = .(updatedBasins)])
# transitions_normal = transitions_normal[updatedBasins %in% koppen[MAINCLASS != "", Station]]
# 
# transitions_normal = transitions_normal[, .(Transitions = sum(N)), by = .(`SPI-T-Class`,`SPI-TVT-Class`)]
# transitions_normal[, Percentage := round(100*Transitions/sum(Transitions),2)]
# transitions_normal = transitions_normal[`SPI-T-Class`!=`SPI-TVT-Class`]
# 
# transitions_normal[, `SPI-T-Class` := factor(`SPI-T-Class`, levels = c("Extremely Wet","Very Wet","Moderately Wet","Near Normal",
#                                                                       "Moderately Dry", "Very Dry", "Extremely Dry"
# ))]
# transitions_normal[, `SPI-TVT-Class` := factor(`SPI-TVT-Class`, levels = c("Extremely Wet","Very Wet","Moderately Wet","Near Normal",
#                                                                           "Moderately Dry", "Very Dry", "Extremely Dry"
# ))]
# transitions_normal[, Scale:= spiScales]
# transitions_normal[, Distribution := "Normal"]

# --------------------------------------------------------------------------------------

# transitions_lognormal = copy(lognormalTr[, rbindlist(transitions_T_TVT), by = .(updatedBasins)])
# transitions_lognormal = transitions_lognormal[updatedBasins %in% koppen[MAINCLASS != "", Station]]
# 
# transitions_lognormal = transitions_lognormal[, .(Transitions = sum(N)), by = .(`SPI-T-Class`,`SPI-TVT-Class`)]
# transitions_lognormal[, Percentage := round(100*Transitions/sum(Transitions),2)]
# transitions_lognormal = transitions_lognormal[`SPI-T-Class`!=`SPI-TVT-Class`]
# 
# transitions_lognormal[, `SPI-T-Class` := factor(`SPI-T-Class`, levels = c("Extremely Wet","Very Wet","Moderately Wet","Near Normal",
#                                                                        "Moderately Dry", "Very Dry", "Extremely Dry"
# ))]
# transitions_lognormal[, `SPI-TVT-Class` := factor(`SPI-TVT-Class`, levels = c("Extremely Wet","Very Wet","Moderately Wet","Near Normal",
#                                                                            "Moderately Dry", "Very Dry", "Extremely Dry"
# ))]
# transitions_lognormal[, Scale:= spiScales]
# transitions_lognormal[, Distribution := "LogNormal"]

# --------------------------------------------------------------------------------------

transitions_nsgamma = copy(nsgammaTr[, rbindlist(transitions_T_TVT), by = .(updatedBasins)])
transitions_nsgamma = transitions_nsgamma[updatedBasins %in% koppen[MAINCLASS != "", Station]]

transitions_nsgamma = transitions_nsgamma[, .(Transitions = sum(N)), by = .(`SPI-T-Class`,`SPI-TVT-Class`)]
transitions_nsgamma[, Percentage := round(100*Transitions/sum(Transitions),2)]
transitions_nsgamma = transitions_nsgamma[`SPI-T-Class`!=`SPI-TVT-Class`]

transitions_nsgamma[, `SPI-T-Class` := factor(`SPI-T-Class`, levels = c("Extremely Wet","Very Wet","Moderately Wet","Near Normal",
                                                                          "Moderately Dry", "Very Dry", "Extremely Dry"
))]
transitions_nsgamma[, `SPI-TVT-Class` := factor(`SPI-TVT-Class`, levels = c("Extremely Wet","Very Wet","Moderately Wet","Near Normal",
                                                                              "Moderately Dry", "Very Dry", "Extremely Dry"
))]
transitions_nsgamma[, Scale:= spiScales]
transitions_nsgamma[, Distribution := "NSGamma"]

# --------------------------------------------------------------------------------------

allTransitions = rbindlist(list(transitions_gamma, transitions_nsgamma))
setnames(allTransitions, "Percentage", "Transitions (%)")
allTransitions[, `Transitions (%)` := round(`Transitions (%)`,1)]
fwrite(allTransitions, paste0("../outputs/Transition_plot_spi",spiScales,".csv"), row.names = F)

p = ggplot(data = allTransitions, aes(x = `SPI-T-Class`, y = `SPI-TVT-Class`, fill = `Transitions (%)`)) + 
  geom_tile(color="white", size=0.1) + facet_wrap(~Distribution, ncol = 1) +
  theme_bw() + 
  theme(legend.key.width = unit(0.2, "cm"), 
        legend.key.height = unit(0.3, "cm"),
        legend.position = "bottom",
        legend.text=element_text(size=5),
        legend.title= element_text(size=5),
        plot.title = element_text(size=8, hjust=0.5),
        strip.text = element_text(size=8),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        strip.text.x = element_text(size = 6)
  ) +
  xlab(paste0("SPI(",spiScales,") - Training")) +
  ylab(paste0("SPI(",spiScales,") - Entire data")) +
  scale_fill_distiller(palette = "Spectral") + 
  geom_text(data = allTransitions, aes(x = `SPI-T-Class`, y = `SPI-TVT-Class`, label =`Transitions (%)`), size = 2)

ggsave(filename = paste0("../outputs/Transition_plot_spi",spiScales,".jpg"),
       plot = p,
       units="cm",
       width=12,
       height=8,
       dpi=1000)


# ggsave(filename = paste0("../outputs/all_distributions_Transition_plot_spi",spiScales,".jpg"),
#        plot = p,
#        units="cm",
#        width=12,
#        height=14,
#        dpi=1000)

fwrite(allTransitions, paste0("../outputs/transitions_spi",spiScales,".csv"), row.names = FALSE)
