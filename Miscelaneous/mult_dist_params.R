library(data.table)
library(ggplot2)
library(gridExtra)


dt_gamma = readRDS("../outputs/bias measurement/Gamma_training_bias_12.RDS")
dt_nsgamma = readRDS("../outputs/bias measurement/NSGamma_training_bias_12.RDS")


setDT(dt_gamma)
setDT(dt_nsgamma)

params_gamma = dt_gamma[,rbindlist(parameters), by = .(spiScales, updatedBasins)]
setnames(params_gamma, c("rn","V1"), c("Parameter","Estimate"))
params_gamma = params_gamma[Description != 'SPI_TV']
params_gamma = dcast.data.table(data = params_gamma, formula = c('spiScales + updatedBasins + Parameter ~ Description'), value.var = 'Estimate')

p1 = ggplot(data = params_gamma[Parameter == 'alpha']) + geom_point(aes(x = `SPI_T`, y = `SPI_TVT`),size = 0.2, stroke = 0.1) +
  xlab("Training set")  + 
  ylab("Entire data") + 
  ggtitle('Gamma - Shape Parameter')+
  theme_bw() + 
  theme(axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        strip.text.x = element_text(size = 6),
        plot.title = element_text(size = 7)
  ) + xlim(c(min(c(params_gamma[Parameter == 'alpha', min(SPI_T)], params_gamma[Parameter == 'alpha', min(SPI_TVT)])),
             max(c(params_gamma[Parameter == 'alpha', max(SPI_T)], params_gamma[Parameter == 'alpha', max(SPI_TVT)])))) +
  ylim(c(min(c(params_gamma[Parameter == 'alpha', min(SPI_T)], params_gamma[Parameter == 'alpha', min(SPI_TVT)])),
           max(c(params_gamma[Parameter == 'alpha', max(SPI_T)], params_gamma[Parameter == 'alpha', max(SPI_TVT)])))) +
  geom_abline(intercept = 0, slope = 1, color = 'red')

  
p2 = ggplot(data = params_gamma[Parameter == 'beta']) + geom_point(aes(x = `SPI_T`, y = `SPI_TVT`),size = 0.2, stroke = 0.1) + 
  xlab("Training set")  + 
  ylab("Entire data") + 
  ggtitle("Gamma - Scale Parameter")+
  theme_bw() + 
  theme(axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        strip.text.x = element_text(size = 6),
        plot.title = element_text(size = 7)
  )+ xlim(c(min(c(params_gamma[Parameter == 'beta', min(SPI_T)], params_gamma[Parameter == 'beta', min(SPI_TVT)])),
            max(c(params_gamma[Parameter == 'beta', max(SPI_T)], params_gamma[Parameter == 'beta', max(SPI_TVT)])))) +
  ylim(c(min(c(params_gamma[Parameter == 'beta', min(SPI_T)], params_gamma[Parameter == 'beta', min(SPI_TVT)])),
         max(c(params_gamma[Parameter == 'beta', max(SPI_T)], params_gamma[Parameter == 'beta', max(SPI_TVT)])))) +
  geom_abline(intercept = 0, slope = 1, color = 'red')


params_nsgamma = dt_nsgamma[,rbindlist(parameters), by = .(spiScales, updatedBasins)]
params_nsgamma = params_nsgamma[Description != 'SPI_TV', .(mu = mean(mu),
                   sigma = max(sigma)
                   ),
               by = .(spiScales, updatedBasins, Description)
               ]


params_nsgamma_mu = dcast.data.table(data = params_nsgamma[,.(spiScales,updatedBasins,Description,mu)],
                                     formula = "spiScales+updatedBasins ~ Description", value.var = 'mu', fill = NA)



p3 = ggplot(data = params_nsgamma_mu) + geom_point(aes(x = `SPI_T`, y = `SPI_TVT`),size = 0.2, stroke = 0.1) +
  xlab("Training set")  + 
  ylab("Entire data") + 
  ggtitle('GAMLSS Gamma - Location Parameter')+
  theme_bw() + 
  theme(axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        strip.text.x = element_text(size = 6),
        plot.title = element_text(size = 7)
  ) + xlim(c(min(c(params_nsgamma_mu[, min(SPI_T)], params_nsgamma_mu[, min(SPI_TVT)])),
             max(c(params_nsgamma_mu[, max(SPI_T)], params_nsgamma_mu[, max(SPI_TVT)])))) +
  ylim(c(min(c(params_nsgamma_mu[, min(SPI_T)], params_nsgamma_mu[, min(SPI_TVT)])),
         max(c(params_nsgamma_mu[, max(SPI_T)], params_nsgamma_mu[, max(SPI_TVT)])))) +
  geom_abline(intercept = 0, slope = 1, color = 'red')


params_nsgamma_sigma = dcast.data.table(data = params_nsgamma[,.(spiScales,updatedBasins,Description,sigma)],
                                     formula = "spiScales+updatedBasins ~ Description", value.var = 'sigma', fill = NA)


p4 = ggplot(data = params_nsgamma_sigma) + geom_point(aes(x = `SPI_T`, y = `SPI_TVT`),size = 0.2, stroke = 0.1) +
  xlab("Training set")  + 
  ylab("Entire data") + 
  ggtitle('GAMLSS Gamma - Scale Parameter')+
  theme_bw() + 
  theme(axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        strip.text.x = element_text(size = 6),
        plot.title = element_text(size = 7)
  ) + xlim(c(min(c(params_nsgamma_sigma[, min(SPI_T)], params_nsgamma_sigma[, min(SPI_TVT)])),
             max(c(params_nsgamma_sigma[, max(SPI_T)], params_nsgamma_sigma[, max(SPI_TVT)])))) +
  ylim(c(min(c(params_nsgamma_sigma[, min(SPI_T)], params_nsgamma_sigma[, min(SPI_TVT)])),
         max(c(params_nsgamma_sigma[, max(SPI_T)], params_nsgamma_sigma[, max(SPI_TVT)])))) +
  geom_abline(intercept = 0, slope = 1, color = 'red')
# params_normal = dt_normal[,rbindlist(parameters), by = .(spiScales, updatedBasins)]
# setnames(params_normal, c("rn","V1"), c("Parameter","Estimate"))
# params_normal = params_normal[Description != 'SPI_TV']
# params_normal = dcast.data.table(data = params_normal, formula = c('spiScales + updatedBasins + Parameter ~ Description'), value.var = 'Estimate')
# 
# p3 = ggplot(data = params_normal[Parameter == 'p_mean']) + geom_point(aes(x = `SPI_T`, y = `SPI_TVT`),size = 0.2, stroke = 0.1) +
#   xlab("Training set")  + 
#   ylab("Entire data") + 
#   ggtitle('Normal - Mean Parameter')+
#   theme_bw() + 
#   theme(axis.text.x = element_text(size = 7),
#         axis.text.y = element_text(size = 7),
#         axis.title.x = element_text(size = 7),
#         axis.title.y = element_text(size = 7),
#         strip.text.x = element_text(size = 6),
#         plot.title = element_text(size = 7)
#   )
# p4 = ggplot(data = params_normal[Parameter == 'p_sd']) + geom_point(aes(x = `SPI_T`, y = `SPI_TVT`),size = 0.2, stroke = 0.1) + 
#   xlab("Training set")  + 
#   ylab("Entire data") + 
#   ggtitle("Normal - Sd Parameter")+
#   theme_bw() + 
#   theme(axis.text.x = element_text(size = 7),
#         axis.text.y = element_text(size = 7),
#         axis.title.x = element_text(size = 7),
#         axis.title.y = element_text(size = 7),
#         strip.text.x = element_text(size = 6),
#         plot.title = element_text(size = 7)
#   )
# 
# 
# params_lognormal = dt_lognormal[,rbindlist(parameters), by = .(spiScales, updatedBasins)]
# setnames(params_lognormal, c("rn","V1"), c("Parameter","Estimate"))
# params_lognormal = params_lognormal[Description != 'SPI_TV']
# params_lognormal = dcast.data.table(data = params_lognormal, formula = c('spiScales + updatedBasins + Parameter ~ Description'), value.var = 'Estimate')
# 
# p5 = ggplot(data = params_lognormal[Parameter == 'p_meanlog']) + geom_point(aes(x = `SPI_T`, y = `SPI_TVT`),size = 0.2, stroke = 0.1) +
#   xlab("Training set")  + 
#   ylab("Entire data") + 
#   ggtitle('LogNormal - Meanlog Parameter')+
#   theme_bw() + 
#   theme(axis.text.x = element_text(size = 7),
#         axis.text.y = element_text(size = 7),
#         axis.title.x = element_text(size = 7),
#         axis.title.y = element_text(size = 7),
#         strip.text.x = element_text(size = 6),
#         plot.title = element_text(size = 7)
#   )
# p6 = ggplot(data = params_lognormal[Parameter == 'p_sdlog']) + geom_point(aes(x = `SPI_T`, y = `SPI_TVT`),size = 0.2, stroke = 0.1) + 
#   xlab("Training set")  + 
#   ylab("Entire data") + 
#   ggtitle("LogNormal - Sdlog Parameter")+
#   theme_bw() + 
#   theme(axis.text.x = element_text(size = 7),
#         axis.text.y = element_text(size = 7),
#         axis.title.x = element_text(size = 7),
#         axis.title.y = element_text(size = 7),
#         strip.text.x = element_text(size = 6),
#         plot.title = element_text(size = 7)
#   )


p = grid.arrange(p1,p2,p3,p4, ncol = 2)
ggsave(plot = p, filename = paste0("../outputs/multiple_distribution_parameters_scale_3.jpg"),
       units="cm", width=12, height=8, dpi = 1000)  
