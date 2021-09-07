computeSPIAll = function(ts_object, s.scale, distribution){
  
  accum_precip = c(rep(NA, s.scale-1),rowSums(embed(ts_object,s.scale),na.rm=FALSE))
  if (distribution == 'Gamma'){
    
    fitGamma = MASS::fitdistr(accum_precip[!is.na(accum_precip)], "gamma")
    gam_cdf = stats::pgamma(accum_precip, fitGamma$estimate[1], fitGamma$estimate[2])
    zscore = stats::qnorm(gam_cdf)
    p_shape = fitGamma$estimate[1][[1]]
    p_rate = fitGamma$estimate[2][[1]]
    params = as.data.table(rbind(p_shape, p_rate),keep.rownames = T)
  }
  if (distribution == 'LogNormal'){
    
    fitLogNormal = MASS::fitdistr(accum_precip[!is.na(accum_precip)], "lognormal")
    lognormal_cdf = stats::plnorm(accum_precip, fitLogNormal$estimate[1], fitLogNormal$estimate[2])
    zscore = stats::qnorm(lognormal_cdf)
    p_meanlog = fitLogNormal$estimate[1][[1]]
    p_sdlog = fitLogNormal$estimate[2][[1]]
    params = as.data.table(rbind(p_meanlog, p_sdlog),keep.rownames = T)
  }
  
  if (distribution == 'Normal'){
    
    fitNormal = MASS::fitdistr(accum_precip[!is.na(accum_precip)], "normal")
    normal_cdf = stats::pnorm(accum_precip, fitNormal$estimate[1], fitNormal$estimate[2])
    zscore <- stats::qnorm(normal_cdf)
    p_mean = fitNormal$estimate[1][[1]]
    p_sd = fitNormal$estimate[2][[1]]
    params = as.data.table(rbind(p_mean, p_sd),keep.rownames = T)
  }
  
  if (distribution == 'NSGamma'){
    nsgamm_rec = data.table(AccumPrecip = accum_precip[!is.na(accum_precip)])
    nsgamm_rec[, Trend := 1:.N]
    model = gamlss(formula = AccumPrecip ~ Trend, sigma.formula = AccumPrecip ~ Trend, data = nsgamm_rec, family = GA)
    pred = predictAll(model, data = nsgamm_rec[, .(AccumPrecip, Trend)], type = 'response')
    nsgamm_rec[, mu := pred$mu]
    nsgamm_rec[, sigma := pred$sigma]
    nsgamm_rec[, ecdfm := gamlss.dist::pGA(AccumPrecip, mu = mu, sigma = sigma)]
    nsgamm_rec[, NSPI := qnorm(ecdfm)]
    nsgamm_rec[, Date := as.yearmon(time(ts_object))[spiScales:length(ts_object)]]
    
    zscore = c(rep(NA, s.scale-1),nsgamm_rec[,NSPI])
    params = nsgamm_rec[, .(Date,mu,sigma)]
  }
  
  return(list(zscore, params))
}



computeSPI = function(ts_object, s.scale, distribution){
  
  # Compute the accumulated precipitation
  accum_precip = c(rep(NA, s.scale-1),rowSums(embed(ts_object,s.scale),na.rm=FALSE))
  
  # Convert object to data.table format
  accum_precip_dt = data.table(Date = as.yearmon(time(ts_object)),
                               AccumPrc = accum_precip
  )
  accum_precip_dt[, SPI := as.numeric(NA)]
  
  # Compute the Standardized Precipitation Index
  if (distribution == 'Gamma'){
    p_shape = c()
    p_rate  = c()
    for (k in 1:12){
      accum_vec = accum_precip_dt[month(Date) == k, AccumPrc]
      fitGamma = MASS::fitdistr(accum_vec[!is.na(accum_vec)], "gamma")
      gam_cdf = stats::pgamma(accum_vec, fitGamma$estimate[1], fitGamma$estimate[2])
      zscore <- stats::qnorm(gam_cdf)
      accum_precip_dt[month(Date) == k, SPI := zscore]
      p_shape = c(p_shape, fitGamma$estimate[1][[1]])
      p_rate = c(p_rate, fitGamma$estimate[2][[1]])
    }
    params = as.data.table(rbind(p_shape, p_rate),keep.rownames = T)
    setnames(params,  c("Parameter","Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul","Aug","Sept","Oct", "Nov", "Dec"))
    
    
  }
  if (distribution == 'Weibull'){
    p_shape = c()
    p_scale  = c()
    for (k in 1:12){
      accum_vec = accum_precip_dt[month(Date) == k, AccumPrc]
      fitWeibull = MASS::fitdistr(accum_vec[!is.na(accum_vec)], "weibull")
      weibul_cdf = stats::pweibull(accum_vec, fitWeibull$estimate[1], fitWeibull$estimate[2])
      zscore <- stats::qnorm(weibul_cdf)
      accum_precip_dt[month(Date) == k, SPI := zscore]
      p_shape = c(p_shape, fitWeibull$estimate[1][[1]])
      p_scale = c(p_scale, fitWeibull$estimate[2][[1]])
    }
    
    params = as.data.table(rbind(p_shape, p_scale),keep.rownames = T)
    setnames(params,  c("Parameter","Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul","Aug","Sept","Oct", "Nov", "Dec"))
  }
  
  if (distribution == 'LogNormal'){
    
    p_meanlog = c()
    p_sdlog  = c()
    for (k in 1:12){
      accum_vec = accum_precip_dt[month(Date) == k, AccumPrc]
      fitLogNormal = MASS::fitdistr(accum_vec[!is.na(accum_vec)], "lognormal")
      lognormal_cdf = stats::plnorm(accum_vec, fitLogNormal$estimate[1], fitLogNormal$estimate[2])
      zscore <- stats::qnorm(lognormal_cdf)
      accum_precip_dt[month(Date) == k, SPI := zscore]
      p_meanlog = c(p_meanlog, fitLogNormal$estimate[1][[1]])
      p_sdlog = c(p_sdlog, fitLogNormal$estimate[2][[1]])
    }
    params = list(p_meanlog, p_sdlog)
    params = as.data.table(rbind(p_meanlog, p_sdlog),keep.rownames = T)
    setnames(params,  c("Parameter","Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul","Aug","Sept","Oct", "Nov", "Dec"))
  }
  if (distribution == 'Normal'){
    p_mean = c()
    p_sd  = c()
    for (k in 1:12){
      accum_vec = accum_precip_dt[month(Date) == k, AccumPrc]
      fitNormal = MASS::fitdistr(accum_vec[!is.na(accum_vec)], "normal")
      normal_cdf = stats::pnorm(accum_vec, fitNormal$estimate[1], fitNormal$estimate[2])
      zscore <- stats::qnorm(normal_cdf)
      accum_precip_dt[month(Date) == k, SPI := zscore]
      p_mean = c(p_mean, fitNormal$estimate[1][[1]])
      p_sd = c(p_sd, fitNormal$estimate[2][[1]])
    }
    params = list(p_mean, p_sd)
    params = as.data.table(rbind(p_mean, p_sd),keep.rownames = T)
    setnames(params,  c("Parameter","Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul","Aug","Sept","Oct", "Nov", "Dec"))
  }
  
  return(list(accum_precip_dt, params))
}
