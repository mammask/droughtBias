ComputeDroughtIndex <- function(Date,y,d.freq, d.index, d.scale){
  
  if (d.index == "spi"){
    mts <- ts(data = y, start = min(Date), frequency = d.freq)
    dIndex <- SPEI::spi(data = mts, scale = d.scale, distribution = "Gamma", fit = "max-lik")
  }
  return(list(Date, as.numeric(dIndex$fitted)))
}

GetDroughtIndexParams <- function(Date,y,d.freq, d.index, d.scale){
  
  if (d.index == "spi"){
    mts <- ts(data = y, start = min(Date), frequency = d.freq)
    dIndex <- SPEI::spi(data = mts, scale = d.scale, distribution = "Gamma", fit = "max-lik")
  }
  return(dIndex)
}

classSPI = function(x){
  
  
  
  if (is.na(x)){
    
    type = as.character(NA)
    
  } else if (round(x,1) >= 2){
    
    type = "Extremely Wet"
    
  } else if (round(x,1) >= 1.5 & round(x,1) <= 1.99){
    
    type = "Very Wet"
    
  } else if (round(x,1) >= 1.0 & round(x,1) <= 1.49){
    
    type = "Moderately Wet"
    
  } else if (round(x,1) >= -0.99 & round(x,1) <= 0.99){
    
    type = "Near Normal"
    
  } else if (round(x,1) >= -1.49 &  round(x,1) <= -1.0){
    
    type = "Moderately Dry"
    
  } else if (round(x,1) >= -1.99 & round(x,1) <= -1.5){
    
    type = "Very Dry"
    
  } else if (round(x,1) <= -2){
    
    type = "Extremely Dry"
    
  }
  
  return(type)
}