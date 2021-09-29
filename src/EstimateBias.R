BiasMeasurement <- function(StationId, ScaleId, dtr, distribution, method){
  
  
  "Function Name: BiasMeasurement
         Purpose: To measure the Bias of wrong use of SPI in drought
                  forecasting applications
          Inputs: StationId -- The id of the basin
                    ScaleId -- The id of the selected scale
                        dtr -- The data.table object with the raw data
  "
  # Get combination and compute monthly rainfall
  iteration = copy(dtr[,c("Date", StationId), with = F])
  iteration = iteration[year(Date) >= 1961]
  iteration[, Station := StationId] 
  setnames(iteration, c("Date","Rainfall","Station"))
  iteration = iteration[,.(Rainfall = sum(Rainfall)),
                        keyby = .(Station,Date = as.yearmon(Date))
  ]
  if (iteration[Rainfall<0,.N] == 0){
    iteration[Rainfall < 0, Rainfall := 0]
    
    # Perform data split
    iteration[, Status:= train_valid_test_split(x = Date,
                                                trainSplit = 0.6 ,
                                                validSplit = 0.2,
                                                testSplit  = 0.2
    ),
    by = Station
    ]
    
    # Convert status to factor
    iteration[, Status:= factor(Status, levels = c("Train", "Validation", "Test"))]
    # Compute the SPI using the training, validation and test set ------------------------------------------------
    ts_object = iteration[Status %in% c("Train","Validation","Test"), ts(Rainfall, start = min(Date), frequency = 12)]
    if (method == 'nonstationary'){
      SPI_TVT = computeSPIAll(ts_object, ScaleId, distribution)
    } else if (method == 'stationary'){
      SPI_TVT = list()
      droughtObject <- copy(iteration[Status %in% c("Train","Validation","Test"),
                                list(GetDroughtIndexParams(Date    = Date,
                                                           y       = Rainfall,
                                                           d.freq  = 1,
                                                           d.index = "spi",
                                                           d.scale = ScaleId,
                                                           distribution = 'Gamma')
                                )
      ]
      )
      SPI_TVT[[2]] = as.data.frame(droughtObject[["V1"]][3])
      setnames(SPI_TVT[[2]], c('V1'))
      SPI_TVT[[2]] = as.data.table(SPI_TVT[[2]], keep.rownames = T)
      SPI_TVT[[1]] = as.numeric(droughtObject[["V1"]][2][[1]])
      rm(droughtObject)
    }
    SPI_TVT_Params = SPI_TVT[[2]]
    SPI_TVT_Params[, Description := "SPI_TVT"]
    iteration[, `SPI-TVT` := SPI_TVT[[1]]]
    
    # Compute the SPI using the training and validation set ------------------------------------------------------
    ts_object = iteration[Status %in% c("Train","Validation"), ts(Rainfall, start = min(Date), frequency = 12)]
    if (method == 'nonstationary'){
      SPI_TV = computeSPIAll(ts_object, ScaleId, distribution)
    } else if (method == 'stationary'){
      
      SPI_TV = list()
      droughtObject <- copy(iteration[Status %in% c("Train","Validation"),
                                      list(GetDroughtIndexParams(Date    = Date,
                                                                 y       = Rainfall,
                                                                 d.freq  = 1,
                                                                 d.index = "spi",
                                                                 d.scale = ScaleId,
                                                                 distribution = 'Gamma')
                                      )
      ]
      )
      SPI_TV[[2]] = as.data.frame(droughtObject[["V1"]][3])
      setnames(SPI_TV[[2]], c('V1'))
      SPI_TV[[2]] = as.data.table(SPI_TV[[2]], keep.rownames = T)
      SPI_TV[[1]] = as.numeric(droughtObject[["V1"]][2][[1]])
      rm(droughtObject)
    }
    SPI_TV_Params = SPI_TV[[2]]
    SPI_TV_Params[, Description := "SPI_TV"]
    iteration[Status %in% c("Train","Validation"), `SPI-TV` := SPI_TV[[1]]]
    
    # Compute the SPI using the training set ---------------------------------------------------------------------
    ts_object = iteration[Status %in% c("Train"), ts(Rainfall, start = min(Date), frequency = 12)]
    if (method == 'nonstationary'){
      SPI_T = computeSPIAll(ts_object, ScaleId, distribution)
    } else if (method == 'stationary'){
      SPI_T = list()
      droughtObject <- copy(iteration[Status %in% c("Train"),
                                      list(GetDroughtIndexParams(Date    = Date,
                                                                 y       = Rainfall,
                                                                 d.freq  = 1,
                                                                 d.index = "spi",
                                                                 d.scale = ScaleId,
                                                                 distribution = 'Gamma')
                                      )
      ]
      )
      SPI_T[[2]] = as.data.frame(droughtObject[["V1"]][3])
      setnames(SPI_T[[2]], c('V1'))
      SPI_T[[2]] = as.data.table(SPI_T[[2]], keep.rownames = T)
      SPI_T[[1]] = as.numeric(droughtObject[["V1"]][2][[1]])
    }
    SPI_T_Params = SPI_T[[2]]
    SPI_T_Params[, Description := "SPI_T"]
    iteration[Status %in% c("Train"), `SPI-T` := SPI_T[[1]]]

    # Gather the total parameters
    totalParameters <- rbindlist(list(SPI_TVT_Params,SPI_TV_Params,SPI_T_Params))
    # Obtain train data to perform comparisons
    comparison = copy(iteration[Status %in% c("Train")])
    comparison = comparison[complete.cases(comparison)]
    # Compute drought class classes
    comparison[, `SPI-TVT-Class` := classSPI(`SPI-TVT`), by = 1:nrow(comparison)]
    comparison[, `SPI-TV-Class` := classSPI(`SPI-TV`), by = 1:nrow(comparison)]
    comparison[, `SPI-T-Class` := classSPI(`SPI-T`), by = 1:nrow(comparison)]
    
    # Compute drought class transitions
    transitions_T_TVT <- comparison[Status %in% c("Train"),
                                    .N, by = .(Year = year(Date),`SPI-T-Class`,`SPI-TVT-Class`)
    ]
    
    discrepancies_T_TVT = copy(transitions_T_TVT)
    
    transitions_T_TVT = transitions_T_TVT[complete.cases(transitions_T_TVT)]
    transitions_T_TVT = transitions_T_TVT[, .(N = sum(N)), by = .(`SPI-T-Class`,`SPI-TVT-Class`)]
    transitions_T_TVT = transitions_T_TVT[complete.cases(transitions_T_TVT)] 
    transitions_T_TVT[, Percentage := round(100*N/sum(N),2)]
    transitions_T_TV = comparison[Status %in% c("Train"),
                                  .N, by = .(`SPI-T-Class`,`SPI-TV-Class`)]
    transitions_T_TV = transitions_T_TV[complete.cases(transitions_T_TV)] 
    transitions_T_TV[, Percentage := round(100*N/sum(N),2)]
    
    # Switched Classes
    switchedClasses_T_TVT = transitions_T_TVT[`SPI-T-Class`!=`SPI-TVT-Class`, sum(N)]
    totalRecords_T_TVT = transitions_T_TVT[, sum(N)]
    
    switchedClasses_T_TV = transitions_T_TV[`SPI-T-Class`!=`SPI-TV-Class`, sum(N)]
    totalRecords_T_TV = transitions_T_TV[, sum(N)]
    
    return(list(iteration,                      
                totalParameters,                # Parameter estimates
                # p.value_T_TVT,                  # P-value between SPI on training and SPI on training, validation and test
                # p.value_T_TV,                   # P-value between SPI on training and SPI on training, validation
                transitions_T_TVT,              # Drought class transitions from training to training validation test
                transitions_T_TV,               # Drought class transitions from training to training validation
                switchedClasses_T_TVT,          # # of Drought class transitions from training to training validation test
                switchedClasses_T_TV,           # # of Drought class transitions from training to training validation
                totalRecords_T_TVT,             # Total records in training set
                discrepancies_T_TVT             # Class transitions per year
    ))
    
  } else {
    
    return(list(data.table(),
                data.table(),
                # as.numeric(NA),
                # as.numeric(NA),
                data.table(),
                data.table(),
                as.integer(NA),
                as.integer(NA),
                as.integer(NA),
                data.table()))
    
  }
}
# }
# SPI_TVT <- copy(iteration[Status %in% c("Train","Validation","Test"),
#                           list(GetDroughtIndexParams(Date    = Date,
#                                                     y       = Rainfall,
#                                                     d.freq  = 12,
#                                                     d.index = "spi",
#                                                     d.scale = ScaleId,
#                                                     distribution = distribution)
#                           )
#                           ]
# )
# 
# 
# # Obtain the parameters of the gamma distribution
# SPI_TVT_Params <- data.frame(SPI_TVT[["V1"]][3])
# setDT(SPI_TVT_Params, keep.rownames = T)
# setnames(SPI_TVT_Params, c("Parameter", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul","Aug","Sept","Oct", "Nov", "Dec"))
# SPI_TVT_Params[, Description := "SPI-TVT"]
# 
# # Obtain the SPI raw data
# iteration[, `SPI-TVT` := as.numeric(SPI_TVT[["V1"]][2][[1]])[1:.N]]
# SPI_TVT_Params = SPI_TVT[[2]]
# iteration = merge(iteration, SPI_TVT[[1]][, .(Date, `SPI-TVT` = SPI)], by = 'Date')


# iteration = SPI_TVT[[1]]

# # Compute the SPI using the training and validation set -------------------------------------------------------
# SPI_TV <- copy(iteration[Status %in% c("Train","Validation"),
#                         list(GetDroughtIndexParams(Date    = Date,
#                                                     y       = Rainfall,
#                                                     d.freq  = 12,
#                                                     d.index = "spi",
#                                                     d.scale = ScaleId,
#                                                     distribution = distribution)
#                         )
#                         ]
# )
# 
# # Obtain the parameters of the gamma distribution
# SPI_TV_Params <- data.frame(SPI_TV[["V1"]][3])
# setDT(SPI_TV_Params, keep.rownames = T)
# setnames(SPI_TV_Params, c("Parameter", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul","Aug","Sept","Oct", "Nov", "Dec"))
# SPI_TV_Params[, Description := "SPI-TV"]
# 
# # Obtain the SPI raw data
# iteration[Status %in% c("Train","Validation"), `SPI-TV` := as.numeric(SPI_TV[["V1"]][2][[1]])]
# 
# 
# # Compute the SPI using the training set only -----------------------------------------------------------------
# SPI_T <- copy(iteration[Status %in% c("Train"),
#                         list(GetDroughtIndexParams(Date    = Date,
#                                                     y       = Rainfall,
#                                                     d.freq  = 12,
#                                                     d.index = "spi",
#                                                     d.scale = ScaleId,
#                                                     distribution = distribution)
#                         )
#                         ]
# )    
# 
# SPI_T_Params <- data.frame(SPI_T[["V1"]][3])
# setDT(SPI_T_Params, keep.rownames = T)
# setnames(SPI_T_Params, c("Parameter", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul","Aug","Sept","Oct", "Nov", "Dec"))
# SPI_T_Params[, Description := "SPI-T"]
# 
# # Obtain the SPI raw data
# iteration[Status %in% c("Train"), `SPI-T` := as.numeric(SPI_T[["V1"]][2][[1]])]

# Gather total parameters -------------------------------------------------------------------------------------
# totalParameters <- rbindlist(list(SPI_TVT_Params,SPI_TV_Params,SPI_T_Params))

# # Perform Hypothesis Testing and compare the sample mean of the two SPI versions
# comparison <- copy(iteration[Status %in% c("Train")])
# 
# # t test between spi-tv and spi-tvt
# p.value_T_TVT <- comparison[complete.cases(comparison[,.(`SPI-TVT`,`SPI-T`)]),
#                             t.test(`SPI-T`,`SPI-TVT`,conf.level = 0.05)$p.value
#                             ]
# # t test between spi-t and spi-tvt
# p.value_T_TV <- comparison[complete.cases(comparison[,.(`SPI-T`,`SPI-TV`)]),
#                          t.test(`SPI-T`,`SPI-TV`,conf.level = 0.05)$p.value
#                          ]

# # Compute drought class transitions
# comparison[, `SPI-TVT-Class` := classSPI(`SPI-TVT`), by = 1:nrow(comparison)]
# comparison[, `SPI-TV-Class` := classSPI(`SPI-TV`), by = 1:nrow(comparison)]
# comparison[, `SPI-T-Class` := classSPI(`SPI-T`), by = 1:nrow(comparison)]


# transitions_T_TVT <- comparison[Status %in% c("Train"),
#                               .N, by = .(Year = year(Date),`SPI-T-Class`,`SPI-TVT-Class`)
#                               ]
# 
# discrepancies_T_TVT <- copy(transitions_T_TVT)
# 
# transitions_T_TVT <- transitions_T_TVT[complete.cases(transitions_T_TVT)]
# 
# transitions_T_TVT <- transitions_T_TVT[, .(N = sum(N)), by = .(`SPI-T-Class`,`SPI-TVT-Class`)]
# 
# transitions_T_TVT <- transitions_T_TVT[complete.cases(transitions_T_TVT)] 
# transitions_T_TVT[, Percentage := round(100*N/sum(N),2)]
# 
# transitions_T_TV <- comparison[Status %in% c("Train"),
#                                .N, by = .(`SPI-T-Class`,`SPI-TV-Class`)
#                               ]
# transitions_T_TV <- transitions_T_TV[complete.cases(transitions_T_TV)] 
# transitions_T_TV[, Percentage := round(100*N/sum(N),2)]
# 
# # Switched Classes
# switchedClasses_T_TVT <- transitions_T_TVT[`SPI-T-Class`!=`SPI-TVT-Class`, sum(N)]
# totalRecords_T_TVT <- transitions_T_TVT[, sum(N)]
# 
# switchedClasses_T_TV <- transitions_T_TV[`SPI-T-Class`!=`SPI-TV-Class`, sum(N)]
# totalRecords_T_TV <- transitions_T_TV[, sum(N)]
#     return(list(iteration,                      
#                 totalParameters,                # Parameter estimates
#                 p.value_T_TVT,                  # P-value between SPI on training and SPI on training, validation and test
#                 p.value_T_TV,                   # P-value between SPI on training and SPI on training, validation
#                 transitions_T_TVT,              # Drought class transitions from training to training validation test
#                 transitions_T_TV,               # Drought class transitions from training to training validation
#                 switchedClasses_T_TVT,          # # of Drought class transitions from training to training validation test
#                 switchedClasses_T_TV,           # # of Drought class transitions from training to training validation
#                 totalRecords_T_TVT,             # Total records in training set
#                 discrepancies_T_TVT             # Class transitions per year
#                 ))
# 
#   } else {
# 
#     return(list(data.table(),data.table(),as.numeric(NA),as.numeric(NA),data.table(),data.table(),
#                 as.integer(NA),as.integer(NA),as.integer(NA), data.table()))
# 
#   }
# }
