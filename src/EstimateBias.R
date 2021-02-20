BiasMeasurement <- function(StationId, ScaleId, dtr){
  "Function Name: BiasMeasurement
         Purpose: To measure the Bias of wrong use of SPI in drought
                  forecasting applications
          Inputs: StationId -- The id of the basin
                    ScaleId -- The id of the selected scale
                        dtr -- The data.table object with the raw data
  "
 
  # Get combination and compute monthly rainfall
  iteration <- copy(dtr[,c("Date", StationId), with = F])
  iteration <- iteration[year(Date) >= 1961]
  iteration[, Station := StationId] 
  setnames(iteration, c("Date","Rainfall","Station"))
  iteration <- iteration[,.(Rainfall = sum(Rainfall)),
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
    SPI_TVT <- copy(iteration[Status %in% c("Train","Validation","Test"),
                              list(GetDroughtIndexParams(Date    = Date,
                                                        y       = Rainfall,
                                                        d.freq  = 12,
                                                        d.index = "spi",
                                                        d.scale = ScaleId)
                              )
                              ]
    )
    
    # Obtain the parameters of the gamma distribution
    SPI_TVT_Params <- data.frame(SPI_TVT[["V1"]][3])
    setDT(SPI_TVT_Params, keep.rownames = T)
    setnames(SPI_TVT_Params, c("Parameter", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul","Aug","Sept","Oct", "Nov", "Dec"))
    SPI_TVT_Params[, Description := "SPI-TVT"]
    
    # Obtain the SPI raw data
    iteration[, `SPI-TVT` := as.numeric(SPI_TVT[["V1"]][2][[1]])[1:.N]]
    
    # Compute the SPI using the training and validation set -------------------------------------------------------
    SPI_TV <- copy(iteration[Status %in% c("Train","Validation"),
                            list(GetDroughtIndexParams(Date    = Date,
                                                        y       = Rainfall,
                                                        d.freq  = 12,
                                                        d.index = "spi",
                                                        d.scale = ScaleId)
                            )
                            ]
    )
    
    # Obtain the parameters of the gamma distribution
    SPI_TV_Params <- data.frame(SPI_TV[["V1"]][3])
    setDT(SPI_TV_Params, keep.rownames = T)
    setnames(SPI_TV_Params, c("Parameter", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul","Aug","Sept","Oct", "Nov", "Dec"))
    SPI_TV_Params[, Description := "SPI-TV"]
    
    # Obtain the SPI raw data
    iteration[Status %in% c("Train","Validation"), `SPI-TV` := as.numeric(SPI_TV[["V1"]][2][[1]])]
    
    
    # Compute the SPI using the training set only -----------------------------------------------------------------
    SPI_T <- copy(iteration[Status %in% c("Train"),
                            list(GetDroughtIndexParams(Date    = Date,
                                                        y       = Rainfall,
                                                        d.freq  = 12,
                                                        d.index = "spi",
                                                        d.scale = ScaleId)
                            )
                            ]
    )    

    SPI_T_Params <- data.frame(SPI_T[["V1"]][3])
    setDT(SPI_T_Params, keep.rownames = T)
    setnames(SPI_T_Params, c("Parameter", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul","Aug","Sept","Oct", "Nov", "Dec"))
    SPI_T_Params[, Description := "SPI-T"]

    # Obtain the SPI raw data
    iteration[Status %in% c("Train"), `SPI-T` := as.numeric(SPI_T[["V1"]][2][[1]])]

    # Gather total parameters -------------------------------------------------------------------------------------
    totalParameters <- rbindlist(list(SPI_TVT_Params,SPI_TV_Params,SPI_T_Params))
    
    # Perform Hypothesis Testing and compare the sample mean of the two SPI versions
    comparison <- copy(iteration[Status %in% c("Train")])
    
    # t test between spi-tv and spi-tvt
    p.value_T_TVT <- comparison[complete.cases(comparison[,.(`SPI-TVT`,`SPI-T`)]),
                                t.test(`SPI-T`,`SPI-TVT`,conf.level = 0.05)$p.value
                                ]
    # t test between spi-t and spi-tvt
    p.value_T_TV <- comparison[complete.cases(comparison[,.(`SPI-T`,`SPI-TV`)]),
                             t.test(`SPI-T`,`SPI-TV`,conf.level = 0.05)$p.value
                             ]

    # Compute drought class transitions
    comparison[, `SPI-TVT-Class` := classSPI(`SPI-TVT`), by = 1:nrow(comparison)]
    comparison[, `SPI-TV-Class` := classSPI(`SPI-TV`), by = 1:nrow(comparison)]
    comparison[, `SPI-T-Class` := classSPI(`SPI-T`), by = 1:nrow(comparison)]


    transitions_T_TVT <- comparison[Status %in% c("Train"),
                                  .N, by = .(Year = year(Date),`SPI-T-Class`,`SPI-TVT-Class`)
                                  ]
    
    discrepancies_T_TVT <- copy(transitions_T_TVT)
    
    transitions_T_TVT <- transitions_T_TVT[complete.cases(transitions_T_TVT)]
    
    transitions_T_TVT <- transitions_T_TVT[, .(N = sum(N)), by = .(`SPI-T-Class`,`SPI-TVT-Class`)]
    
    transitions_T_TVT <- transitions_T_TVT[complete.cases(transitions_T_TVT)] 
    transitions_T_TVT[, Percentage := round(100*N/sum(N),2)]
    
    transitions_T_TV <- comparison[Status %in% c("Train"),
                                   .N, by = .(`SPI-T-Class`,`SPI-TV-Class`)
                                  ]
    transitions_T_TV <- transitions_T_TV[complete.cases(transitions_T_TV)] 
    transitions_T_TV[, Percentage := round(100*N/sum(N),2)]

    # Switched Classes
    switchedClasses_T_TVT <- transitions_T_TVT[`SPI-T-Class`!=`SPI-TVT-Class`, sum(N)]
    totalRecords_T_TVT <- transitions_T_TVT[, sum(N)]
    
    switchedClasses_T_TV <- transitions_T_TV[`SPI-T-Class`!=`SPI-TV-Class`, sum(N)]
    totalRecords_T_TV <- transitions_T_TV[, sum(N)]
    return(list(iteration,                      
                totalParameters,                # Parameter estimates
                p.value_T_TVT,                  # P-value between SPI on training and SPI on training, validation and test
                p.value_T_TV,                   # P-value between SPI on training and SPI on training, validation
                transitions_T_TVT,              # Drought class transitions from training to training validation test
                transitions_T_TV,               # Drought class transitions from training to training validation
                switchedClasses_T_TVT,          # # of Drought class transitions from training to training validation test
                switchedClasses_T_TV,           # # of Drought class transitions from training to training validation
                totalRecords_T_TVT,             # Total records in training set
                discrepancies_T_TVT             # Class transitions per year
                ))

  } else {

    return(list(data.table(),data.table(),as.numeric(NA),as.numeric(NA),data.table(),data.table(),
                as.integer(NA),as.integer(NA),as.integer(NA), data.table()))

  }
}
