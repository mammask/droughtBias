# --------------------------------------------------------------------#
# script name: main.R                                                 #
#     purpose: "Addressing the Effect of Bias During Model Selection  #
#               in Drought Forecasting Applications"                  #
#      author: Kostas Mammas                                          #
#       email: mammas@env.aegean.gr                                   #
# --------------------------------------------------------------------#
"
Description of input data (.csv):
Date (date) | Station id | Station id | .... | Station id
12-01-2010  |     0.2    |     0      |      |   2.1
13-01-2020  |     0.16   |     0      |      |    0
"

"
Description of Koppen Classification:
"

# Delete everything
rm(list = ls())

# Load configurations ---------------------------------------------------------
# install.packages("checkpoint")
# library(checkpoint)

# checkpoint("2020-12-11")
# install.packages("configr")

# Load configuration files
config = configr::read.config(file = "config.cfg")

# Load multiple libraries
lapply(config$dependencies$libraries, require, character.only = TRUE)

# Load source functions
sapply(paste0("src/",list.files("src/")), source)

# Set parameters
spiScales = config[['scale']][['spiScales']]
measureBias = config[['scenario']][["measureBias"]]
saveOutputs = config[['scenario']][["saveOutputs"]]

# Load Köppen Classification
koppen = fread("data/mapper_sweden_Koppen.csv")
koppen = koppen[is.na(GRIDCODE), GRIDCODE:= 0]
koppen[GRIDCODE == 42, GRIDCODE := 43]

# Load Basins locations
basin_locations = fread("data/mapper_sweden.csv")
basin_locations[, Station:= paste0("V",OBJECTID)]

# Load rainfall records
dtr = fread("data/Pobs.txt", skip = 1)
setnames(dtr, "V1", "Date")

# Define date formats
dtr[, Date:= as.Date(Date, format = "%Y-%m-%d")]

# Obtain the list of the basins
basins = names(dtr)[!names(dtr) %in% "Date"]

# Update names to match with kopen file
updatedBasins = paste0("V",1:length(basins))
setnames(dtr, c("Date",updatedBasins))

# Generate combinations of scales and basins 
combinations = data.table::CJ(spiScales, basins, unique = T, sorted = T)

# Obtain Kopen Class in combinations Set
koppen[, basins := paste0("V",OBJECTID)]
combinations = base::merge(combinations, koppen[,.(basins,GRIDCODE)], by = "basins")

# Obtain bias measure ------------------------------------------------#
if (measureBias == T){
  tt = system.time({
  pb = txtProgressBar(min = 0, max = combinations[,.N], style = 3)
  results =   combinations[, {setTxtProgressBar(pb, .GRP);
                              total = BiasMeasurement(StationId = basins,
                                                      ScaleId   = spiScales,
                                                      dtr       = dtr);
                              .(rawSpi        = list(total[[1]]),
                                parameters    = list(total[[2]]),
                                p.value_T_TVT    = total[[3]],
                                p.value_T_TV     = total[[4]],
                                transitions_T_TVT   = list(total[[5]]),
                                transitions_T_TV    = list(total[[6]]),
                                switchedClasses_T_TVT = total[[7]],
                                switchedClasses_T_TV = total[[8]],
                                recordsTrainingSet  = total[[9]],
                                transitions_T_TVT_year = list(total[[10]])
                                )},
                            by = .(spiScales,basins, GRIDCODE)
                            ]
    

  })
  close(pb)
  paste0("Bias measured in ",round(tt[[3]],2), " seconds")
  if (saveOutputs == T){
    # Saving analytical results
    saveRDS(results, paste0("outputs/bias measurement/training_bias_",spiScales,".RDS"))
  }

}