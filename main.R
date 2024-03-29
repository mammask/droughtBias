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
library(tmap)
library(data.table)
library(ggplot2)
library(configr)
library(rgdal)
library(sf)
library(RColorBrewer)
library(foreign)

# Load configuration files
config = configr::read.config(file = "config.cfg")

# Load multiple libraries
lapply(config$dependencies$libraries, require, character.only = TRUE)

# Load source functions
sourceFunctions = paste0("src/",list.files("src/"))
for (i in sourceFunctions){
  if (i == "src/runReporting.R"){
    next 
  }
  source(i)
}

# Set parameters
spiScales = config[['scale']][['spiScales']]
measureBias = config[['scenario']][["measureBias"]]
saveOutputs = config[['scenario']][["saveOutputs"]]

# Load Köppen Classification
koppen = fread("data/mapper_sweden_Koppen.csv")
koppen = koppen[is.na(GRIDCODE), GRIDCODE:= 0]
koppen[GRIDCODE == 42, GRIDCODE := 43]
koppen = koppen[MAINCLASS !=""]

# Load Basins locations
basin_locations = read.dbf("c:/Users/komammas.EUROPE/Downloads/drive-download-20210222T094357Z-001/SHYPE2012_version_1_2_0_polygons_smallglomma_wgs84.dbf")
setDT(basin_locations)
basin_locations[, SUBIDnew := as.character(SUBIDnew) ]

# Load rainfall records
dtr = fread("c:/Users/komammas.EUROPE/Downloads/Pobs.txt", skip = 0)
setnames(dtr, "date","Date")

# Define date formats
dtr[, Date:= as.Date(Date, format = "%Y-%m-%d")]

# Obtain the list of the basins
updatedBasins = names(dtr)[!names(dtr) %in% "Date"]

# Generate combinations of scales and basins 
combinations = data.table::CJ(spiScales, updatedBasins, unique = T, sorted = T)

# Obtain Kopen Class in combinations Set
koppen[, basins := as.character(SUBIDnew)]
combinations = base::merge(combinations, koppen[,.(basins,GRIDCODE)],
                           by.x = "updatedBasins",
                           by.y = "basins")

# Obtain bias measure ------------------------------------------------#
if (measureBias == T){
  tt = system.time({
  pb = txtProgressBar(min = 0, max = combinations[,.N], style = 3)
  results =   combinations[, {setTxtProgressBar(pb, .GRP);
                              total = BiasMeasurement(StationId = updatedBasins ,
                                                      ScaleId   = spiScales,
                                                      dtr       = dtr,
                                                      distribution = config[['distribution']],
                                                      method = config[['method']]);
                              .(rawSpi        = list(total[[1]]),
                                parameters    = list(total[[2]]),
                                transitions_T_TVT   = list(total[[3]]),
                                transitions_T_TV    = list(total[[4]]),
                                switchedClasses_T_TVT = total[[5]],
                                switchedClasses_T_TV = total[[6]],
                                recordsTrainingSet  = total[[7]],
                                transitions_T_TVT_year = list(total[[8]])
                                )},
                            by = .(spiScales,updatedBasins, GRIDCODE)
                            ]
    

  })
  close(pb)
  paste0("Bias measured in ",round(tt[[3]],2), " seconds")
  if (saveOutputs == T){
    # Saving analytical results
    saveRDS(results, paste0("outputs/bias measurement/",config[['distribution']],"_training_bias_",spiScales,".RDS"))
  }

}
