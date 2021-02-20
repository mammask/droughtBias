# Load configuration files
config = configr::read.config(file = "../config.cfg")

# Load multiple libraries
lapply(config$dependencies$libraries, require, character.only = TRUE)

# Load source functions
source("../src/GenerateDescriptives.R")
source("../src/GenerateMissClass.R")
source("../src/CompareParameters.R")
source("../src/PlotRawStationData.R")
source("../src/CompareDensities.R")
source("../src/ComputeBiasOverScales.R")
source("../src/GenerateSpatialMissClass.R")
source("../src/GenerateBoxPlotPerClGr.R")


# Set parameters
spiScales = config[['scale']][['spiScales']]
measureBias = config[['scenario']][["measureBias"]]
saveOutputs = config[['scenario']][["saveOutputs"]]

# Load Köppen Classification
koppen = fread("../data/mapper_sweden_Koppen.csv")
koppen = koppen[is.na(GRIDCODE), GRIDCODE:= 0]
koppen[GRIDCODE == 42, GRIDCODE := 43]
koppen[, Station := paste0("V",OBJECTID)]


# Load Basins locations
basins = fread("../data/mapper_sweden.csv")
basins[, Station:= paste0("V",OBJECTID)]

# Generate Plots with spatial descriptives -------------------------------------

# Loading metadata
print("Loading metadata")
t = system.time({
  spiData = readRDS(paste0("../outputs/bias measurement/training_bias_",
                           config[["scale"]],".RDS")
  )
  setDT(spiData)
})
print(paste0("Metadata loaded in ",t[[3]]," seconds."))

if (config[["plots"]][["spatialDescriptives"]] == TRUE){
  GenerateDescriptivesMaps(spiData, koppen, station, basins)
}

if (config[["plots"]][["missclassifications"]] == TRUE){
  GenerateMissclassifications(spiData, spiScales)
}

if (config[["plots"]][["compareParameters"]] == TRUE){
  CompareParameters(spiData, spiScales)
}

if (config[["plots"]][["compareStatioRawData"]] == TRUE){
  stationRawData(spiData, spiScales, stationId = config[["station"]][["stationId"]])
}

if (config[["plots"]][["compareDensities"]] == TRUE){
 CompareDensities(spiData, spiScales, stationId = config[["station"]][["stationId"]])
}

if (config[["plots"]][["biasOverScales"]] == TRUE){
  ComputeBiasOverScales()
}
if (config[["plots"]][["spatialMissclassifications"]] == TRUE){
  GenerateSpatialMissClass()
}
if (config[["plots"]][["boxplotClimateRegions"]] == TRUE){
  GenerateBoxPlotsPerClGradient()
}