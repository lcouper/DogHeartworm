### Dog Heartworm Q2 Analysis: Data pre-processing ####
# Part 1: Combining climate, land cover, and surveillance data sets
# Part 2: Cleaning surveillance (transforming skewed variables, removing rare counties / trap types)


#### Part 1: Combining data sets for analysis #####
# Create data sheets for each species that links 
# mosquito surveillance data, & calcualted climate and land cover variables 

AllSpecies = read.csv("~/Documents/Current_Projects/DogHeartworm/SurveillanceData_AllSpecies_WithClimVariables.csv", header = T)
AllSpecies = AllSpecies[,-1]
Clim_AeVexans = AllSpecies[AllSpecies$species == "AedesVexans",]
Clim_AeSierrensis = AllSpecies[AllSpecies$species == "AedesSierrensis",]
Clim_AeAegypti = AllSpecies[AllSpecies$species == "AedesAegypti",]
Clim_AeAlbopictus = AllSpecies[AllSpecies$species == "AedesAlbopictus",]
Clim_AnFreeborni = AllSpecies[AllSpecies$species == "AnophelesFreeborni",]
Clim_CsIncidens = AllSpecies[AllSpecies$species == "CulisetaIncidens",]
Clim_CsInornata = AllSpecies[AllSpecies$species == "CulisetaInornata",]
Clim_CxQuinque = AllSpecies[AllSpecies$species == "CulexQuinquefasciatus",]
Clim_CxTarsalis = AllSpecies[AllSpecies$species == "CulexTarsalis",]

#output each above df as file into MosquitoDataWithClimData_ForQ2 folder
# Example: write.csv(Clim_AeVexans, "~/Documents/Current_Projects/DogHeartworm/MosquitoDataWithClimData_ForQ2/AedesVexans_SurveillanceData_WithClimVariables.csv")

# Pull in land cover data
Land_AeVexans = read.csv("~/Documents/Current_Projects/DogHeartworm/LandCoverData_WithVarsCalculated/DogHeartworm_LandCoverVariables_AedesVexans.csv", header= T)
Land_AeSierrensis = read.csv("~/Documents/Current_Projects/DogHeartworm/LandCoverData_WithVarsCalculated/DogHeartworm_LandCoverVariables_AedesSierrensis.csv", header= T)
Land_AeAegypti = read.csv("~/Documents/Current_Projects/DogHeartworm/LandCoverData_WithVarsCalculated/DogHeartworm_LandCoverVariables_AedesAegypti.csv", header= T)
Land_AeAlbopictus = read.csv("~/Documents/Current_Projects/DogHeartworm/LandCoverData_WithVarsCalculated/DogHeartworm_LandCoverVariables_AedesAlbopictus.csv", header= T)
Land_AnFreeborni = read.csv("~/Documents/Current_Projects/DogHeartworm/LandCoverData_WithVarsCalculated/DogHeartworm_LandCoverVariables_AnFreeborni.csv", header= T)
Land_CsIncidens = read.csv("~/Documents/Current_Projects/DogHeartworm/LandCoverData_WithVarsCalculated/DogHeartworm_LandCoverVariables_CsIncidens.csv", header= T)
Land_CsInornata= read.csv("~/Documents/Current_Projects/DogHeartworm/LandCoverData_WithVarsCalculated/DogHeartworm_LandCoverVariables_CsInornata.csv", header= T)
Land_CxQuinque = read.csv("~/Documents/Current_Projects/DogHeartworm/LandCoverData_WithVarsCalculated/DogHeartworm_LandCoverVariables_CxQuinque.csv", header= T)
Land_CxTarsalis = read.csv("~/Documents/Current_Projects/DogHeartworm/LandCoverData_WithVarsCalculated/DogHeartworm_LandCoverVariables_WorkAround_CxTarsalis.csv", header= T)

# Link mosquito surveillacne, climate, and land cover data
AeVexans = cbind(Clim_AeVexans, Land_AeVexans[,c(21:38)])
AeSierrensis = cbind(Clim_AeSierrensis, Land_AeSierrensis[,c(21:38)])
AeAegypti = cbind(Clim_AeAegypti, Land_AeAegypti[,c(21:38)])
AeAlbopictus = cbind(Clim_AeAlbopictus, Land_AeAlbopictus[,c(21:38)])
AnFreeborni = cbind(Clim_AnFreeborni, Land_AnFreeborni[,c(21:38)])
CsIncidens = cbind(Clim_CsIncidens, Land_CsIncidens[,c(21:38)])
CsInornata = cbind(Clim_CsInornata, Land_CsInornata[,c(21:38)])
CxQuinque = cbind(Clim_CxQuinque, Land_CxQuinque[,c(21:38)])
CxTarsalis = cbind(Clim_CxTarsalis, Land_CxTarsalis[,c(21:38)])

# Create column for week and month of surveillance (will use in GBM), and re-order
library(lubridate)
AeVexans$week = week(AeVexans$collection_date)
AeVexans$month = month(AeVexans$collection_date)
AeSierrensis$week = week(AeSierrensis$collection_date)
AeSierrensis$month = month(AeSierrensis$collection_date)
AeAegypti$week = week(AeAegypti$collection_date)
AeAegypti$month = month(AeAegypti$collection_date)
AeAlbopictus$week = week(AeAlbopictus$collection_date)
AeAlbopictus$month = month(AeAlbopictus$collection_date)
AnFreeborni$week = week(AnFreeborni$collection_date)
AnFreeborni$month = month(AnFreeborni$collection_date)
CsIncidens$week = week(CsIncidens$collection_date)
CsIncidens$month = month(CsIncidens$collection_date)
CsInornata$week = week(CsInornata$collection_date)
CsInornata$month = month(CsInornata$collection_date)
CxQuinque$week = week(CxQuinque$collection_date)
CxQuinque$month = month(CxQuinque$collection_date)
CxTarsalis$week = week(CxTarsalis$collection_date)
CxTarsalis$month = month(CxTarsalis$collection_date)

# re-order columns for ease later
AeVexans = AeVexans[, c(1:4, 98,99, 5:97)]
AeSierrensis = AeSierrensis[, c(1:4, 98,99, 5:97)]
AeAegypti = AeAegypti[, c(1:4, 98,99, 5:97)]
AeAlbopictus = AeAlbopictus[, c(1:4, 98,99, 5:97)]
AnFreeborni = AnFreeborni[, c(1:4, 98,99, 5:97)]
CsIncidens = CsIncidens[, c(1:4, 98,99, 5:97)]
CsInornata = CsInornata[, c(1:4, 98,99, 5:97)]
CxQuinque = CxQuinque[, c(1:4, 98,99, 5:97)]
CxTarsalis = CxTarsalis[, c(1:4, 98,99, 5:97)]


# Output #
# Output each above df as file into Q2_SpeciesDatasets_WithAllPredictors folder
# Example: write.csv(AeVexans, "~/Documents/Current_Projects/DogHeartworm/Q2_SpeciesDatasets_WithAllPredictors/AeVexans_Q2_AllPredictors.csv")
# placeholder
write.csv(CxTarsalis, "~/Documents/Current_Projects/DogHeartworm/Q2_SpeciesDatasets_WithAllPredictors/CxTarsalis_Q2_AllPredictors.csv")


#### Part 2: Data pre-processing #####

# keep only rows with 1 trap and 1 trap night used
# Remove any counties contributing < 3 observations
# Remove any rows containing a trap type used <3 times
# checking for extreme skewness (i.e. skewness >3 and log transform these (log(x+ min(x) + 1)))

#### Part 2a: Pre-processing for Aedes Vexans ####

AeVexans = read.csv("~/Documents/Current_Projects/DogHeartworm/Q2_SpeciesDatasets_WithAllPredictors/Pre-Cleaning/AeVexans_Q2_AllPredictors.csv", header = T)[,-1]

# keep only rows with 1 trap(night) used
AeVexans = AeVexans[AeVexans$trap_nights == 1,]
AeVexans = AeVexans[AeVexans$num_trap == 1,]

# remove rare counties & trap types
AeVexansLowCounties = c("Butte County MVCD", "Owens Valley MAP", "Tulare MAD", "San Diego County Dept. of Environmental Health Vector Control")
AeVexans = subset(AeVexans, ! (name %in% AeVexansLowCounties))
AeVexansLowTraps = c("Mosquito magnet trap")
AeVexans = subset(AeVexans, ! (trap %in% AeVexansLowTraps))

#  assess skewness of temp / land cover variables
AeVexansSkewed = as.numeric(lapply(AeVexans[,22:99], FUN = skewness))
AeVexansWhichSkewed = which(AeVexansSkewed > 3 | AeVexansSkewed < -3)

# transform skewed variables: log(x + min(x) + 1) transformation
logPmin1 = function(x) {
  return(log(x+abs(min(x))+1)) }
AeVexans[,(AeVexansWhichSkewed+21)] = lapply(AeVexans[,(AeVexansWhichSkewed+21)], FUN = logPmin1)

# update column names to indicate which were logged
names(AeVexans)[(AeVexansWhichSkewed+21)] <- paste0(names(AeVexans)[(AeVexansWhichSkewed+21)], "_log")

# output cleaned file
write.csv(AeVexans, "~/Documents/Current_Projects/DogHeartworm/Q2_SpeciesDatasets_WithAllPredictors/AeVexans_Q2_AllPredictors.csv")

#### Part 2b: Pre-processing for Aedes Albopictus ####

AeAlbo = read.csv("~/Documents/Current_Projects/DogHeartworm/Q2_SpeciesDatasets_WithAllPredictors/Pre-Cleaning/AeAlbopictus_Q2_AllPredictors.csv", header = T)[,-1]

# keep only rows with 1 trap(night) used
AeAlbo = AeAlbo[AeAlbo$trap_nights == 1,]
AeAlbo = AeAlbo[AeAlbo$num_trap == 1,]




