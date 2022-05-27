### Dog Heartworm Q2 Analysis: Data pre-processing 2 ####
# Part 1: Combining climate, land cover, and surveillance data sets
# Part 2: Cleaning surveillance (transforming skewed variables, removing rare counties / trap types)


#### Part 1: Combining data sets for analysis #####
# Create data sheets for each species that links 
# mosquito surveillance data, & calculated climate and land cover variables 

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

# rbind individual species datsets into one dataframe 
AllSpPreds = rbind.data.frame(AeVexans, AeSierrensis, AeAegypti, AeAlbopictus, AnFreeborni,
                              CsIncidens, CsInornata, CxQuinque, CxTarsalis)

#### Rehshaping long to wide ####
# find rows from the same collection date, and lat/long where different species were found
# and store in dataframe dups
#dups = mini[duplicated(mini[,c(3:6,17,18)]),]

# keep only the rows from distinct sampling times / places

#mini= AllSpPreds[sample(1:nrow(AllSpPreds), 600000),]
#mini$ID = 1:nrow(mini)
AllSpPreds$ID = 1:nrow(AllSpPreds)
AllMosqs = spread(AllSpPreds, key = species, value = num_count, fill = 0) # adds a 0 rather than NA

# then aggregated by collection_lat, collection_long and collection_date and sum species columns
agged = aggregate(AllMosqs[,99:107],
   by = list(AllMosqs$collection_date, AllMosqs$collection_latitude, 
             AllMosqs$collection_longitude),
                  FUN = sum)

# now identify all distinct collection dates, lats/longs
unis = distinct(AllMosqs, collection_date, collection_latitude, collection_longitude,
                .keep_all= TRUE)

# Get dataframes in the same order, sorting by date, lat, long
aggedS = agged[with(agged, order(Group.1, Group.2, Group.3)), ]
unisS = unis[with(unis, order(collection_date, collection_latitude, collection_longitude)), ]

# Append aggregated columns to distinct rows to create final data frames
df = cbind(unisS[,c(1:97)],aggedS[,4:12])
dfS = df[with(df, order(collection_date, collection_latitude, collection_longitude)), ]

#### Removing rows with trap issues ####

# keep only rows with 1 trap and 1 trap night used
# Remove any counties contributing < 5 observations
# Remove any rows containing a trap type not listed in methods

dfS = dfS[dfS$num_trap == 1,]
dfS = dfS[dfS$trap_nights == 1,]
dfS = dfS[dfS$trap_problem_bit == "FALSE",]
RareTraps = c("Reiter-Cummings Gravid Trap", "Modified CDC Autocidal Gravid Ovitrap (counts)", "Biogents Gravid Aedes Trap", "Mosquito magnet trap")
dfS = subset(dfS, ! (trap %in% RareTraps))
RareCounties = c("Oroville MAD", "Cache Mosquito Abatement District", "Calaveras County Environmental Health Department", "Colusa MAD", "Presidio Trust")
dfS = subset(dfS, ! (name %in% RareCounties))


#### Check and correct for predictor skewness ####
library(timeDate)

#  assess skewness of temp / land cover variables
PredSkewed = as.numeric(lapply(dfS[,20:97], FUN = skewness, na.rm = T))
WhichPredSkewed = which(PredSkewed > 3 | PredSkewed < -3)

# transform skewed variables: log(x + min(x) + 1) transformation
logPmin1 = function(x) {
  return(log(x+abs(min(x))+1)) }

dfS[,(WhichPredSkewed+19)] = lapply(dfS[,(WhichPredSkewed+19)], FUN = logPmin1)

# update column names to indicate which were logged
names(dfS)[(WhichPredSkewed+19)] <- paste0(names(dfS)[(WhichPredSkewed+19)], "_log")

#### Output species abundance dataset with all predictors ####
# Abundance dataset
#write.csv(dfS, "~/Documents/Current_Projects/DogHeartworm/Q2_AllSpeciesAbundance_AllPredictors.csv")

#### Create df with species presence/ absence ####

# Convert to presence/absence
dfA = dfS
dfA$AedesAegypti = as.numeric(dfA$AedesAegypti > 0)
dfA$AedesAlbopictus = as.numeric(dfA$AedesAlbopictus > 0)
dfA$AedesSierrensis = as.numeric(dfA$AedesSierrensis > 0)
dfA$AedesVexans = as.numeric(dfA$AedesVexans > 0)
dfA$AnophelesFreeborni = as.numeric(dfA$AnophelesFreeborni > 0)
dfA$CulexQuinquefasciatus = as.numeric(dfA$CulexQuinquefasciatus > 0)
dfA$CulexTarsalis = as.numeric(dfA$CulexTarsalis > 0)
dfA$CulisetaIncidens = as.numeric(dfA$CulisetaIncidens > 0)
dfA$CulisetaInornata = as.numeric(dfA$CulisetaInornata > 0)

#### Output species presence/absence dataset with all predictors ####
# presence/absence dataset
#write.csv(dfA, "~/Documents/Current_Projects/DogHeartworm/Q2_AllSpeciesPresAbs_AllPredictors.csv")




