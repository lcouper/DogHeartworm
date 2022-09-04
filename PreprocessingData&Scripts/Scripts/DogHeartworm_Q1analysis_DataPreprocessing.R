#### Dog Heartworm Question 1 Analysis: Data Pre-processing ####

# Pre-processing steps involved:

# merge: individual predictor data sets (e.g., pop size, HDUs, income, etc)
# lag predictors by one year
# calculate presence / absence column from abundance data

#### 1. Load libraries & bring in datasets for processing #####

library(reshape2)
library(dplyr)

# Predictor data files # 

Data1 = read.csv("~/Documents/Current_Projects/DogHeartworm/Merged_Cases&Mosquitoes_OutliersRemoved.csv", header = T)
Data2 = read.csv("~/Documents/Current_Projects/DogHeartworm/Predictors/DogHeartworm_MedianHouseholdIncome.csv", header = T)
Data3 = read.csv("~/Documents/Current_Projects/DogHeartworm/Predictors/DogHeartworm_CountyPopSizes_2009_2021.csv", header = T)
Data4 = read.csv("~/Documents/Current_Projects/DogHeartworm/Predictors/DogHeartworm_HDUs_ByCountyandYear.csv", header = T)
Data5 = read.csv("~/Documents/Current_Projects/DogHeartworm/Predictors/DogHeartworm_TotalPrecip_ByCountyandYear.csv", header = T)
Data6 = read.csv("~/Documents/Current_Projects/DogHeartworm/Predictors/CountytoBioregionLinker.csv", header = T)

#### Rescaling and lagging #####

# rescale median household income to be in 1000s of $s
Data2$MedianHouseholdIncome. = Data2$MedianHouseholdIncome. / 1000
# replace with 1-year lagged version
Data2$Year = Data2$Year - 1
colnames(Data2) = c("Year", "County", "Lagged_Income")
DataTemp = merge(Data1,Data2, by = c("County", "Year"), all.x = TRUE)

# remove human pop size estimates and focus only on pop density & dog density estimates

Data3a = Data3[,c(1,16:28)]
Data3b = Data3[,c(1,55:67)]

# reshape & change labels to appropriate years
Data3am = melt(Data3a)
Data3bm = melt(Data3b, id = "County")
colnames(Data3am) = c("County", "Year", "Lagged_PopDensity")
colnames(Data3bm) = c("County", "Year", "Lagged_DogDensity")

# Lag density
Data3am$Year = recode(Data3am$Year, 'Density.Year.2009' = 2010,
                      'Density.Year.2010' = 2011, 'Density.Year.2011' = 2012,
                      'Density.Year.2012' = 2013, 'Density.Year.2013' = 2014,
                      'Density.Year.2014' = 2015, 'Density.Year.2015' = 2016,
                      'Density.Year.2016' = 2017, 'Density.Year.2017' = 2018,
                      'Density.Year.2018' = 2019, 'Density.Year.2019' = 2020,
                      'Density.Year.2020' = 2021)
# Lag dog density
Data3bm$Year = recode(Data3bm$Year, 'DogDensity.Year.2011' = 2012,
                      'DogDensity.Year.2012' = 2013, 'DogDensity.Year.2013' = 2014,
                      'DogDensity.Year.2014' = 2015, 'DogDensity.Year.2015' = 2016,
                      'DogDensity.Year.2016' = 2017, 'DogDensity.Year.2017' = 2018,
                      'DogDensity.Year.2018' = 2019, 'DogDensity.Year.2019' = 2020)

DataT = merge(DataTemp, Data3am, by = c("County", "Year"), all.x = TRUE)
DataT2 = merge(DataT, Data3bm, by = c("County", "Year"), all.x = TRUE)

Data4$Year = Data4$Year - 1 # lagged the HDU variables
Data5$Year = Data5$Year - 1 # lagged the precip variable

colnames(Data4) = c("County", "Year", "Lagged_HDU", "Lagged_HDUday")
colnames(Data5) = c("County", "Year", "Lagged_TotalPrecip")

DataT3 = merge(DataT2, Data4, by = c("County", "Year"), all.x = TRUE)
DataX = merge(DataT3, Data5, by = c("County", "Year"), all.x = TRUE)

DataR = merge(DataX,Data6, by = "County", all.x = TRUE)

#### Calculate proxy for surveillance effort ####

# calculate number of traps surveyed each year by each county

Species =read.csv("~/Documents/Current_Projects/DogHeartworm/Q2_AllSpeciesPresAbs_AllPredictors.csv", header = T)[,-1]
Species$name = as.factor(Species$name)
Species$trap = as.factor(Species$trap)

SpeciesTemp = Species
SpeciesTemp$count = 1

effort = aggregate(SpeciesTemp$count, by = list(SpeciesTemp$county, SpeciesTemp$surv_year), FUN = sum)
colnames(effort) = c("County", "Year", "effort")
# convert to lagged effort 
effort$Year = effort$Year -1


# Merge with Data by county and year
Data = merge(DataR, effort, by = c("County", "Year"), all.x = TRUE)


#### Output abundance dataset with all predictors ##### 
#write.csv(Data, "~/Documents/Current_Projects/DogHeartworm/Q1analysis_AllPredictors_MosqAbundance.csv")


#### Create presence/absence columns for each species #####  

Data$LagAeSierrensisPresence = !is.na(Data$Lag_Ae.sierrensis)
Data$LagAeAlbopictusPresence = !is.na(Data$Lag_Ae.albopictus)
Data$LagAeAegyptiPresence = !is.na(Data$Lag_Ae.aegypti)
Data$LagAeVexansPresence = !is.na(Data$Lag_Ae.vexans)
Data$LagAnFreeborniPresence = !is.na(Data$Lag_An.freeborni)
Data$LagCsIncidensPresence = !is.na(Data$Lag_Cs.incidencs)
Data$LagCsInornataPresence = !is.na(Data$Lag_Cs.inornata)
Data$LagCxQuinquePresence = !is.na(Data$Lag_Cx.quinquefasciatus)
Data$LagCxTarsalisPresence = !is.na(Data$Lag_Cx.tarsalis)

# remove abundance data
Data = Data[,-c(3:11)]
DataXP = merge(Data, Data6, by = "County", all.x = TRUE)
# remove duplicated bioregion column
DataXP = DataXP[,-24]


#### Output presence/absence dataset with all predictors #####
#write.csv(DataXP, "~/Documents/Current_Projects/DogHeartworm/Q1analysis_AllPredictors_MosqPresence.csv")
