#### Dog Heartworm Analysis: Part 1 #####
# Lisa Couper, Stanford University
# Code overview:
# 1. Load packages and data
# 2. Separate Northern and Southern CA bioregions
# 3. Run panel models 


#### 1. Load packages and data ####
library(data.table)
library(mltools)

DataA = read.csv("~/Documents/Current_Projects/DogHeartworm/Q1analysis_AllPredictors_MosqAbundance.csv", header =T)[,-1]
DataADT = data.table(DataA)
DataA2 = one_hot(DataADT)

# set NAs in mosquito abundance data to 0
DataA2$Lag_Ae.sierrensis[is.na(DataA2$Lag_Ae.sierrensis)] <- 0
DataA2$Lag_Ae.albopictus[is.na(DataA2$Lag_Ae.albopictus)] <- 0
DataA2$Lag_Ae.aegypti[is.na(DataA2$Lag_Ae.aegypti)] <- 0
DataA2$Lag_Ae.vexans[is.na(DataA2$Lag_Ae.vexans)] <- 0
DataA2$Lag_An.freeborni[is.na(DataA2$Lag_An.freeborni)] <- 0
DataA2$Lag_Cs.incidencs[is.na(DataA2$Lag_Cs.incidencs)] <- 0
DataA2$Lag_Cs.inornata[is.na(DataA2$Lag_Cs.inornata)] <- 0
DataA2$Lag_Cx.tarsalis[is.na(DataA2$Lag_Cx.tarsalis)] <- 0
DataA2$Lag_Cx.quinquefasciatus[is.na(DataA2$Lag_Cx.quinquefasciatus)] <- 0

#### 2. Separate Northern & Southern CA bioregions #####
# Separate Nor and SoCal

NorthBior = c("BayDelta", "Klamath", "Sierra", "SacramentoValley")
SouthBior = c("CentralCoast", "SanJoaquinValley", "ColoradoDesert", "SouthCoast")

DataNorth = DataA2[DataA2$Bioregion %in% NorthBior, ]
DataSouth = DataA2[DataA2$Bioregion %in% SouthBior, ]

#### 3. Run panel mdoels for Northern and Southern CA ####
# scale predictors so they are directly comparable

# Full NorCal Panel Model
NorCalpm = lm(TotalPositive ~  scale(Lag_Ae.aegypti) + scale(Lag_Ae.albopictus) + scale(Lag_Ae.sierrensis) +
                scale(Lag_Ae.vexans) + scale(Lag_An.freeborni) + scale(Lag_Cs.incidencs) + scale(Lag_Cs.inornata) +
                scale(Lag_Cx.quinquefasciatus) + scale(Lag_Cx.tarsalis) + 
                scale(Lagged_DogDensity) + scale(Lagged_Income) 
                factor(Year) + factor(Bioregion) - 1, data = DataNorth)
summary(NorCalpm)

# Full SoCal Panel model #
SoCalpm = lm(TotalPositive ~  scale(Lag_Ae.aegypti) + scale(Lag_Ae.albopictus) + scale(Lag_Ae.sierrensis) +
               scale(Lag_Ae.vexans) + scale(Lag_An.freeborni) + scale(Lag_Cs.incidencs) + scale(Lag_Cs.inornata) +
               scale(Lag_Cx.quinquefasciatus) + scale(Lag_Cx.tarsalis) + 
               scale(Lagged_DogDensity) + scale(Lagged_Income) 
               factor(Year) + factor(Bioregion) - 1, data = DataSouth)
summary(SoCalpm)

# NorCal fixed effects only
NorCalpmFEonly = lm(TotalPositive ~   
                      factor(Year) + factor(Bioregion) - 1, data = DataNorth)
summary(NorCalpmFEonly)

# SoCal fixed effects only
SoCalpmFEonly = lm(TotalPositive ~   
                      factor(Year) + factor(Bioregion) - 1, data = DataSouth)
summary(SoCalpmFEonly)

# NorCal year fixed effect only
NorCalpmYearonly = lm(TotalPositive ~   
                        factor(Year) - 1, data = DataNorth)
summary(NorCalpmYearonly)

# SoCal year fixed effect only
SoCalpmYearonly = lm(TotalPositive ~   
                        factor(Year) - 1, data = DataSouth)
summary(SoCalpmYearonly)

# NorCal bioregion fixed effect only
NorCalpmBioronly = lm(TotalPositive ~   
                        factor(Bioregion) - 1, data = DataNorth)
summary(NorCalpmBioronly)

# SoCal bioregion fixed effect only
SoCalpmBioronly = lm(TotalPositive ~   
                        factor(Bioregion) - 1, data = DataSouth)
summary(SoCalpmBioronly)


