# Scripts used in analysis # 

## 1. Model climate data 
File name: DogHeartworm_ModeledClimateData.R <br/>
Purpose: Write functions to pull in NOAA climate data files and output Tmax, Tmin in Celsius and precip data for relevant CA lat /longs 

## 2. Calculate relevant climate variables
File name: DogHeartworm_CalculateClimateVariables.R <br/>
Purpose: Calculates climate variables (e.g., daily/weekly/monthly/quarterly mean, max, min, diurnal ranges for temp and precip).   
Using modeled climate data from NOAA

## 3. Surveillance data cleaning and abundance calculations
File name: DogHeartworm_MosquitoSpecies.R  <br/>
Purpose: Uses CalSurv mosquito surveillance data and excludes rows with reported trap issues or incomparable trap types. 
Calculates average annual adult mosquito abundance for each species of interest

## 4. Link mosquito surveillance to climate data based on proximity
File name: DogHeartworm_LinkingMosquito&Climate.R <br/>
Purpose: Identify the nearest climate data observation for each mosquito surveillance point based on lat/longs

## 5. Calculate relevant land cover variables
File name: DogHeartworm_LandCoverDataAnalysis.R <br/>
Purpose: Use NLCD to calculate relevant land cover variables (e.g., prop forest within 100 m of surveillance point)

## 6. Clean data prior to GBM analysis for Q2
File name: DogHeartworm_Q2analysis_DataPreprocessing.R <br/>
Purpose: Merge surveillance data with climate and land cover data. Remove rows from rare counties/ trap types. Transform skewed variables

## 7. Merge predictor data prior to GBM analysis for Q1
File name: DogHeartworm_Q1analysis_DataPreprocessing.R
Purpose: Merge data on individual predictors (i.e., HDUs, median income, dog density estimates). One-year lag relevant variables
