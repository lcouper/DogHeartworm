# Main data and scripts using in dog heartworm analysis 

## Part 1: Identifying key vector species ##

### Data ###
File name: *Q1analysis_AllPredictors_MosqAbundance.csv*  
Usage: Data used to model dog heartworm cases with mosquito *abundance* as a predictor. Also includes dog density and income data

File name: *Q1analysis_AllPredictors_MosqPresence.csv*    
Usage: Data used to model dog heartworm cases with mosquito *presence* as a predictor. Also includes dog density and income data

### Scripts ###

File name: *DogHeartwormAnalysis_Part1.R*  
Usage: Script used to model dog heartworm cases with mosquito abundance as a predictor

## Part 2: Ecological drivers of vector presence ##

Note: this code was run on the Stanford cluster. If running on a personal computer, will need to lower the number of rounds &/or bootstrap iterations

### Data ###
File name: *Q2_AllSpeciesPresAbs_AllPredictors_1000.csv*  
Usage: Data used to model mosquito species presence with climate and land cover predictors
Note: The actual file is too large to upload to github. Here I have uploaded a dataset containing a random sample of 1000 rows from the original dataset. The full file used in analysis is readily available upon email request to: lcouper@stanford.edu

### Scripts ###

File name: *DogHeartwormAnalysis_Part2.R*  
Usage: Script used to model mosquito species presence with climate and land cover predictors
