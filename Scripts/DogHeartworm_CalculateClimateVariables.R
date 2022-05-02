#### DogHeartworm_CalculateClimateVariables #####

# climate variables:
# daily mean, min, max temp, diurnal temp range and precip from prior day, 2 days before, 3 days before
# weekly mean, mnin, max temp, [diurnal temp range] and precip from preceeding week, 2 weeks before, 3 weeks before
# monthly mean, mnin, max temp, [diurnal temp range] and precip from preceeding month, 2 months before, 3 months before
# quarterly mean, mnin, and max temp from preceeding quarter, 2 quarters before, 3 quarters before

# All calculations used climate variable arrays creaeted in the "DogHeartworm_ModeledClimateData.R" script
# Calculations performed on the dataset that has linked surveillance points to the modeled cliamte data,
# which was created in "DogHeartworm_LinkingMosquito&Climate.R"

LinkedData = read.csv("~/Documents/Current_Projects/DogHeartworm/Mosquito&WeatherStationLinks_AllCounties_AeSierrensis.csv", header = T)
LinkedData$collection_date = as.Date(LinkedData$collection_date, format = "%m/%d/%y")

# all functions take in a row from the linkeddata df which contains collection lat/long and date
# and outputs the value for the given climate variable (e.g., max temp 1 month prior)

### First doing only for Aedes sierrensis ####

#### 1a. Max temp from 1 day prior  ####

MT1DP = function(x)
  {KeyLong = (x$closest_longitude + 360)
  KeyLat = x$closest_latitude
  KeyDate = x$collection_date
  if (KeyDate < "1979-01-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
    {return(NA)}
  else {
  index1 = which(rownames(Tmaxs) == KeyLong)
  index2 = which(colnames(Tmaxs) == KeyLat)
  index3 = which(Dates == (KeyDate-1))
  Temperature = Tmaxs[index1, index2,index3]
  return(Temperature)}}
  
MaxTemp1DayPrior = vector()
for (i in 1:nrow(LinkedData))
{MaxTemp1DayPrior[i] = MT1DP(LinkedData[i,])}

LinkedData$MaxTemp1DayPrior = MaxTemp1DayPrior # add to data frame

#### 1b. Max temp from 2 days prior  ####

MT2DP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-01-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Tmaxs) == KeyLong)
  index2 = which(colnames(Tmaxs) == KeyLat)
  index3 = which(Dates == (KeyDate-2))
  Temperature = Tmaxs[index1, index2,index3]
  return(Temperature)}}

MaxTemp2DayPrior = vector()
for (i in 1:nrow(LinkedData))
{MaxTemp2DayPrior[i] = MT2DP(LinkedData[i,])}

LinkedData$MaxTemp2DayPrior = MaxTemp2DayPrior # add to data frame

#### 1c. Max temp from 3 days prior  ####

MT3DP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-01-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Tmaxs) == KeyLong)
  index2 = which(colnames(Tmaxs) == KeyLat)
  index3 = which(Dates == (KeyDate-3))
  Temperature = Tmaxs[index1, index2,index3]
  return(Temperature)}}

MaxTemp3DayPrior = vector()
for (i in 1:nrow(LinkedData))
{MaxTemp3DayPrior[i] = MT3DP(LinkedData[i,])}

LinkedData$MaxTemp3DayPrior = MaxTemp3DayPrior # add to data frame

#### 2a: Average max temp from 1 week prior  ####

MT1WP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-02-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Tmaxs) == KeyLong)
  index2 = which(colnames(Tmaxs) == KeyLat)
  index3 = which(Dates == (KeyDate-1))
  index4 = which(Dates == (KeyDate-7))
  Temperature = mean(Tmaxs[index1, index2,index3:index4], na.rm = T)
  return(Temperature)}}

MaxTemp1WeekPrior = vector()
for (i in 1:nrow(LinkedData))
{MaxTemp1WeekPrior[i] = MT1WP(LinkedData[i,])}

LinkedData$MaxTemp1WeekPrior = MaxTemp1WeekPrior # add to data frame

#### 2b: Average max temp from 2 weeks prior  ####

MT2WP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-02-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Tmaxs) == KeyLong)
  index2 = which(colnames(Tmaxs) == KeyLat)
  index3 = which(Dates == (KeyDate-8))
  index4 = which(Dates == (KeyDate-14))
  Temperature = mean(Tmaxs[index1, index2,index3:index4], na.rm = T)
  return(Temperature)}}

MaxTemp2WeekPrior = vector()
for (i in 1:nrow(LinkedData))
{MaxTemp2WeekPrior[i] = MT2WP(LinkedData[i,])}

LinkedData$MaxTemp2WeekPrior = MaxTemp2WeekPrior # add to data frame

#### 2c: Average max temp from 3 weeks prior  ####

MT3WP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-02-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Tmaxs) == KeyLong)
  index2 = which(colnames(Tmaxs) == KeyLat)
  index3 = which(Dates == (KeyDate-15))
  index4 = which(Dates == (KeyDate-21))
  Temperature = mean(Tmaxs[index1, index2,index3:index4], na.rm = T)
  return(Temperature)}}

MaxTemp3WeekPrior = vector()
for (i in 1:nrow(LinkedData))
{MaxTemp3WeekPrior[i] = MT3WP(LinkedData[i,])}

LinkedData$MaxTemp3WeekPrior = MaxTemp3WeekPrior # add to data frame

#### 3a: Average max temp from 1 month prior  ####

MT1MP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-04-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Tmaxs) == KeyLong)
  index2 = which(colnames(Tmaxs) == KeyLat)
  index3 = which(Dates == (KeyDate-1))
  index4 = which(Dates == (KeyDate-30))
  Temperature = mean(Tmaxs[index1, index2,index3:index4], na.rm = T)
  return(Temperature)}}

MaxTemp1MonthPrior = vector()
for (i in 1:nrow(LinkedData))
{MaxTemp1MonthPrior[i] = MT1MP(LinkedData[i,])}

LinkedData$MaxTemp1MonthPrior = MaxTemp1MonthPrior # add to data frame

#### 3b: Average max temp from 2 month prior  ####

MT2MP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-04-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Tmaxs) == KeyLong)
  index2 = which(colnames(Tmaxs) == KeyLat)
  index3 = which(Dates == (KeyDate-31))
  index4 = which(Dates == (KeyDate-60))
  Temperature = mean(Tmaxs[index1, index2,index3:index4], na.rm = T)
  return(Temperature)}}

MaxTemp2MonthPrior = vector()
for (i in 1:nrow(LinkedData))
{MaxTemp2MonthPrior[i] = MT2MP(LinkedData[i,])}

LinkedData$MaxTemp2MonthPrior = MaxTemp2MonthPrior # add to data frame

#### 3c: Average max temp from 3 month prior  ####

MT3MP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-04-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Tmaxs) == KeyLong)
  index2 = which(colnames(Tmaxs) == KeyLat)
  index3 = which(Dates == (KeyDate-61))
  index4 = which(Dates == (KeyDate-90))
  Temperature = mean(Tmaxs[index1, index2,index3:index4], na.rm = T)
  return(Temperature)}}

MaxTemp3MonthPrior = vector()
for (i in 1:nrow(LinkedData))
{MaxTemp3MonthPrior[i] = MT3MP(LinkedData[i,])}

LinkedData$MaxTemp3MonthPrior = MaxTemp3MonthPrior # add to data frame

#### 4a: Average max temp from 1 quarter prior  ####

MT1QP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-11-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Tmaxs) == KeyLong)
  index2 = which(colnames(Tmaxs) == KeyLat)
  index3 = which(Dates == (KeyDate-1))
  index4 = which(Dates == (KeyDate-90))
  Temperature = mean(Tmaxs[index1, index2,index3:index4], na.rm = T)
  return(Temperature)}}

MaxTemp1QuarterPrior = vector()
for (i in 1:nrow(LinkedData))
{MaxTemp1QuarterPrior[i] = MT1QP(LinkedData[i,])}

LinkedData$MaxTemp1QuarterPrior = MaxTemp1QuarterPrior # add to data frame

#### 4b: Average max temp from 2 quarter prior  ####

MT2QP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-11-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Tmaxs) == KeyLong)
  index2 = which(colnames(Tmaxs) == KeyLat)
  index3 = which(Dates == (KeyDate-91))
  index4 = which(Dates == (KeyDate-180))
  Temperature = mean(Tmaxs[index1, index2,index3:index4], na.rm = T)
  return(Temperature)}}

MaxTemp2QuarterPrior = vector()
for (i in 1:nrow(LinkedData))
{MaxTemp2QuarterPrior[i] = MT2QP(LinkedData[i,])}

LinkedData$MaxTemp2QuarterPrior = MaxTemp2QuarterPrior # add to data frame

#### 4c: Average max temp from 3 quarter prior  ####

MT3QP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-11-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Tmaxs) == KeyLong)
  index2 = which(colnames(Tmaxs) == KeyLat)
  index3 = which(Dates == (KeyDate-181))
  index4 = which(Dates == (KeyDate-270))
  Temperature = mean(Tmaxs[index1, index2,index3:index4], na.rm = T)
  return(Temperature)}}

MaxTemp3QuarterPrior = vector()
for (i in 1:nrow(LinkedData))
{MaxTemp3QuarterPrior[i] = MT3QP(LinkedData[i,])}

LinkedData$MaxTemp3QuarterPrior = MaxTemp3QuarterPrior # add to data frame



#### 5a. Min temp from 1 day prior  ####

MNT1DP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-01-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Tmins) == KeyLong)
  index2 = which(colnames(Tmins) == KeyLat)
  index3 = which(Datesb == (KeyDate-1))
  Temperature = Tmins[index1, index2,index3]
  return(Temperature)}}

MinTemp1DayPrior = vector()
for (i in 1:nrow(LinkedData))
{MinTemp1DayPrior[i] = MNT1DP(LinkedData[i,])}

LinkedData$MinTemp1DayPrior = MinTemp1DayPrior # add to data frame

#### 5b. Min temp from 2 days prior  ####

MNT2DP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-01-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Tmins) == KeyLong)
  index2 = which(colnames(Tmins) == KeyLat)
  index3 = which(Datesb == (KeyDate-2))
  Temperature = Tmins[index1, index2,index3]
  return(Temperature)}}

MinTemp2DayPrior = vector()
for (i in 1:nrow(LinkedData))
{MinTemp2DayPrior[i] = MNT2DP(LinkedData[i,])}

LinkedData$MinTemp2DayPrior = MinTemp2DayPrior # add to data frame

#### 5c. Min temp from 3 days prior  ####

MNT3DP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-01-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Tmins) == KeyLong)
  index2 = which(colnames(Tmins) == KeyLat)
  index3 = which(Datesb == (KeyDate-3))
  Temperature = Tmins[index1, index2,index3]
  return(Temperature)}}

MinTemp3DayPrior = vector()
for (i in 1:nrow(LinkedData))
{MinTemp3DayPrior[i] = MNT3DP(LinkedData[i,])}

LinkedData$MinTemp3DayPrior = MinTemp3DayPrior # add to data frame



#### 6a: Average min temp from 1 week prior  ####

MNT1WP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-02-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Tmins) == KeyLong)
  index2 = which(colnames(Tmins) == KeyLat)
  index3 = which(Datesb == (KeyDate-1))
  index4 = which(Datesb == (KeyDate-7))
  Temperature = mean(Tmins[index1, index2,index3:index4], na.rm = T)
  return(Temperature)}}

MinTemp1WeekPrior = vector()
for (i in 1:nrow(LinkedData))
{MinTemp1WeekPrior[i] = MNT1WP(LinkedData[i,])}

LinkedData$MinTemp1WeekPrior = MinTemp1WeekPrior # add to data frame

#### 6b: Average min temp from 2 week prior  ####

MNT2WP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-02-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Tmins) == KeyLong)
  index2 = which(colnames(Tmins) == KeyLat)
  index3 = which(Datesb == (KeyDate-8))
  index4 = which(Datesb == (KeyDate-14))
  Temperature = mean(Tmins[index1, index2,index3:index4], na.rm = T)
  return(Temperature)}}

MinTemp2WeekPrior = vector()
for (i in 1:nrow(LinkedData))
{MinTemp2WeekPrior[i] = MNT2WP(LinkedData[i,])}

LinkedData$MinTemp2WeekPrior = MinTemp2WeekPrior # add to data frame

#### 6c: Average min temp from 3 week prior  ####

MNT3WP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-02-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Tmins) == KeyLong)
  index2 = which(colnames(Tmins) == KeyLat)
  index3 = which(Datesb == (KeyDate-15))
  index4 = which(Datesb == (KeyDate-21))
  Temperature = mean(Tmins[index1, index2,index3:index4], na.rm = T)
  return(Temperature)}}

MinTemp3WeekPrior = vector()
for (i in 1:nrow(LinkedData))
{MinTemp3WeekPrior[i] = MNT3WP(LinkedData[i,])}

LinkedData$MinTemp3WeekPrior = MinTemp3WeekPrior # add to data frame




#### 7a: Average min temp from 1 month prior  ####

MNT1MP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-04-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Tmins) == KeyLong)
  index2 = which(colnames(Tmins) == KeyLat)
  index3 = which(Datesb == (KeyDate-1))
  index4 = which(Datesb == (KeyDate-30))
  Temperature = mean(Tmins[index1, index2,index3:index4], na.rm = T)
  return(Temperature)}}

MinTemp1MonthPrior = vector()
for (i in 1:nrow(LinkedData))
{MinTemp1MonthPrior[i] = MNT1MP(LinkedData[i,])}

LinkedData$MinTemp1MonthPrior = MinTemp1MonthPrior # add to data frame

#### 7b: Average min temp from 2 month prior  ####

MNT2MP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-04-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Tmins) == KeyLong)
  index2 = which(colnames(Tmins) == KeyLat)
  index3 = which(Datesb == (KeyDate-31))
  index4 = which(Datesb == (KeyDate-60))
  Temperature = mean(Tmins[index1, index2,index3:index4], na.rm = T)
  return(Temperature)}}

MinTemp2MonthPrior = vector()
for (i in 1:nrow(LinkedData))
{MinTemp2MonthPrior[i] = MNT2MP(LinkedData[i,])}

LinkedData$MinTemp2MonthPrior = MinTemp2MonthPrior # add to data frame

#### 7c: Average min temp from 3 month prior  ####

MNT3MP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-04-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Tmins) == KeyLong)
  index2 = which(colnames(Tmins) == KeyLat)
  index3 = which(Datesb == (KeyDate-61))
  index4 = which(Datesb == (KeyDate-90))
  Temperature = mean(Tmins[index1, index2,index3:index4], na.rm = T)
  return(Temperature)}}

MinTemp3MonthPrior = vector()
for (i in 1:nrow(LinkedData))
{MinTemp3MonthPrior[i] = MNT3MP(LinkedData[i,])}

LinkedData$MinTemp3MonthPrior = MinTemp3MonthPrior # add to data frame






#### 8a: Average min temp from 1 quarter prior  ####

MNT1QP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-11-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Tmins) == KeyLong)
  index2 = which(colnames(Tmins) == KeyLat)
  index3 = which(Datesb == (KeyDate-1))
  index4 = which(Datesb == (KeyDate-90))
  Temperature = mean(Tmins[index1, index2,index3:index4], na.rm = T)
  return(Temperature)}}

MinTemp1QuarterPrior = vector()
for (i in 1:nrow(LinkedData))
{MinTemp1QuarterPrior[i] = MNT1QP(LinkedData[i,])}

LinkedData$MinTemp1QuarterPrior = MinTemp1QuarterPrior # add to data frame

#### 8b: Average min temp from 2 quarter prior  ####

MNT2QP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-11-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Tmins) == KeyLong)
  index2 = which(colnames(Tmins) == KeyLat)
  index3 = which(Datesb == (KeyDate-91))
  index4 = which(Datesb == (KeyDate-180))
  Temperature = mean(Tmins[index1, index2,index3:index4], na.rm = T)
  return(Temperature)}}

MinTemp2QuarterPrior = vector()
for (i in 1:nrow(LinkedData))
{MinTemp2QuarterPrior[i] = MNT2QP(LinkedData[i,])}

LinkedData$MinTemp2QuarterPrior = MinTemp2QuarterPrior # add to data frame

#### 8c: Average min temp from 3 quarter prior  ####

MNT3QP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-11-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Tmins) == KeyLong)
  index2 = which(colnames(Tmins) == KeyLat)
  index3 = which(Datesb == (KeyDate-181))
  index4 = which(Datesb == (KeyDate-270))
  Temperature = mean(Tmins[index1, index2,index3:index4], na.rm = T)
  return(Temperature)}}

MinTemp3QuarterPrior = vector()
for (i in 1:nrow(LinkedData))
{MinTemp3QuarterPrior[i] = MNT3QP(LinkedData[i,])}

LinkedData$MinTemp3QuarterPrior = MinTemp3QuarterPrior # add to data frame





#### 9a: Precip from 1 day prior ####
P1DP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-01-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Precips) == KeyLong)
  index2 = which(colnames(Precips) == KeyLat)
  index3 = which(Datesc == (KeyDate-1))
  Precipitation = Precips[index1, index2,index3]
  return(Precipitation)}}

Precip1DayPrior = vector()
for (i in 1:nrow(LinkedData))
{Precip1DayPrior[i] = P1DP(LinkedData[i,])}

LinkedData$Precip1DayPrior = Precip1DayPrior # add to data frame

#### 9b: Precip from 2 day prior ####
P2DP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-01-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Precips) == KeyLong)
  index2 = which(colnames(Precips) == KeyLat)
  index3 = which(Datesc == (KeyDate-2))
  Precipitation = Precips[index1, index2,index3]
  return(Precipitation)}}

Precip2DayPrior = vector()
for (i in 1:nrow(LinkedData))
{Precip2DayPrior[i] = P2DP(LinkedData[i,])}

LinkedData$Precip2DayPrior = Precip2DayPrior # add to data frame

#### 9c: Precip from 2 day prior ####
P3DP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-01-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Precips) == KeyLong)
  index2 = which(colnames(Precips) == KeyLat)
  index3 = which(Datesc == (KeyDate-3))
  Precipitation = Precips[index1, index2,index3]
  return(Precipitation)}}

Precip3DayPrior = vector()
for (i in 1:nrow(LinkedData))
{Precip3DayPrior[i] = P3DP(LinkedData[i,])}

LinkedData$Precip3DayPrior = Precip3DayPrior # add to data frame


#### 10a: Average (daily total) precip from 1 week prior  ####

P1WP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-02-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Precips) == KeyLong)
  index2 = which(colnames(Precips) == KeyLat)
  index3 = which(Datesc == (KeyDate-1))
  index4 = which(Datesc == (KeyDate-7))
  Precipitation = mean(Precips[index1, index2,index3:index4], na.rm = T)
  return(Precipitation)}}

Precip1WeekPrior = vector()
for (i in 1:nrow(LinkedData))
{Precip1WeekPrior[i] = P1WP(LinkedData[i,])}

LinkedData$Precip1WeekPrior = Precip1WeekPrior # add to data frame

#### 10b: Average (daily total) precip from 2 week prior  ####

P2WP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-02-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Precips) == KeyLong)
  index2 = which(colnames(Precips) == KeyLat)
  index3 = which(Datesc == (KeyDate-8))
  index4 = which(Datesc == (KeyDate-14))
  Precipitation = mean(Precips[index1, index2,index3:index4], na.rm = T)
  return(Precipitation)}}

Precip2WeekPrior = vector()
for (i in 1:nrow(LinkedData))
{Precip2WeekPrior[i] = P2WP(LinkedData[i,])}

LinkedData$Precip2WeekPrior = Precip2WeekPrior # add to data frame

#### 10c: Average (daily total) precip from 3 week prior  ####

P3WP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-02-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Precips) == KeyLong)
  index2 = which(colnames(Precips) == KeyLat)
  index3 = which(Datesc == (KeyDate-15))
  index4 = which(Datesc == (KeyDate-21))
  Precipitation = mean(Precips[index1, index2,index3:index4], na.rm = T)
  return(Precipitation)}}

Precip3WeekPrior = vector()
for (i in 1:nrow(LinkedData))
{Precip3WeekPrior[i] = P3WP(LinkedData[i,])}

LinkedData$Precip3WeekPrior = Precip3WeekPrior # add to data frame


#### 11a: Average (daily total) precip from 1 month prior ####

P1MP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-04-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Precips) == KeyLong)
  index2 = which(colnames(Precips) == KeyLat)
  index3 = which(Datesc == (KeyDate-1))
  index4 = which(Datesc == (KeyDate-30))
  Precipitation = mean(Precips[index1, index2,index3:index4], na.rm = T)
  return(Precipitation)}}

Precip1MonthPrior = vector()
for (i in 1:nrow(LinkedData))
{Precip1MonthPrior[i] = P1MP(LinkedData[i,])}

LinkedData$Precip1MonthPrior = Precip1MonthPrior # add to data frame

#### 11b: Average (daily total) precip from 2 month prior ####

P2MP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-04-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Precips) == KeyLong)
  index2 = which(colnames(Precips) == KeyLat)
  index3 = which(Datesc == (KeyDate-31))
  index4 = which(Datesc == (KeyDate-60))
  Precipitation = mean(Precips[index1, index2,index3:index4], na.rm = T)
  return(Precipitation)}}

Precip2MonthPrior = vector()
for (i in 1:nrow(LinkedData))
{Precip2MonthPrior[i] = P2MP(LinkedData[i,])}

LinkedData$Precip2MonthPrior = Precip2MonthPrior # add to data frame

#### 11c: Average (daily total) precip from 3 month prior ####

P3MP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-04-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Precips) == KeyLong)
  index2 = which(colnames(Precips) == KeyLat)
  index3 = which(Datesc == (KeyDate-61))
  index4 = which(Datesc == (KeyDate-90))
  Precipitation = mean(Precips[index1, index2,index3:index4], na.rm = T)
  return(Precipitation)}}

Precip3MonthPrior = vector()
for (i in 1:nrow(LinkedData))
{Precip3MonthPrior[i] = P3MP(LinkedData[i,])}

LinkedData$Precip3MonthPrior = Precip3MonthPrior # add to data frame


#### 12a: Average (daily total) precip from 1 quarter prior  ####

P1QP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-11-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Precips) == KeyLong)
  index2 = which(colnames(Precips) == KeyLat)
  index3 = which(Datesc == (KeyDate-1))
  index4 = which(Datesc == (KeyDate-90))
  Precipitation = mean(Precips[index1, index2,index3:index4], na.rm = T)
  return(Precipitation)}}

Precip1QuarterPrior = vector()
for (i in 1:nrow(LinkedData))
{Precip1QuarterPrior[i] = P1QP(LinkedData[i,])}

LinkedData$Precip1QuarterPrior = Precip1QuarterPrior # add to data frame

#### 12b: Average (daily total) precip from 2 quarter prior  ####

P2QP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-11-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Precips) == KeyLong)
  index2 = which(colnames(Precips) == KeyLat)
  index3 = which(Datesc == (KeyDate-91))
  index4 = which(Datesc == (KeyDate-180))
  Precipitation = mean(Precips[index1, index2,index3:index4], na.rm = T)
  return(Precipitation)}}

Precip2QuarterPrior = vector()
for (i in 1:nrow(LinkedData))
{Precip2QuarterPrior[i] = P2QP(LinkedData[i,])}

LinkedData$Precip2QuarterPrior = Precip2QuarterPrior # add to data frame

#### 12c: Average (daily total) precip from 3 quarter prior  ####

P3QP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-11-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Precips) == KeyLong)
  index2 = which(colnames(Precips) == KeyLat)
  index3 = which(Datesc == (KeyDate-181))
  index4 = which(Datesc == (KeyDate-270))
  Precipitation = mean(Precips[index1, index2,index3:index4], na.rm = T)
  return(Precipitation)}}

Precip3QuarterPrior = vector()
for (i in 1:nrow(LinkedData))
{Precip3QuarterPrior[i] = P3QP(LinkedData[i,])}

LinkedData$Precip3QuarterPrior = Precip3QuarterPrior # add to data frame





#### 13a: Diurnal temp variation from 1 day prior ####

DT1DP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-01-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(DiurnalT) == KeyLong)
  index2 = which(colnames(DiurnalT) == KeyLat)
  index3 = which(Dates == (KeyDate-1))
  DiurnalTemp = DiurnalT[index1, index2,index3]
  return(DiurnalTemp)}}

DiurnalTemp1DayPrior = vector()
for (i in 1:nrow(LinkedData))
{DiurnalTemp1DayPrior[i] = DT1DP(LinkedData[i,])}

LinkedData$DiurnalTemp1DayPrior = DiurnalTemp1DayPrior # add to data frame

#### 13b: Diurnal temp variation from 2 day prior ####

DT2DP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-01-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(DiurnalT) == KeyLong)
  index2 = which(colnames(DiurnalT) == KeyLat)
  index3 = which(Dates == (KeyDate-2))
  DiurnalTemp = DiurnalT[index1, index2,index3]
  return(DiurnalTemp)}}

DiurnalTemp2DayPrior = vector()
for (i in 1:nrow(LinkedData))
{DiurnalTemp2DayPrior[i] = DT2DP(LinkedData[i,])}

LinkedData$DiurnalTemp2DayPrior = DiurnalTemp2DayPrior # add to data frame

#### 13c: Diurnal temp variation from 3 day prior ####

DT3DP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-01-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(DiurnalT) == KeyLong)
  index2 = which(colnames(DiurnalT) == KeyLat)
  index3 = which(Dates == (KeyDate-3))
  DiurnalTemp = DiurnalT[index1, index2,index3]
  return(DiurnalTemp)}}

DiurnalTemp3DayPrior = vector()
for (i in 1:nrow(LinkedData))
{DiurnalTemp3DayPrior[i] = DT3DP(LinkedData[i,])}

LinkedData$DiurnalTemp3DayPrior = DiurnalTemp3DayPrior # add to data frame



#### 14a: Average diurnal temp from 1 week prior  ####

DT1WP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-02-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(DiurnalT) == KeyLong)
  index2 = which(colnames(DiurnalT) == KeyLat)
  index3 = which(Dates == (KeyDate-1))
  index4 = which(Dates == (KeyDate-7))
  DiurnalTemp = mean(DiurnalT[index1, index2,index3:index4], na.rm = T)
  return(DiurnalTemp)}}

DiurnalTemp1WeekPrior = vector()
for (i in 1:nrow(LinkedData))
{DiurnalTemp1WeekPrior[i] = DT1WP(LinkedData[i,])}

LinkedData$DiurnalTemp1WeekPrior = DiurnalTemp1WeekPrior # add to data frame

#### 14b: Average diurnal temp from 2 week prior  ####
LinkedData$DiurnalTemp2WeekPrior = LinkedData$MaxTemp2WeekPrior - LinkedData$MinTemp2WeekPrior 

#### 14c: Average diurnal temp from 3 week prior  ####
LinkedData$DiurnalTemp3WeekPrior = LinkedData$MaxTemp3WeekPrior - LinkedData$MinTemp3WeekPrior 

#### 15a: Average diurnal temp from 1 month prior  ####
LinkedData$DiurnalTemp1MonthPrior = LinkedData$MaxTemp1MonthPrior - LinkedData$MinTemp1MonthPrior 

#### 15b: Average diurnal temp from 2 month prior  ####
LinkedData$DiurnalTemp2MonthPrior = LinkedData$MaxTemp2MonthPrior - LinkedData$MinTemp2MonthPrior 

#### 15c: Average diurnal temp from 3 month prior  ####
LinkedData$DiurnalTemp3MonthPrior = LinkedData$MaxTemp3MonthPrior - LinkedData$MinTemp3MonthPrior 



#### 16a: Average diurnal temp from 1 quarter prior  ####
LinkedData$DiurnalTemp1QuarterPrior = LinkedData$MaxTemp1QuarterPrior - LinkedData$MinTemp1QuarterPrior

#### 16b: Average diurnal temp from 2 quarter prior  ####
LinkedData$DiurnalTemp2QuarterPrior = LinkedData$MaxTemp2QuarterPrior - LinkedData$MinTemp2QuarterPrior

#### 16c: Average diurnal temp from 3 quarter prior  ####
LinkedData$DiurnalTemp3QuarterPrior = LinkedData$MaxTemp3QuarterPrior - LinkedData$MinTemp3QuarterPrior




#### 17a: Mean temp from 1 day prior ####

MT1DP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-01-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(MeanT) == KeyLong)
  index2 = which(colnames(MeanT) == KeyLat)
  index3 = which(Dates == (KeyDate-1))
  MeanTemp = MeanT[index1, index2,index3]
  return(MeanTemp)}}

MeanTemp1DayPrior = vector()
for (i in 1:nrow(LinkedData))
{MeanTemp1DayPrior[i] = MT1DP(LinkedData[i,])}

LinkedData$MeanTemp1DayPrior = MeanTemp1DayPrior # add to data frame

#### 17b: Mean temp from 2 day prior ####
LinkedData$MeanTemp2DayPrior = (LinkedData$MaxTemp2DayPrior + LinkedData$MinTemp2DayPrior)/ 2

#### 17c: Mean temp from 3 day prior ####
LinkedData$MeanTemp3DayPrior = (LinkedData$MaxTemp3DayPrior + LinkedData$MinTemp3DayPrior)/ 2

#### 18a: Average mean temp from 1 week prior  ####

MT1WP = function(x)
{KeyLong = (x$closest_longitude + 360)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-02-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(MeanT) == KeyLong)
  index2 = which(colnames(MeanT) == KeyLat)
  index3 = which(Dates == (KeyDate-1))
  index4 = which(Dates == (KeyDate-7))
  MeanTemp = mean(MeanT[index1, index2,index3:index4], na.rm = T)
  return(MeanTemp)}}

MeanTemp1WeekPrior = vector()
for (i in 1:nrow(LinkedData))
{MeanTemp1WeekPrior[i] = MT1WP(LinkedData[i,])}

LinkedData$MeanTemp1WeekPrior = MeanTemp1WeekPrior # add to data frame

#### 18b: Average mean temp from 2 week prior  ####
LinkedData$MeanTemp2WeekPrior = (LinkedData$MaxTemp2WeekPrior + LinkedData$MinTemp2WeekPrior)/2

#### 18c: Average mean temp from 3 week prior  ####
LinkedData$MeanTemp3WeekPrior = (LinkedData$MaxTemp3WeekPrior + LinkedData$MinTemp3WeekPrior)/2

#### 19a: Average mean temp from 1 month prior  ####
LinkedData$MeanTemp1MonthPrior = (LinkedData$MaxTemp1MonthPrior + LinkedData$MinTemp1MonthPrior)/2

#### 19b: Average mean temp from 2 month prior  ####
LinkedData$MeanTemp2MonthPrior = (LinkedData$MaxTemp2MonthPrior + LinkedData$MinTemp2MonthPrior)/2

#### 19c: Average mean temp from 3 month prior  ####
LinkedData$MeanTemp3MonthPrior = (LinkedData$MaxTemp3MonthPrior + LinkedData$MinTemp3MonthPrior)/2

#### 20a: Average mean temp from 1 quarter prior  ####
LinkedData$MeanTemp1QuarterPrior = (LinkedData$MaxTemp1QuarterPrior + LinkedData$MinTemp1QuarterPrior)/2

#### 20b: Average mean temp from 2 quarter prior  ####
LinkedData$MeanTemp2QuarterPrior = (LinkedData$MaxTemp2QuarterPrior + LinkedData$MinTemp2QuarterPrior)/2

#### 20c: Average mean temp from 3 quarter prior  ####
LinkedData$MeanTemp3QuarterPrior = (LinkedData$MaxTemp3QuarterPrior + LinkedData$MinTemp3QuarterPrior)/2


# output data file
write.csv(LinkedData, "~/Documents/Current_Projects/DogHeartworm/AedesSierrensis_SurveillanceData_WithClimVariables.csv")


