#### DogHeartworm_CalculateClimateVariables #####

# climate variables:
# daily mean, min, max temp, diurnal temp range and precip from prior day, 2 days before, 3 days before
# weekly mean, mnin, max temp, [diurnal temp range] and precip from preceeding week, 2 weeks before, 3 weeks before
# monthly mean, mnin, max temp, [diurnal temp range] and precip from preceeding month, 2 months before, 3 months before
# quarterly mean, mnin, and max temp from preceeding quarter, 2 quarters before, 3 quarters before

# All calculations used climate variable arrays creaeted in the "DogHeartworm_ModeledClimateData.R" script
# Calculations performed on the dataset that has linked surveillance points to the modeled cliamte data,
# which was created in "DogHeartworm_LinkingMosquito&Climate.R"

# all functions take in a row from the dataframe containing collection lat/long and date
# and outputs the value for the given climate variable (e.g., max temp 1 month prior)
#### Climate variable calculation functions ####
##### 1a. Max temp from 1 day prior  #####

MT1DP = function(x)
  {KeyLong = (x$closest_longitude)
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
  
##### 1b. Max temp from 2 days prior  #####

MT2DP = function(x)
{KeyLong = (x$closest_longitude)
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

##### 1c. Max temp from 3 days prior  ####

MT3DP = function(x)
{KeyLong = (x$closest_longitude)
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

##### 2a: Average max temp from 1 week prior  ####

MT1WP = function(x)
{KeyLong = (x$closest_longitude)
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

##### 2b: Average max temp from 2 weeks prior  ####

MT2WP = function(x)
{KeyLong = (x$closest_longitude)
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

##### 2c: Average max temp from 3 weeks prior  ####

MT3WP = function(x)
{KeyLong = (x$closest_longitude)
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

##### 3a: Average max temp from 1 month prior  ####

MT1MP = function(x)
{KeyLong = (x$closest_longitude)
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

##### 3b: Average max temp from 2 month prior  ####

MT2MP = function(x)
{KeyLong = (x$closest_longitude)
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

##### 3c: Average max temp from 3 month prior  ####

MT3MP = function(x)
{KeyLong = (x$closest_longitude)
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

##### 4a: Average max temp from 1 quarter prior  ####

MT1QP = function(x)
{KeyLong = (x$closest_longitude)
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

##### 4b: Average max temp from 2 quarter prior  ####

MT2QP = function(x)
{KeyLong = (x$closest_longitude)
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

##### 4c: Average max temp from 3 quarter prior  ####

MT3QP = function(x)
{KeyLong = (x$closest_longitude)
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

##### 5a. Min temp from 1 day prior  ####

MNT1DP = function(x)
{KeyLong = (x$closest_longitude)
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

##### 5b. Min temp from 2 days prior  ####

MNT2DP = function(x)
{KeyLong = (x$closest_longitude)
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

##### 5c. Min temp from 3 days prior  ####

MNT3DP = function(x)
{KeyLong = (x$closest_longitude)
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

##### 6a: Average min temp from 1 week prior  ####

MNT1WP = function(x)
{KeyLong = (x$closest_longitude)
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

##### 6b: Average min temp from 2 week prior  ####

MNT2WP = function(x)
{KeyLong = (x$closest_longitude)
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

##### 6c: Average min temp from 3 week prior  ####

MNT3WP = function(x)
{KeyLong = (x$closest_longitude)
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

##### 7a: Average min temp from 1 month prior  ####

MNT1MP = function(x)
{KeyLong = (x$closest_longitude)
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

##### 7b: Average min temp from 2 month prior  ####

MNT2MP = function(x)
{KeyLong = (x$closest_longitude)
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

##### 7c: Average min temp from 3 month prior  ####

MNT3MP = function(x)
{KeyLong = (x$closest_longitude)
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

##### 8a: Average min temp from 1 quarter prior  ####

MNT1QP = function(x)
{KeyLong = (x$closest_longitude)
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

##### 8b: Average min temp from 2 quarter prior  ####

MNT2QP = function(x)
{KeyLong = (x$closest_longitude)
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

##### 8c: Average min temp from 3 quarter prior  ####

MNT3QP = function(x)
{KeyLong = (x$closest_longitude)
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

##### 9a: Precip from 1 day prior ####
P1DP = function(x)
{KeyLong = (x$closest_longitude)
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

##### 9b: Precip from 2 day prior ####
P2DP = function(x)
{KeyLong = (x$closest_longitude)
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

##### 9c: Precip from 3 day prior ####
P3DP = function(x)
{KeyLong = (x$closest_longitude)
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

##### 10a: Total precip from 1 week prior  ####

P1WP = function(x)
{KeyLong = (x$closest_longitude)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-02-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Precips) == KeyLong)
  index2 = which(colnames(Precips) == KeyLat)
  index3 = which(Datesc == (KeyDate-1))
  index4 = which(Datesc == (KeyDate-7))
  Precipitation = sum(Precips[index1, index2,index3:index4], na.rm = T)
  return(Precipitation)}}

##### 10b: Total precip from 2 week prior  ####

P2WP = function(x)
{KeyLong = (x$closest_longitude)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-02-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Precips) == KeyLong)
  index2 = which(colnames(Precips) == KeyLat)
  index3 = which(Datesc == (KeyDate-8))
  index4 = which(Datesc == (KeyDate-14))
  Precipitation = sum(Precips[index1, index2,index3:index4], na.rm = T)
  return(Precipitation)}}

##### 10c: Total precip from 3 week prior  ####

P3WP = function(x)
{KeyLong = (x$closest_longitude)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-02-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Precips) == KeyLong)
  index2 = which(colnames(Precips) == KeyLat)
  index3 = which(Datesc == (KeyDate-15))
  index4 = which(Datesc == (KeyDate-21))
  Precipitation = sum(Precips[index1, index2,index3:index4], na.rm = T)
  return(Precipitation)}}

##### 11a: Total precip from 1 month prior ####

P1MP = function(x)
{KeyLong = (x$closest_longitude)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-04-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Precips) == KeyLong)
  index2 = which(colnames(Precips) == KeyLat)
  index3 = which(Datesc == (KeyDate-1))
  index4 = which(Datesc == (KeyDate-30))
  Precipitation = sum(Precips[index1, index2,index3:index4], na.rm = T)
  return(Precipitation)}}

##### 11b: Total precip from 2 month prior ####

P2MP = function(x)
{KeyLong = (x$closest_longitude)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-04-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Precips) == KeyLong)
  index2 = which(colnames(Precips) == KeyLat)
  index3 = which(Datesc == (KeyDate-31))
  index4 = which(Datesc == (KeyDate-60))
  Precipitation = sum(Precips[index1, index2,index3:index4], na.rm = T)
  return(Precipitation)}}

##### 11c: Total precip from 3 month prior ####

P3MP = function(x)
{KeyLong = (x$closest_longitude)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-04-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Precips) == KeyLong)
  index2 = which(colnames(Precips) == KeyLat)
  index3 = which(Datesc == (KeyDate-61))
  index4 = which(Datesc == (KeyDate-90))
  Precipitation = sum(Precips[index1, index2,index3:index4], na.rm = T)
  return(Precipitation)}}

##### 12a: Total precip from 1 quarter prior  ####

P1QP = function(x)
{KeyLong = (x$closest_longitude)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-11-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Precips) == KeyLong)
  index2 = which(colnames(Precips) == KeyLat)
  index3 = which(Datesc == (KeyDate-1))
  index4 = which(Datesc == (KeyDate-90))
  Precipitation = sum(Precips[index1, index2,index3:index4], na.rm = T)
  return(Precipitation)}}

##### 12b: Total precip from 2 quarter prior  ####

P2QP = function(x)
{KeyLong = (x$closest_longitude)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-11-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Precips) == KeyLong)
  index2 = which(colnames(Precips) == KeyLat)
  index3 = which(Datesc == (KeyDate-91))
  index4 = which(Datesc == (KeyDate-180))
  Precipitation = sum(Precips[index1, index2,index3:index4], na.rm = T)
  return(Precipitation)}}

##### 12c: Total precip from 3 quarter prior  ####

P3QP = function(x)
{KeyLong = (x$closest_longitude)
KeyLat = x$closest_latitude
KeyDate = x$collection_date
if (KeyDate < "1979-11-01" | KeyDate > "2021-07-20")  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(Precips) == KeyLong)
  index2 = which(colnames(Precips) == KeyLat)
  index3 = which(Datesc == (KeyDate-181))
  index4 = which(Datesc == (KeyDate-270))
  Precipitation = sum(Precips[index1, index2,index3:index4], na.rm = T)
  return(Precipitation)}}

##### 13a: Diurnal temp variation from 1 day prior ####

DT1DP = function(x)
{KeyLong = (x$closest_longitude)
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

##### 13b: Diurnal temp variation from 2 day prior ####

DT2DP = function(x)
{KeyLong = (x$closest_longitude)
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

##### 13c: Diurnal temp variation from 3 day prior ####

DT3DP = function(x)
{KeyLong = (x$closest_longitude)
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

##### 14a: Average diurnal temp from 1 week prior  ####

DT1WP = function(x)
{KeyLong = (x$closest_longitude)
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

# these are same as just subtracting min temp from max temp at that time scale
##### 17a: Mean temp from 1 day prior ####

MT1DP = function(x)
{KeyLong = (x$closest_longitude)
KeyLat = x$closest_latitude
Year = x$surv_year
if (Year < 1979 | Year > 2021)  # if surveillance outside of cliamte data date bounds, return NA
{return(NA)}
else {
  index1 = which(rownames(MeanT) == KeyLong)
  index2 = which(colnames(MeanT) == KeyLat)
  index3 = which(Dates == (KeyDate-1))
  MeanTemp = MeanT[index1, index2,index3]
  return(MeanTemp)}}

##### 17b: Mean temp from 2 day prior ####
# these are same as just averaging max/min temp at that time scale

#### Climate variables for Q1 Analysis ####
#### 21a: HDU ####
# Calculated for each lat/long and years 2010 - 2021 in "DogHeartworm_ModeledClimateData.R"

# Now I want to identify the lat/longs in each county 
# to calculate a county-level average HDU for each year 
# that is spatially based on where mosquitoes where surveyed

linkerfile = read.csv("~/Documents/Current_Projects/DogHeartworm/AllSpecies_SurveillanceSiteToNOAAgeos_Linker.csv", header = T)

# Pull out just the closest_latitude and closest_longitude columns
# as these link the surveillance site to the NOAA data
lf = linkerfile[,c(1,4,5)]

# add HDU values to this dataframe
lf$HDU.2010 = NA
lf$HDU.2011 = NA
lf$HDU.2012 = NA
lf$HDU.2013 = NA
lf$HDU.2014 = NA
lf$HDU.2015 = NA
lf$HDU.2016 = NA
lf$HDU.2017 = NA
lf$HDU.2018 = NA
lf$HDU.2019 = NA
lf$HDU.2020 = NA
lf$HDU.2021 = NA

# Now extract the HDU values calculated in "DogHeartworm_ModeledClimateData.R"

for (i in 1:nrow(lf))
  {RowIndex = which(rownames(AnnualHDU[,,1]) == lf$closest_longitude[i])
  ColIndex = which(colnames(AnnualHDU[,,1]) == lf$closest_latitude[i])
  lf$HDU.2010[i] = AnnualHDU[RowIndex, ColIndex, 1]
  lf$HDU.2011[i] = AnnualHDU[RowIndex, ColIndex, 2]
  lf$HDU.2012[i] = AnnualHDU[RowIndex, ColIndex, 3]
  lf$HDU.2013[i] = AnnualHDU[RowIndex, ColIndex, 4]
  lf$HDU.2014[i] = AnnualHDU[RowIndex, ColIndex, 5]
  lf$HDU.2015[i] = AnnualHDU[RowIndex, ColIndex, 6]
  lf$HDU.2016[i] = AnnualHDU[RowIndex, ColIndex, 7]
  lf$HDU.2017[i] = AnnualHDU[RowIndex, ColIndex, 8]
  lf$HDU.2018[i] = AnnualHDU[RowIndex, ColIndex, 9]
  lf$HDU.2019[i] = AnnualHDU[RowIndex, ColIndex, 10]
  lf$HDU.2020[i] = AnnualHDU[RowIndex, ColIndex, 11]
  lf$HDU.2021[i] = AnnualHDU[RowIndex, ColIndex, 12]
}

#### 21b : Timing of HDU #### 
# i.e., Julian day when 130 HDUs are accumulated
# how to code counties/years where 130 HDUs is never reached? 

lf$HDUday.2010 = NA
lf$HDUday.2011 = NA
lf$HDUday.2012 = NA
lf$HDUday.2013 = NA
lf$HDUday.2014 = NA
lf$HDUday.2015 = NA
lf$HDUday.2016 = NA
lf$HDUday.2017 = NA
lf$HDUday.2018 = NA
lf$HDUday.2019 = NA
lf$HDUday.2020 = NA
lf$HDUday.2021 = NA

for (i in 1:nrow(lf))
{RowIndex = which(rownames(HDUtiming[,,1]) == lf$closest_longitude[i])
ColIndex = which(colnames(HDUtiming[,,1]) == lf$closest_latitude[i])
lf$HDUday.2010[i] = HDUtiming[RowIndex, ColIndex, 1]
lf$HDUday.2011[i] = HDUtiming[RowIndex, ColIndex, 2]
lf$HDUday.2012[i] = HDUtiming[RowIndex, ColIndex, 3]
lf$HDUday.2013[i] = HDUtiming[RowIndex, ColIndex, 4]
lf$HDUday.2014[i] = HDUtiming[RowIndex, ColIndex, 5]
lf$HDUday.2015[i] = HDUtiming[RowIndex, ColIndex, 6]
lf$HDUday.2016[i] = HDUtiming[RowIndex, ColIndex, 7]
lf$HDUday.2017[i] = HDUtiming[RowIndex, ColIndex, 8]
lf$HDUday.2018[i] = HDUtiming[RowIndex, ColIndex, 9]
lf$HDUday.2019[i] = HDUtiming[RowIndex, ColIndex, 10]
lf$HDUday.2020[i] = HDUtiming[RowIndex, ColIndex, 11]
lf$HDUday.2021[i] = HDUtiming[RowIndex, ColIndex, 12]
}

##### 21c: Aggregate HDUs & timing of HDUs columns by county #####
lfreduced = lf[,-c(2,3)]
CountyAvgs = aggregate(.~county, lfreduced,FUN = mean, na.rm = T)

# to keep track of how many points were averaged for each county & the variances
CountySds = aggregate(.~county, lfreduced,FUN = sd, na.rm = T)

lfreduced2 = lfreduced
lfreduced2$counter = 1
CountyCounts = aggregate(counter~ county, lfreduced2,FUN = sum)

# melt so that year is its own column & separate HDU and HDU timing
library(reshape2)
CountyAvgMelt = melt(CountyAvgs)
CountyHDU = CountyAvgMelt[1:576,]
CountyHDUtiming = CountyAvgMelt[577:nrow(CountyAvgMelt),]

CountyAllHDU = cbind(CountyHDU, CountyHDUtiming[,3])
colnames(CountyAllHDU) = c("County", "Year", "HDU", "HDUday")
CountyAllHDU$Year = recode(CountyAllHDU$Year, 'HDU.2010' = 2010, 'HDU.2011' = 2011,
                           'HDU.2012' = 2012, 'HDU.2013' = 2013, 'HDU.2014' = 2014, 'HDU.2015' = 2015,
                           'HDU.2016' = 2016, 'HDU.2017' = 2017, 'HDU.2018' = 2018, 'HDU.2019' = 2019, 
                           'HDU.2020' = 2020, 'HDU.2021' = 2021)
# export this file
#write.csv(CountyAllHDU, "~/Documents/Current_Projects/DogHeartworm/Predictors/DogHeartworm_HDUs_ByCountyandYear.csv")

#### 22: Annual total precip ####
# use linker file from above
linkerfile = read.csv("~/Documents/Current_Projects/DogHeartworm/AllSpecies_SurveillanceSiteToNOAAgeos_Linker.csv", header = T)

# Pull out just the closest_latitude and closest_longitude columns
# as these link the surveillance site to the NOAA data
lf2 = linkerfile[,c(1,4,5)]

# add total precip values to this dataframe
lf2$Precip.2010 = NA
lf2$Precip.2011 = NA
lf2$Precip.2012 = NA
lf2$Precip.2013 = NA
lf2$Precip.2014 = NA
lf2$Precip.2015 = NA
lf2$Precip.2016 = NA
lf2$Precip.2017 = NA
lf2$Precip.2018 = NA
lf2$Precip.2019 = NA
lf2$Precip.2020 = NA
lf2$Precip.2021 = NA

# Now add total precip to these columns using modeled precip data 

for (i in 1:nrow(lf2))
{RowIndex = which(rownames(AnnualPrecip[,,1]) == lf2$closest_longitude[i])
ColIndex = which(colnames(AnnualPrecip[,,1]) == lf2$closest_latitude[i])
lf2$Precip.2010[i] = AnnualPrecip[RowIndex, ColIndex, 1]
lf2$Precip.2011[i] = AnnualPrecip[RowIndex, ColIndex, 2]
lf2$Precip.2012[i] = AnnualPrecip[RowIndex, ColIndex, 3]
lf2$Precip.2013[i] = AnnualPrecip[RowIndex, ColIndex, 4]
lf2$Precip.2014[i] = AnnualPrecip[RowIndex, ColIndex, 5]
lf2$Precip.2015[i] = AnnualPrecip[RowIndex, ColIndex, 6]
lf2$Precip.2016[i] = AnnualPrecip[RowIndex, ColIndex, 7]
lf2$Precip.2017[i] = AnnualPrecip[RowIndex, ColIndex, 8]
lf2$Precip.2018[i] = AnnualPrecip[RowIndex, ColIndex, 9]
lf2$Precip.2019[i] = AnnualPrecip[RowIndex, ColIndex, 10]
lf2$Precip.2020[i] = AnnualPrecip[RowIndex, ColIndex, 11]
lf2$Precip.2021[i] = AnnualPrecip[RowIndex, ColIndex, 12]
}

##### 22b: Aggregate annual precipitation by county #####

lf2reduced = lf2[,-c(2,3)]
CountyPrecipsAvgs = aggregate(.~county, lf2reduced,FUN = mean, na.rm = T) 
CountyAvgPrecip = melt(CountyPrecipsAvgs)
colnames(CountyAvgPrecip) = c("County", "Year", "AnnualPrecip")
CountyAvgPrecip$Year = recode(CountyAvgPrecip$Year, 'Precip.2010' = 2010, 'Precip.2011' = 2011,
                           'Precip.2012' = 2012, 'Precip.2013' = 2013, 'Precip.2014' = 2014, 'Precip.2015' = 2015,
                           'Precip.2016' = 2016, 'Precip.2017' = 2017, 'Precip.2018' = 2018, 'Precip.2019' = 2019, 
                           'Precip.2020' = 2020, 'Precip.2021' = 2021)

# export this file
#write.csv(CountyAvgPrecip, "~/Documents/Current_Projects/DogHeartworm/Predictors/DogHeartworm_TotalPrecip_ByCountyandYear.csv")





#### Calculate climate variables for all species ####

AllSp = read.csv("~/Documents/Current_Projects/DogHeartworm/CalSurv_Cleaned_AllSpeciesWithStationLinks.csv", header = T)
AllSp$collection_date = as.Date(AllSp$collection_date, format = "%m/%d/%y")

# Max Temp Days prior
MaxTemp1DayPrior = vector()
MaxTemp2DayPrior = vector()
MaxTemp3DayPrior = vector()

for (i in 1:nrow(AllSp))
{MaxTemp1DayPrior[i] = MT1DP(AllSp[i,])
MaxTemp2DayPrior[i] = MT2DP(AllSp[i,])
MaxTemp3DayPrior[i] = MT3DP(AllSp[i,])}

AllSp$MaxTemp1DayPrior = MaxTemp1DayPrior
AllSp$MaxTemp2DayPrior = MaxTemp2DayPrior
AllSp$MaxTemp3DayPrior = MaxTemp3DayPrior 



# MaxTemp1WeekPrior
MaxTemp1WeekPrior = vector()
for (i in 1:nrow(AllSp))
{MaxTemp1WeekPrior[i] = MT1WP(AllSp[i,])}
AllSp$MaxTemp1WeekPrior = MaxTemp1WeekPrior # add to data frame

# MaxTemp2WeekPrior
MaxTemp2WeekPrior = vector()
for (i in 1:nrow(AllSp))
{MaxTemp2WeekPrior[i] = MT2WP(AllSp[i,])}
AllSp$MaxTemp2WeekPrior = MaxTemp2WeekPrior # add to data frame

# MaxTemp3WeekPrior
MaxTemp3WeekPrior = vector()
for (i in 1:nrow(AllSp))
{MaxTemp3WeekPrior[i] = MT3WP(AllSp[i,])}
AllSp$MaxTemp3WeekPrior = MaxTemp3WeekPrior # add to data frame

# MaxTemp1MonthPrior
MaxTemp1MonthPrior = vector()
for (i in 1:nrow(AllSp))
{MaxTemp1MonthPrior[i] = MT1MP(AllSp[i,])}
AllSp$MaxTemp1MonthPrior = MaxTemp1MonthPrior # add to data frame

# MaxTemp2MonthPrior
MaxTemp2MonthPrior = vector()
for (i in 1:nrow(AllSp))
{MaxTemp2MonthPrior[i] = MT2MP(AllSp[i,])}
AllSp$MaxTemp2MonthPrior = MaxTemp2MonthPrior # add to data frame

# MaxTemp3MonthPrior
MaxTemp3MonthPrior = vector()
for (i in 1:nrow(AllSp))
{MaxTemp3MonthPrior[i] = MT3MP(AllSp[i,])}
AllSp$MaxTemp3MonthPrior = MaxTemp3MonthPrior # add to data frame

# MaxTemp1QuarterPrior
MaxTemp1QuarterPrior = vector()
for (i in 1:nrow(AllSp))
{MaxTemp1QuarterPrior[i] = MT1QP(AllSp[i,])}
AllSp$MaxTemp1QuarterPrior = MaxTemp1QuarterPrior # add to data frame

# MaxTemp2QuarterPrior
MaxTemp2QuarterPrior = vector()
for (i in 1:nrow(AllSp))
{MaxTemp2QuarterPrior[i] = MT2QP(AllSp[i,])}
AllSp$MaxTemp2QuarterPrior = MaxTemp2QuarterPrior # add to data frame

# MaxTemp3QuarterPrior
MaxTemp3QuarterPrior = vector()
for (i in 1:nrow(AllSp))
{MaxTemp3QuarterPrior[i] = MT3QP(AllSp[i,])}
AllSp$MaxTemp3QuarterPrior = MaxTemp3QuarterPrior # add to data frame

# MinTemp Days Prior
MinTemp1DayPrior = vector()
MinTemp2DayPrior = vector()
MinTemp3DayPrior = vector()

for (i in 1:nrow(AllSp))
{MinTemp1DayPrior[i] = MNT1DP(AllSp[i,])
MinTemp2DayPrior[i] = MNT2DP(AllSp[i,])
MinTemp3DayPrior[i] = MNT3DP(AllSp[i,])}

AllSp$MinTemp1DayPrior = MinTemp1DayPrior 
AllSp$MinTemp2DayPrior = MinTemp2DayPrior 
AllSp$MinTemp3DayPrior = MinTemp3DayPrior 

# MinTemp Weeks Prior
MinTemp1WeekPrior = vector()
MinTemp2WeekPrior = vector()
MinTemp3WeekPrior = vector()

for (i in 1:nrow(AllSp))
{MinTemp1WeekPrior[i] = MNT1WP(AllSp[i,])
MinTemp2WeekPrior[i] = MNT2WP(AllSp[i,])
MinTemp3WeekPrior[i] = MNT3WP(AllSp[i,])}

AllSp$MinTemp1WeekPrior = MinTemp1WeekPrior 
AllSp$MinTemp2WeekPrior = MinTemp2WeekPrior 
AllSp$MinTemp3WeekPrior = MinTemp3WeekPrior 

# MinTemp Months Prior
MinTemp1MonthPrior = vector()
MinTemp2MonthPrior = vector()
MinTemp3MonthPrior = vector()

for (i in 1:nrow(AllSp))
{MinTemp1MonthPrior[i] = MNT1MP(AllSp[i,])
MinTemp2MonthPrior[i] = MNT2MP(AllSp[i,])
MinTemp3MonthPrior[i] = MNT3MP(AllSp[i,])}

AllSp$MinTemp1MonthPrior = MinTemp1MonthPrior 
AllSp$MinTemp2MonthPrior = MinTemp2MonthPrior 
AllSp$MinTemp3MonthPrior = MinTemp3MonthPrior 

# MinTemp Quarters Prior
MinTemp1QuarterPrior = vector()
MinTemp2QuarterPrior = vector()
MinTemp3QuarterPrior = vector()

for (i in 1:nrow(AllSp))
{MinTemp1QuarterPrior[i] = MNT1QP(AllSp[i,])
MinTemp2QuarterPrior[i] = MNT2QP(AllSp[i,])
MinTemp3QuarterPrior[i] = MNT3QP(AllSp[i,])}

AllSp$MinTemp1QuarterPrior = MinTemp1QuarterPrior
AllSp$MinTemp2QuarterPrior = MinTemp2QuarterPrior
AllSp$MinTemp3QuarterPrior = MinTemp3QuarterPrior

# Precip1DayPrior
Precip1DayPrior = vector()
Precip2DayPrior = vector()
Precip3DayPrior = vector()

for (i in 1:nrow(AllSp))
{Precip1DayPrior[i] = P1DP(AllSp[i,])
Precip2DayPrior[i] = P2DP(AllSp[i,])
Precip3DayPrior[i] = P3DP(AllSp[i,])}

AllSp$Precip1DayPrior = Precip1DayPrior 
AllSp$Precip2DayPrior = Precip2DayPrior
AllSp$Precip3DayPrior = Precip3DayPrior 


# Precip Weeks Prior
Precip1WeekPrior = vector()
Precip2WeekPrior = vector()
Precip3WeekPrior = vector()

for (i in 1:nrow(AllSp))
{Precip1WeekPrior[i] = P1WP(AllSp[i,])
Precip2WeekPrior[i] = P2WP(AllSp[i,])
Precip3WeekPrior[i] = P3WP(AllSp[i,])}
AllSp$Precip1WeekPrior = Precip1WeekPrior 
AllSp$Precip2WeekPrior = Precip2WeekPrior 
AllSp$Precip3WeekPrior = Precip3WeekPrior 

# Precip1MonthPrior
Precip1MonthPrior = vector()
Precip2MonthPrior = vector()
Precip3MonthPrior = vector()

for (i in 1:nrow(AllSp))
{Precip1MonthPrior[i] = P1MP(AllSp[i,])
Precip2MonthPrior[i] = P2MP(AllSp[i,])
Precip3MonthPrior[i] = P3MP(AllSp[i,])}

AllSp$Precip1MonthPrior = Precip1MonthPrior
AllSp$Precip2MonthPrior = Precip2MonthPrior
AllSp$Precip3MonthPrior = Precip3MonthPrior

# Precip Quarters Prior 
Precip1QuarterPrior = vector()
Precip2QuarterPrior = vector()
Precip3QuarterPrior = vector()

for (i in 1:nrow(AllSp))
{Precip1QuarterPrior[i] = P1QP(AllSp[i,])
Precip2QuarterPrior[i] = P2QP(AllSp[i,])
Precip3QuarterPrior[i] = P3QP(AllSp[i,])}
AllSp$Precip1QuarterPrior = Precip1QuarterPrior
AllSp$Precip2QuarterPrior = Precip2QuarterPrior 
AllSp$Precip3QuarterPrior = Precip3QuarterPrior

# Diurnal Temps
AllSp$DiurnalTemp1DayPrior = AllSp$MaxTemp1DayPrior - AllSp$MinTemp1DayPrior 
AllSp$DiurnalTemp2DayPrior = AllSp$MaxTemp2DayPrior - AllSp$MinTemp2DayPrior 
AllSp$DiurnalTemp3DayPrior = AllSp$MaxTemp3DayPrior - AllSp$MinTemp3DayPrior 
AllSp$DiurnalTemp1WeekPrior = AllSp$MaxTemp1WeekPrior - AllSp$MinTemp1WeekPrior 
AllSp$DiurnalTemp2WeekPrior = AllSp$MaxTemp2WeekPrior - AllSp$MinTemp2WeekPrior 
AllSp$DiurnalTemp3WeekPrior = AllSp$MaxTemp3WeekPrior - AllSp$MinTemp3WeekPrior 
AllSp$DiurnalTemp1MonthPrior = AllSp$MaxTemp1MonthPrior - AllSp$MinTemp1MonthPrior 
AllSp$DiurnalTemp2MonthPrior = AllSp$MaxTemp2MonthPrior - AllSp$MinTemp2MonthPrior 
AllSp$DiurnalTemp3MonthPrior = AllSp$MaxTemp3MonthPrior - AllSp$MinTemp3MonthPrior 
AllSp$DiurnalTemp1QuarterPrior = AllSp$MaxTemp1QuarterPrior - AllSp$MinTemp1QuarterPrior 
AllSp$DiurnalTemp2QuarterPrior = AllSp$MaxTemp2QuarterPrior - AllSp$MinTemp2QuarterPrior 
AllSp$DiurnalTemp3QuarterPrior = AllSp$MaxTemp3QuarterPrior - AllSp$MinTemp3QuarterPrior 

# Mean temps
AllSp$MeanTemp1DayPrior = (AllSp$MaxTemp1DayPrior + AllSp$MinTemp1DayPrior)/ 2
AllSp$MeanTemp2DayPrior = (AllSp$MaxTemp2DayPrior + AllSp$MinTemp2DayPrior)/ 2
AllSp$MeanTemp3DayPrior = (AllSp$MaxTemp3DayPrior + AllSp$MinTemp3DayPrior)/ 2
AllSp$MeanTemp1WeekPrior = (AllSp$MaxTemp1WeekPrior + AllSp$MinTemp1WeekPrior)/ 2
AllSp$MeanTemp2WeekPrior = (AllSp$MaxTemp2WeekPrior + AllSp$MinTemp2WeekPrior)/ 2
AllSp$MeanTemp3WeekPrior = (AllSp$MaxTemp3WeekPrior + AllSp$MinTemp3WeekPrior)/ 2
AllSp$MeanTemp1MonthPrior = (AllSp$MaxTemp1MonthPrior + AllSp$MinTemp1MonthPrior)/ 2
AllSp$MeanTemp2MonthPrior = (AllSp$MaxTemp2MonthPrior + AllSp$MinTemp2MonthPrior)/ 2
AllSp$MeanTemp3MonthPrior = (AllSp$MaxTemp3MonthPrior + AllSp$MinTemp3MonthPrior)/ 2
AllSp$MeanTemp1QuarterPrior = (AllSp$MaxTemp1QuarterPrior + AllSp$MinTemp1QuarterPrior)/ 2
AllSp$MeanTemp2QuarterPrior = (AllSp$MaxTemp2QuarterPrior + AllSp$MinTemp2QuarterPrior)/ 2
AllSp$MeanTemp3QuarterPrior = (AllSp$MaxTemp3QuarterPrior + AllSp$MinTemp3QuarterPrior)/ 2

AllSp = AllSp[,c(1:31,47:58,32:43, 44:46, 59:79)]

#output
#write.csv(AllSp, "~/Documents/Current_Projects/DogHeartworm/SurveillanceData_AllSpecies_WithClimVariables")


#### Scrap: delete later. For when doing just Ae. sierrensis first ###

# Starting with just Aedes sierrensis surveillance data #
LinkedData = read.csv("~/Documents/Current_Projects/DogHeartworm/Mosquito&WeatherStationLinks_AllCounties_AeSierrensis.csv", header = T)
LinkedData$collection_date = as.Date(LinkedData$collection_date, format = "%m/%d/%y")
LinkedData$closest_longitude = (LinkedData$closest_longitude + 360) # convert long to compatible scale with NOAA lat/longs

MaxTemp1DayPrior = vector()
for (i in 1:nrow(LinkedData))
{MaxTemp1DayPrior[i] = MT1DP(LinkedData[i,])}

LinkedData$MaxTemp1DayPrior = MaxTemp1DayPrior # add to data frame
# repeat for all other functions 
