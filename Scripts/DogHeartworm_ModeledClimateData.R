#### DogHeartworm_ModeledClimateData ##### 

# 1. Write functions that pull in climate data files and output data in Celsius for CA lat/longs
# 2. Run functions for Tmax, Tmin, and precip data
# 2a. Tmax data
# 2b. Tmin data
# 2c. Precip data

library(ncdf4)
library(ncdf4.helpers)
library(weathermetrics)
library(chron)
library(lattice)
library(tidyr)

# Min temp files stored in DogHeartworm/ModeledClimateData/Tmin
# Max temp files stored in DogHeartworm/ModeledClimateData/Tmax
# Precip files stored in DogHeartworm/ModeledClimateData/Precip

#### 1. Function that takes in file path & outputs temp data in Celsius for CA lat/longs #####

CAmaxtemps = function(x)
{OpenFile = nc_open(x) 
lonindex = 461:521 # roughly CA longitudes (240 to 260)
latindex = 92:125 # roughly CA latitudes (28 to 44)
Temp_array = ncvar_get(OpenFile, "tmax") # pull out min temp data
CAmaxtempStore = Temp_array[lonindex, latindex, ]
rownames(CAmaxtempStore) = longitude[lonindex]
colnames(CAmaxtempStore) = latitude[latindex]
return(CAmaxtempStore)
}

CAmintemps = function(x)
{OpenFile = nc_open(x) 
lonindex = 461:521 # roughly CA longitudes (240 to 260)
latindex = 92:125 # roughly CA latitudes (28 to 44)
Temp_array = ncvar_get(OpenFile, "tmin") # pull out min temp data
CAmintempStore = Temp_array[lonindex, latindex, ]
rownames(CAmintempStore) = longitude[lonindex]
colnames(CAmintempStore) = latitude[latindex]
return(CAmintempStore)
}

CAprecip = function(x)
{OpenFile = nc_open(x) 
lonindex = 461:521 # roughly CA longitudes (240 to 260)
latindex = 92:125 # roughly CA latitudes (28 to 44)
Precip_array = ncvar_get(OpenFile, "precip") # pull out min temp data
CAprecipStore = Precip_array[lonindex, latindex, ]
rownames(CAprecipStore) = longitude[lonindex]
colnames(CAprecipStore) = latitude[latindex]
return(CAprecipStore)
}

#### 2a. Pull in and compile max temp data #####

filenames <- list.files("~/Documents/Current_Projects/DogHeartworm/ModeledClimateData/Tmax/", pattern="*.nc")
listoffiles = paste("~/Documents/Current_Projects/DogHeartworm/ModeledClimateData/Tmax/", filenames, sep="")

datalist = list()
for (i in 1:length(listoffiles))
{datalist[[i]] <- CAmaxtemps(listoffiles[i])}

# Bind arrays together so that third dimension (time runs continuously from day 1 year 1 onwards)
# here, each datalist[[i]] contains one year of data
Tmaxs = abind(datalist[[1]], datalist[[2]], datalist[[3]], datalist[[4]],
              datalist[[5]], datalist[[6]], datalist[[7]], datalist[[8]],
              datalist[[9]], datalist[[10]], datalist[[11]], datalist[[12]],
              datalist[[13]], datalist[[14]], datalist[[15]], datalist[[16]],
              datalist[[17]], datalist[[18]], datalist[[19]], datalist[[20]],
              datalist[[21]], datalist[[22]], datalist[[23]], datalist[[24]],
              datalist[[25]], datalist[[26]], datalist[[27]], datalist[[28]],
              datalist[[29]], datalist[[30]], datalist[[31]], datalist[[32]],
              datalist[[33]], datalist[[34]], datalist[[35]], datalist[[36]],
              datalist[[37]], datalist[[38]], datalist[[39]], datalist[[40]],
              datalist[[41]], datalist[[42]], datalist[[43]])

# Create list of the dates stored in the third dimension of the array for accessing later
# library(PCICt)
ExtractTime = function(x)
{OpenFile = nc_open(x)
TempTime <- nc.get.time.series(OpenFile, time.dim.name = "time")
TempTime = as.character.PCICt(TempTime)
Time = as.Date(TempTime, format = "%Y-%m-%d")}

DateList = list()
for (i in 1:length(listoffiles))
{DateList[[i]] <- ExtractTime(listoffiles[i])}

Dates = as.Date(unlist(DateList))
Dates = Dates[-15544] # to make same length as following Dates vector

#### 2b. Pull in and compile min temp data #####
filenamesb <- list.files("~/Documents/Current_Projects/DogHeartworm/ModeledClimateData/Tmin/", pattern="*.nc")
listoffilesb = paste("~/Documents/Current_Projects/DogHeartworm/ModeledClimateData/Tmin/", filenamesb, sep="")

datalistb = list()
for (i in 1:length(listoffilesb))
{datalistb[[i]] <- CAmintemps(listoffilesb[i])}

# Bind arrays together so that third dimension (time runs continuously from day 1 year 1 onwards)
# here, each datalist[[i]] contains one year of data
Tmins = abind(datalistb[[1]], datalistb[[2]], datalistb[[3]], datalistb[[4]],
              datalistb[[5]], datalistb[[6]], datalistb[[7]], datalistb[[8]],
              datalistb[[9]], datalistb[[10]], datalistb[[11]], datalistb[[12]],
              datalistb[[13]], datalistb[[14]], datalistb[[15]], datalistb[[16]],
              datalistb[[17]], datalistb[[18]], datalistb[[19]], datalistb[[20]],
              datalistb[[21]], datalistb[[22]], datalistb[[23]], datalistb[[24]],
              datalistb[[25]], datalistb[[26]], datalistb[[27]], datalistb[[28]],
              datalistb[[29]], datalistb[[30]], datalistb[[31]], datalistb[[32]],
              datalistb[[33]], datalistb[[34]], datalistb[[35]], datalistb[[36]],
              datalistb[[37]], datalistb[[38]], datalistb[[39]], datalistb[[40]],
              datalistb[[41]], datalistb[[42]], datalistb[[43]])

# using ExtractTime function created in 2a
DateListb = list()
for (i in 1:length(listoffilesb))
{DateListb[[i]] <- ExtractTime(listoffilesb[i])}

Datesb = as.Date(unlist(DateListb))
Datesb = Datesb[-15544]

#### 2c. Pull in and compile precip data #####

filenamesc <- list.files("~/Documents/Current_Projects/DogHeartworm/ModeledClimateData/Precip/", pattern="*.nc")
listoffilesc = paste("~/Documents/Current_Projects/DogHeartworm/ModeledClimateData/Precip/", filenamesc, sep="")

datalistc = list()
for (i in 1:length(listoffilesc))
{datalistc[[i]] <- CAprecip(listoffilesc[i])}


# Bind arrays together so that third dimension (time runs continuously from day 1 year 1 onwards)
# here, each datalist[[i]] contains one year of data
Precips = abind(datalistc[[1]], datalistc[[2]], datalistc[[3]], datalistc[[4]],
              datalistc[[5]], datalistc[[6]], datalistc[[7]], datalistc[[8]],
              datalistc[[9]], datalistc[[10]], datalistc[[11]], datalistc[[12]],
              datalistc[[13]], datalistc[[14]], datalistc[[15]], datalistc[[16]],
              datalistc[[17]], datalistc[[18]], datalistc[[19]], datalistc[[20]],
              datalistc[[21]], datalistc[[22]], datalistc[[23]], datalistc[[24]],
              datalistc[[25]], datalistc[[26]], datalistc[[27]], datalistc[[28]],
              datalistc[[29]], datalistc[[30]], datalistc[[31]], datalistc[[32]],
              datalistc[[33]], datalistc[[34]], datalistc[[35]], datalistc[[36]],
              datalistc[[37]], datalistc[[38]], datalistc[[39]], datalistc[[40]],
              datalistc[[41]], datalistc[[42]], datalistc[[43]])

# using ExtractTime function created in 2a
DateListc = list()
for (i in 1:length(listoffilesc))
{DateListc[[i]] <- ExtractTime(listoffilesc[i])}

Datesc = as.Date(unlist(DateListc))

#### 3 Calculate diurnal temp range #####

DiurnalT = array(NA, dim = c(61,34,15543))
for (i in 1:61)
  for (j in 1:34)
    for (k in 1:15543)
    {DiurnalT[i,j,k] = Tmaxs[i,j,k] - Tmins[i,j,k]}
rownames(DiurnalT) = longitude[lonindex]
colnames(DiurnalT) = latitude[latindex]

# Can use dates from Tmins or Tmaxs

#### 4 Calculate mean daily temp #####

MeanT = array(NA, dim = c(61,34,15543))
for (i in 1:61)
  for (j in 1:34)
    for (k in 1:15543)
    {MeanT[i,j,k] = (Tmaxs[i,j,k] + Tmins[i,j,k])/2}
rownames(MeanT) = longitude[lonindex]
colnames(MeanT) = latitude[latindex]

