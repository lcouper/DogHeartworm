### Mosquito surveillance & weather station data linking ####

# Approach 1: using county-specific climate data from NCDC
# 1a: Pull in data, rbind into single dataframe
# 1b. remove any stations with no or very little temperature data
# 1c. Link the mosquito surveillance location with closest weather station

# Approach 2: using NOAA PSL modeled weather data (from Elle )
# data here: https://psl.noaa.gov/repository/entry/show?entryid=synth%3Ae570c8f9-ec09-4e89-93b4-babd5651e7a9%3AL2NwY19nbG9iYWxfdGVtcA%3D%3D
# 2a: Pull in climate data
# 2b: Write linking functions
# 2c: Run linking functions on subsets of data
# 2d: Link mosquito surveillance with closest NOAA data point based on lat/long

library(dplyr)
library(tidyverse)

#### 1a. Pull in county-specific climate data #####

# write function to pull in all the individual climate data files & put them into 1
my_read_csv = function(x) {
  out = read_csv(x)
  county <- str_extract(x, '(?<=_| )[^_ ]+(?=\\.csv)')
  cbind(County=county, out)
}

path = "~/Documents/Current_Projects/DogHeartworm/Complete_1979_2020"
filenames <- list.files(path, full.names = TRUE, recursive = TRUE)
AllClim <- lapply(filenames, my_read_csv) %>% bind_rows()

#### 1b.pull out good stations ####
stations = unique(AllClim$STATION)
stationdata = vector()
for (i in 1:length(stations)) {
  x = AllClim[AllClim$STATION == stations[i],]
  y = sum(as.numeric(is.na(x$TMAX)))
  z = y/nrow(x)
  stationdata[i] = z
}

# Keep only stations with at least 70% temp data
goodstations = stations[stationdata < 0.30]
AllClimGood = AllClim[AllClim$STATION %in% goodstations,]

# Read in mosquito climate data 
# this cleaned file was created in "DogHeartworm_AedesSierrensis_DataReshape.R"
ads = read.csv("~/Documents/Current_Projects/DogHeartworm/MosquitoSpeciesData/CalSurv_CleanedAeSierrensis_Data.csv", header = T)
ads$collection_date = as.Date(ads$collection_date, format = "%m/%d/%y")
ads = ads[,-1] # drop weird first column

##### 1c. Link mosquito surveillance & NCDC weather station data #####

# load packages for spatial analysis
library(sp)
library(rgeos)


# Write function to link mosquito surveillance collection site with nearest weather station location
# based on lat/longs. Also outputs the distance (in degrees) between collection & weather station

Station2Station <- function(x) {
   CountyData = ads[ads$county == x,]
   CountyLoc = ads[ads$county == x,c(16,17)]
   CountyClim = AllClimGood[AllClimGood$County == x,]
  CountyClimU = CountyClim[!duplicated(CountyClim$STATION),c(4,5)]
     colnames(CountyClimU) = c("collection_latitude", "collection_longitude")
 
  stationname = vector()
  stationdist = vector()

   for (i in 1:nrow(CountyLoc)) {
        Combined = rbind.data.frame(CountyLoc[i,], CountyClimU)
        coordinates(Combined) = ~collection_longitude+collection_latitude
        d = gDistance(Combined, byid = T)
        labelmin_temp = which.min(d[1,-1])
        Station = CountyClim[rownames(CountyClimU[labelmin_temp,]),2]
        stationname[i] = Station
        stationdist[i] = min(d[1,-1]) 
        }  

    CountyData$STATION_NAME = stationname
    CountyData$Station_Distance = stationdist
    return(CountyData)
    }

# Run the function above for each county (doing individually so I can spot check each one)

Alameda = Station2Station("Alameda") # 520 data points, 4 weather stations
Butte = Station2Station("Butte") # 898 data points, 3 weather stations
Calaveras = Station2Station("Calaveras") # 21 data points, 2 weather stations
Colusa = Station2Station("Colusa") # REMOVE - no good weather stations for this county
ContraCosta = Station2Station("ContraCosta") # 317 data points, 9 weather stations
Fresno = Station2Station("Fresno") # 525 data points, 5 weather stations -- 1 really far away
Kern = Station2Station("Kern") # 184 data points, 3 weather stations
Kings = Station2Station("Kings") # 75 data points, 2 weather stations
Lake = Station2Station("Lake") # 6926 data points, 3 weather stations
LosAngeles = Station2Station("LosAngeles") # 339 data points, 10 weather stations
Madera = Station2Station("Madera") # 16 data points, 2 weather stations
Marin = Station2Station("Marin") # 387 data points, 1 weather station
Merced = Station2Station("Merced") # 44 data points, 3 weather stations
Monterey = Station2Station("Monterey") # 93 data points, 2 weather stations
Napa = Station2Station("Napa") # 194 data points, 3 weather stations
Placer = Station2Station("Placer") # 2482 data points, 11 weather stations
Plumas = Station2Station("Plumas") # 11 data points, 1 weather station
Riverside = Station2Station("Riverside") # 75 data points, 4 weather stations
Sacramento = Station2Station("Sacramento") # 3754 data points, 2 weather stations
SanBenito = Station2Station("SanBenito") # 32 data points, 1 weather station.. pretty far away
SanBernardino = Station2Station("SanBernardino") # 166 data points, 10 weather stations
SanDiego = Station2Station("SanDiego") # 55 data points, 9 weather stations
SanJoaquin = Station2Station("SanJoaquin") # 739 data points, 2 weather stations
SanMateo = Station2Station("SanMateo") # 457 data points, 4 weather stations
SantaClara = Station2Station("SantaClara") # 92 data points, 6 weather stations
SantaCruz = Station2Station("SantaCruz") # 526 data points, 6 weather stations
Solano = Station2Station("Solano") # 231 data points, 2 weather stations
Sonoma = Station2Station("Sonoma") # 732 data points, 5 weather stations
Tehama = Station2Station("Tehama") # 72 data points, 1 weather station
Tulare = Station2Station("Tulare") # 255 data points, 3 weather stations
Ventura = Station2Station("Ventura") # 35 data points, 3 weather stations
Yolo = Station2Station("Yolo") # 1435 data points, 4 weather stations
Yuba = Station2Station("Yuba") # 351 data points, 1 weather station

### Approach 2: Match mosquito surveillance site with nearest NOAA modeled weather point ####
#### 2a: Read in climate data ####


# Read in mosquito climate data 
# this cleaned file was created in "DogHeartworm_AedesSierrensis_DataReshape.R"
ads = read.csv("~/Documents/Current_Projects/DogHeartworm/MosquitoSpeciesData/CalSurv_CleanedAeSierrensis_Data.csv", header = T)
ads$collection_date = as.Date(ads$collection_date, format = "%m/%d/%y")
ads = ads[,-1] # drop weird first column

clim = datalist[[1]][,,1] # Pull out the lat longs for a given year and date 
# Doesn't actually matter which year/ date you use since lat/longs for each will be the same
climM = melt(clim)
climM$Id = 1:nrow(climM)
colnames(climM) = c("collection_longitude", "collection_latitude", "temp", "id")

#### 2b: Write functions to match NOAA lat/long with mosquito surveillance point #####
# function for just a subset (n = 5) to see if this works 
Station2Station2subset <- function(x) {
  CountyData = ads[ads$county == x,]
  CountyData = CountyData[sample(1:nrow(CountyData), 5, replace = FALSE),]
  CountyLoc = CountyData[,16:17]
  CountyClim = climM
  CountyClim[,1] = CountyClim[,1] - 360
  CountyClimU = CountyClim[!duplicated(CountyClim[,1:2]),c(1,2)]
  stationname = vector()
  stationdist = vector()
  stationlong = vector()
  stationlat = vector()

  for (i in 1:nrow(CountyLoc)) {
    Combined = rbind.data.frame(CountyLoc[i,], CountyClimU)
    coordinates(Combined) = ~collection_longitude+collection_latitude
    d = gDistance(Combined, byid = T)
    labelmin_temp = which.min(d[1,-1])
    ID = CountyClim[labelmin_temp,4]
    stationname[i] = ID
    stationdist[i] = min(d[1,-1]) 
    stationlong[i] = CountyClimU[labelmin_temp,1]
    stationlat[i] = CountyClimU[labelmin_temp,2]
  }  
  
  CountyData$closest_longitude = stationlong
  CountyData$closest_latitude = stationlat
  CountyData$Station_Distance = stationdist
  return(CountyData)
}

# function for full data
Station2Station2 <- function(x) {
  CountyData = ads[ads$county == x,]
  CountyLoc = CountyData[,16:17]
  CountyClim = climM
  CountyClim[,1] = CountyClim[,1] - 360
  CountyClimU = CountyClim[!duplicated(CountyClim[,1:2]),c(1,2)]
  stationname = vector()
  stationdist = vector()
  stationlong = vector()
  stationlat = vector()
  
  for (i in 1:nrow(CountyLoc)) {
    Combined = rbind.data.frame(CountyLoc[i,], CountyClimU)
    coordinates(Combined) = ~collection_longitude+collection_latitude
    d = gDistance(Combined, byid = T)
    labelmin_temp = which.min(d[1,-1])
    ID = CountyClim[labelmin_temp,4]
    stationname[i] = ID
    stationdist[i] = min(d[1,-1]) 
    stationlong[i] = CountyClimU[labelmin_temp,1]
    stationlat[i] = CountyClimU[labelmin_temp,2]
  }  
  
  CountyData$closest_longitude = stationlong
  CountyData$closest_latitude = stationlat
  CountyData$Station_Distance = stationdist
  return(CountyData)
}

# function for half data (in case you need to split up)
Station2Station3 <- function(x) {
  CountyData = ads[ads$county == x,]
  CountyData = CountyData[6001:6926,]
  CountyLoc = CountyData[,16:17]
  CountyClim = climM
  CountyClim[,1] = CountyClim[,1] - 360
  CountyClimU = CountyClim[!duplicated(CountyClim[,1:2]),c(1,2)]
  stationname = vector()
  stationdist = vector()
  stationlong = vector()
  stationlat = vector()
  
  for (i in 1:nrow(CountyLoc)) {
    Combined = rbind.data.frame(CountyLoc[i,], CountyClimU)
    coordinates(Combined) = ~collection_longitude+collection_latitude
    d = gDistance(Combined, byid = T)
    labelmin_temp = which.min(d[1,-1])
    ID = CountyClim[labelmin_temp,4]
    stationname[i] = ID
    stationdist[i] = min(d[1,-1]) 
    stationlong[i] = CountyClimU[labelmin_temp,1]
    stationlat[i] = CountyClimU[labelmin_temp,2]
  }  
  
  CountyData$closest_longitude = stationlong
  CountyData$closest_latitude = stationlat
  CountyData$Station_Distance = stationdist
  return(CountyData)
}

#### 2c: Test: Comparing distance between mosquito collection stations & weather stations vs modeled data #####
# Doing just a subset of each
AlamedaSubset = Station2Station2subset("Alameda")
mean(AlamedaSubset$Station_Distance) # 0.204088 (compared to 0.126 from weather stations)
ButteSubset = Station2Station2subset("Butte") 
mean(ButteSubset$Station_Distance) # 0.1756649 (compared to 0.13658)
SacramentoSubset = Station2Station2subset("Sacramento") # slightly better for sacramento
SanBernardinoSubset = Station2Station2subset("SanBernardino")
mean(SanBernardinoSubset$Station_Distance) # 0.176 compared to 0.119 for San Bernardino

# Physical weather stations were closer on average in this small subset, but they are often missing 
# many days of data. Conclusion = use modeled data

#### 2d: Get lat/long of nearest modeled point to each mosquito surveillance site, by county #####
Alameda2 = Station2Station2("Alameda")
Butte2 = Station2Station2("Butte")
Colusa2 = Station2Station2("Colusa") 
ContraCosta2 = Station2Station2("ContraCosta") 
Fresno2 = Station2Station2("Fresno") 
Kern2 = Station2Station2("Kern") 
Kings2 = Station2Station2("Kings") 
Lake2a = Station2Station3("Lake") 
Lake2b = Station2Station3("Lake") 
Lake2c = Station2Station3("Lake") 
Lake2d = Station2Station3("Lake") 
Lake2e = Station2Station3("Lake") 
Lake2f = Station2Station3("Lake") 
Lake2g = Station2Station3("Lake") 
Lake2 = rbind(Lake2a, Lake2b, Lake2c, Lake2d, Lake2e, Lake2f, Lake2g)
LosAngeles2 = Station2Station2("LosAngeles") 
Madera2 = Station2Station2("Madera") 
Marin2 = Station2Station2("Marin") 
Merced2 = Station2Station2("Merced") 
Monterey2 = Station2Station2("Monterey") 
Napa2 = Station2Station2("Napa") 
Placer2 = Station2Station2("Placer") 
Plumas2 = Station2Station2("Plumas")
Riverside2 = Station2Station2("Riverside") 
Sacramento2a = Station2Station3("Sacramento")
Sacramento2b = Station2Station3("Sacramento")
Sacramento2c = Station2Station3("Sacramento")
Sacramento2d = Station2Station3("Sacramento")
Sacramento2 = rbind(Sacramento2a, Sacramento2b, Sacramento2c, Sacramento2d)
SanBenito2 = Station2Station2("SanBenito")
SanBernardino2 = Station2Station2("SanBernardino") 
SanDiego2 = Station2Station2("SanDiego") 
SanJoaquin2 = Station2Station2("SanJoaquin") 
SanMateo2 = Station2Station2("SanMateo") 
SantaClara2 = Station2Station2("SantaClara") 
SantaCruz2 = Station2Station2("SantaCruz") 
Solano2 = Station2Station2("Solano")
Sonoma2 = Station2Station2("Sonoma") 
Tehama2 = Station2Station2("Tehama") 
Tulare2 = Station2Station2("Tulare")
Ventura2 = Station2Station2("Ventura") 
Yuba2 = Station2Station2("Yuba") 
Yolo2a = Station2Station3("Yolo") 
Yolo2b = Station2Station3("Yolo")
Yolo2 = rbind(Yolo2a, Yolo2b)

# Bind together and output 
AllCounties = rbind(Alameda2, Butte2, Colusa2, ContraCosta2, Fresno2, Kern2, Kings2,
                      Lake2, LosAngeles2, Madera2, Marin2, Merced2, Monterey2, Napa2, Placer2,
                      Plumas2, Riverside2, Sacramento2, SanBenito2, SanBernardino2, 
                      SanDiego2, SanJoaquin2,  SanMateo2, SantaClara2,
                      SantaCruz2, Solano2, Sonoma2, Tehama2, Tulare2, Yolo2, Yuba2)
write.csv(AllCounties,"~/Documents/Current_Projects/DogHeartworm/Mosquito&WeatherStationLinks_AllCounties.csv")


#### 3: Create data frame using data from ALL species 
# will include: county, collection lat/long and closest lat/long to NOAA data

CleanedCt = read.csv("~/Documents/Current_Projects/DogHeartworm/MosquitoSpeciesData/CalSurv_CleanedCxTarsalis_Data.csv", header = T)
CleanedCs = read.csv("~/Documents/Current_Projects/DogHeartworm/MosquitoSpeciesData/CalSurv_CleanedCsInornata_Data.csv", header = T)
CleanedCq = read.csv("~/Documents/Current_Projects/DogHeartworm/MosquitoSpeciesData/CalSurv_CleanedCxQuinquefasciatus_Data.csv", header = T)
CleanedCi = read.csv("~/Documents/Current_Projects/DogHeartworm/MosquitoSpeciesData/CalSurv_CleanedCsIncidens_Data.csv", header = T)
CleanedAf = read.csv("~/Documents/Current_Projects/DogHeartworm/MosquitoSpeciesData/CalSurv_CleanedAnFreeborni_Data.csv", header = T)
CleanedAv = read.csv("~/Documents/Current_Projects/DogHeartworm/MosquitoSpeciesData/CalSurv_CleanedAeVexans_Data.csv", header = T)
CleanedAe = read.csv("~/Documents/Current_Projects/DogHeartworm/MosquitoSpeciesData/CalSurv_CleanedAeAegypti_Data.csv", header = T)
CleanedAa = read.csv("~/Documents/Current_Projects/DogHeartworm/MosquitoSpeciesData/CalSurv_CleanedAeAlbopictus_Data.csv", header = T)
CleanedAs = read.csv("~/Documents/Current_Projects/DogHeartworm/MosquitoSpeciesData/CalSurv_CleanedAeSierrensis_Data.csv", header = T)
# make Ae. sierrensis column names match the other species
colnames(CleanedAs)[c(4,8,11,14:18)] = c("agency_collection_num", "trap", "sex", "code", "latitude", "longitude", "latitude.1", "longitude.1")

# combine dataframes and remove extra row from most data frames
CleanedAll = rbind.data.frame(CleanedAs[,-1], CleanedAa, CleanedAe[,-1], CleanedAf[,-1], 
                              CleanedAv[,-1], CleanedCt[,-1], CleanedCs[,-1], CleanedCi[,-1],
                              CleanedCq[,-1])

# Keep just the county name and collection lat/long columns
AllLocation = CleanedAll[,c(1,14,15)]

# Add 360 to longitude to be consistent with NOAA longitude values
AllLocation$longitude = AllLocation$longitude + 360

# keep just the unique locations for each
AllUniqueIndexes = unique(AllLocation[c("latitude", "longitude")])
AllUnique = AllLocation[rownames(AllUniqueIndexes),]

# Now link THESE to the closest NOAA data point based on lat/long 

library(sp)
library(rgeos)

# First create a list of all the lat/long combinations from the NOAA dataset
Lats = latitude[latindex]
Longs = longitude[lonindex]
NOAAstations = expand.grid(Lats,Longs)
colnames(NOAAstations) = c("latitude","longitude")

# Now find closest NOAA lat/long to the unique surveillance lat/longs
# doing on pieces at a time

AllUnique$closest_latitude = NA
AllUnique$closest_longitude = NA
AllUnique$Station_Distance = NA

for (i in 48001:51000) {
  stationdist = NA
  stationlat = NA
  stationlong = NA
  Combined = rbind.data.frame(AllUnique[i,c(2,3)], NOAAstations)
  coordinates(Combined) = ~longitude+latitude
  d = gDistance(Combined, byid = T)
  labelmin_dist = which.min(d[1,-1])
  AllUnique$closest_longitude[i] = NOAAstations[labelmin_dist,2]
  AllUnique$closest_latitude[i] = NOAAstations[labelmin_dist,1]
  AllUnique$Station_Distance[i] = min(d[1,-1]) 
  }  

# output this file
#write.csv(AllUnique, "~/Downloads/AllSpecies_SurveillanceSiteToNOAAgeos_LinkerC.csv")
