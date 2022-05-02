#### Dog heartworm land cover data analysis #####
# using lab desktop - unsure why this wouldn't run on laptop

# from : https://stackoverflow.com/questions/15824853/large-img-file-processing-in-r-gis

#### Load libraries and import dataset ####

library(raster)
library(rgdal)

# pull in twice - data 2 will be transformed to spatial object

data = read.csv("/users/Mordecai Lab/Downloads/Mosquito&WeatherStationLinks_AllCounties_AeSierrensis.csv", header = T)
data2 = read.csv("/users/Mordecai Lab/Downloads/Mosquito&WeatherStationLinks_AllCounties_AeSierrensis.csv", header = T)

# Land cover classes: https://www.mrlc.gov/downloads/sciweb1/shared/mrlc/metadata/nlcd_2006_land_cover_l48_20210604.xml

# 11 : open water, # 21 : developed, open space
# 22 - 24 : developed, low to high intensity, # 31 : barren land
# 41 - 43 : deciduous, evergreen, and mixed forest, # 51 - 52 : dwarf/scrub shrub
# 71 - 72 : grassland/sedge herbaceous, # 73 - 74 : lichens / moss
# 81 - 82 : pasture/hay, cultivated crops, # 90, 95 : woody/herbaceous wetlands


# Want to calculate a) percent forest cover (41-43), b) percent wetland, c) percent impervious (diff. data set)
# at 100m, and 1000m buffer for each surveillance point (not doing 10m like Skaff since this is smaller than data resolution)
# Want to use NLCD data from closest year to surveillance record 
# NLCD data available for: 2001, 2004, 2006, 2008, 2011, 2013, 2016, 2019

#### Land cover variables for records near 2019 #### 
# i.e., surveillance records from 2018, 2019, 2020, and 2021 

setwd("~/NLCD2019")
x = list.files()[2]
NLCD2019 = raster(x)

# to visualize where the surveillance records are
plot (NLCD2019)
#add the converted x y points
points (sites_transformed, pch=16, col="red", cex=.75) # created above

# pull out just the surv years this NLCD 2019 dataset will be used for
data2019 = data[data$surv_year > 2017,]

# extract lat/longs and reproject to be consistent with NLCD data
data2 = data2019[, 16:17] # pull out just lat/longs
coordinates(data2)  <-  c("collection_longitude",  "collection_latitude")
proj4string(data2)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
sites_transformed<-spTransform(data2, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

#extract land cover values from 100 & 1000m buffer around surveillance record coordinates
Landcover100.2019 <-extract (NLCD2019, sites_transformed, buffer=100)
Landcover1000.2019 <-extract (NLCD2019, sites_transformed, buffer=1000)

# create table that lists the proportion of each land cover class around the surveillance record
y100.2019 = lapply(Landcover100.2019, function(x) prop.table(table(x)))
y1000.2019 = lapply(Landcover1000.2019, function(x) prop.table(table(x)))

# Pull out proportion of forest (i.e, the sum of land cover class 41, 42, & 43)
propforest100 = vector()
for (i in 1:nrow(data2019))
{l41 = as.numeric(y100.2019[[i]][names(y100.2019[[i]]) == "41"])
l42 = as.numeric(y100.2019[[i]][names(y100.2019[[i]]) == "42"])
l43 = as.numeric(y100.2019[[i]][names(y100.2019[[i]]) == "43"])
propforest100[i] = sum(c(l41, l42, l43))}

propforest1000 = vector()
for (i in 1:nrow(data2019))
{l41 = as.numeric(y1000.2019[[i]][names(y1000.2019[[i]]) == "41"])
l42 = as.numeric(y1000.2019[[i]][names(y1000.2019[[i]]) == "42"])
l43 = as.numeric(y1000.2019[[i]][names(y1000.2019[[i]]) == "43"])
propforest1000[i] = sum(c(l41, l42, l43))}

# Pull out proportion of wetlands (i.e., sum of land cover class 90, 95)
propwetlands100 = vector()
for (i in 1:nrow(data2019))
{l90 = as.numeric(y100.2019[[i]][names(y100.2019[[i]]) == "90"])
l95 = as.numeric(y100.2019[[i]][names(y100.2019[[i]]) == "95"])
propwetlands100[i] = sum(c(l90, l95))}

propwetlands1000 = vector()
for (i in 1:nrow(data2019))
{l90 = as.numeric(y1000.2019[[i]][names(y1000.2019[[i]]) == "90"])
l95 = as.numeric(y1000.2019[[i]][names(y1000.2019[[i]]) == "95"])
propwetlands1000[i] = sum(c(l90, l95))}


# create columns to add data to
data$propforest100 = NA  
data$propforest1000 = NA
data$propwetlands100 = NA  
data$propwetlands1000 = NA

# Add these values to data frame for surv years 2018, 2019, 2020, and 2021 
for (i in 1:nrow(data2019))
  {data$propforest100[as.numeric(rownames(data2019)[i])] = propforest100[i]}

for (i in 1:nrow(data2019))
{data$propforest1000[as.numeric(rownames(data2019)[i])] = propforest1000[i]}

for (i in 1:nrow(data2019))
{data$propwetlands100[as.numeric(rownames(data2019)[i])] = propwetlands100[i]}

for (i in 1:nrow(data2019))
{data$propwetlands1000[as.numeric(rownames(data2019)[i])] = propwetlands1000[i]}
 
#### Land cover variables for records near 2016 #### 
# i.e., surveillance records from 2015, 2016, 2017 

setwd("~/NLCD2016")
x = list.files()[2]
NLCD2016 = raster(x)

# pull out just the surv years this NLCD 2016 dataset will be used for
years2016 = c("2015", "2016", "2017")
data2016 = data[data$surv_year %in% years2016,]

# extract lat/longs and reproject to be consistent with NLCD data
data2 = data2016[, 16:17] # pull out just lat/longs
coordinates(data2)  <-  c("collection_longitude",  "collection_latitude")
proj4string(data2)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
sites_transformed<-spTransform(data2, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

#extract land cover values from 100m & 1000m buffer around surveillance record coordinates
Landcover100.2016 <-extract (NLCD2016, sites_transformed, buffer=100)
Landcover1000.2016 <-extract (NLCD2016, sites_transformed, buffer=1000)

# create table that lists the proportion of each land cover class around the surveillance record
y100.2016 = lapply(Landcover100.2016, function(x) prop.table(table(x)))
y1000.2016 = lapply(Landcover1000.2016, function(x) prop.table(table(x)))

# Pull out proportion of forest (i.e, the sum of land cover class 41, 42, & 43)
propforest100 = vector()
for (i in 1:nrow(data2016))
{l41 = as.numeric(y100.2016[[i]][names(y100.2016[[i]]) == "41"])
l42 = as.numeric(y100.2016[[i]][names(y100.2016[[i]]) == "42"])
l43 = as.numeric(y100.2016[[i]][names(y100.2016[[i]]) == "43"])
propforest100[i] = sum(c(l41, l42, l43))}

propforest1000 = vector()
for (i in 1:nrow(data2016))
{l41 = as.numeric(y1000.2016[[i]][names(y1000.2016[[i]]) == "41"])
l42 = as.numeric(y1000.2016[[i]][names(y1000.2016[[i]]) == "42"])
l43 = as.numeric(y1000.2016[[i]][names(y1000.2016[[i]]) == "43"])
propforest1000[i] = sum(c(l41, l42, l43))}

# Pull out proportion of wetlands (i.e., sum of land cover class 90, 95)
propwetlands100 = vector()
for (i in 1:nrow(data2016))
{l90 = as.numeric(y100.2016[[i]][names(y100.2016[[i]]) == "90"])
l95 = as.numeric(y100.2016[[i]][names(y100.2016[[i]]) == "95"])
propwetlands100[i] = sum(c(l90, l95))}

propwetlands1000 = vector()
for (i in 1:nrow(data2016))
{l90 = as.numeric(y1000.2016[[i]][names(y1000.2016[[i]]) == "90"])
l95 = as.numeric(y1000.2016[[i]][names(y1000.2016[[i]]) == "95"])
propwetlands1000[i] = sum(c(l90, l95))}


# Add these values to data frame for surv years 2015, 2016, 2017
for (i in 1:nrow(data2016))
{data$propforest100[as.numeric(rownames(data2016)[i])] = propforest100[i]}

for (i in 1:nrow(data2016))
{data$propforest1000[as.numeric(rownames(data2016)[i])] = propforest1000[i]}

for (i in 1:nrow(data2016))
{data$propwetlands100[as.numeric(rownames(data2016)[i])] = propwetlands100[i]}

for (i in 1:nrow(data2016))
{data$propwetlands1000[as.numeric(rownames(data2016)[i])] = propwetlands1000[i]}
#### Land cover variables for records near 2013 #### 
# i.e., surveillance records from 2012, 2013, 2014

setwd("~/NLCD2013")
x = list.files()[2]
NLCD2013 = raster(x)

# pull out just the surv years this NLCD 2013 dataset will be used for
years2013 = c("2012", "2013", "2014")
data2013 = data[data$surv_year %in% years2013,]

# extract lat/longs and reproject to be consistent with NLCD data
data2 = data2013[, 16:17] # pull out just lat/longs
coordinates(data2)  <-  c("collection_longitude",  "collection_latitude")
proj4string(data2)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
sites_transformed<-spTransform(data2, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

#extract land cover values from 100m & 1000m buffer around surveillance record coordinates
Landcover100.2013 <-extract (NLCD2013, sites_transformed, buffer=100)
Landcover1000.2013 <-extract (NLCD2013, sites_transformed, buffer=1000)

# create table that lists the proportion of each land cover class around the surveillance record
y100.2013 = lapply(Landcover100.2013, function(x) prop.table(table(x)))
y1000.2013 = lapply(Landcover1000.2013, function(x) prop.table(table(x)))

# Pull out proportion of forest (i.e, the sum of land cover class 41, 42, & 43)
propforest100 = vector()
for (i in 1:nrow(data2013))
{l41 = as.numeric(y100.2013[[i]][names(y100.2013[[i]]) == "41"])
l42 = as.numeric(y100.2013[[i]][names(y100.2013[[i]]) == "42"])
l43 = as.numeric(y100.2013[[i]][names(y100.2013[[i]]) == "43"])
propforest100[i] = sum(c(l41, l42, l43))}

propforest1000 = vector()
for (i in 1:nrow(data2013))
{l41 = as.numeric(y1000.2013[[i]][names(y1000.2013[[i]]) == "41"])
l42 = as.numeric(y1000.2013[[i]][names(y1000.2013[[i]]) == "42"])
l43 = as.numeric(y1000.2013[[i]][names(y1000.2013[[i]]) == "43"])
propforest1000[i] = sum(c(l41, l42, l43))}

# Pull out proportion of wetlands (i.e., sum of land cover class 90, 95)
propwetlands100 = vector()
for (i in 1:nrow(data2013))
{l90 = as.numeric(y100.2013[[i]][names(y100.2013[[i]]) == "90"])
l95 = as.numeric(y100.2013[[i]][names(y100.2013[[i]]) == "95"])
propwetlands100[i] = sum(c(l90, l95))}

propwetlands1000 = vector()
for (i in 1:nrow(data2013))
{l90 = as.numeric(y1000.2013[[i]][names(y1000.2013[[i]]) == "90"])
l95 = as.numeric(y1000.2013[[i]][names(y1000.2013[[i]]) == "95"])
propwetlands1000[i] = sum(c(l90, l95))}

# Add these values to data frame for surv years 2012, 2013, 2014
for (i in 1:nrow(data2013))
{data$propforest100[as.numeric(rownames(data2013)[i])] = propforest100[i]}

for (i in 1:nrow(data2013))
{data$propforest1000[as.numeric(rownames(data2013)[i])] = propforest1000[i]}

for (i in 1:nrow(data2013))
{data$propwetlands100[as.numeric(rownames(data2013)[i])] = propwetlands100[i]}

for (i in 1:nrow(data2013))
{data$propwetlands1000[as.numeric(rownames(data2013)[i])] = propwetlands1000[i]}
#### Land cover variables for records near 2011 #### 
# i.e., surveillance records from 2011, 2010, 2009, 2008

setwd("~/NLCD2011")
x = list.files()[2]
NLCD2011 = raster(x)

# pull out just the surv years this NLCD 2011 dataset will be used for
years2011 = c("2011", "2010", "2009", "2008")
data2011 = data[data$surv_year %in% years2011,]

# extract lat/longs and reproject to be consistent with NLCD data
data2 = data2011[, 16:17] # pull out just lat/longs
coordinates(data2)  <-  c("collection_longitude",  "collection_latitude")
proj4string(data2)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
sites_transformed<-spTransform(data2, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

#extract land cover values from 100m & 1000m buffer around surveillance record coordinates
Landcover100.2011 <-extract (NLCD2011, sites_transformed, buffer=100)
Landcover1000.2011 <-extract (NLCD2011, sites_transformed, buffer=1000)

# create table that lists the proportion of each land cover class around the surveillance record
y100.2011 = lapply(Landcover100.2011, function(x) prop.table(table(x)))
y1000.2011 = lapply(Landcover1000.2011, function(x) prop.table(table(x)))

# Pull out proportion of forest (i.e, the sum of land cover class 41, 42, & 43)
propforest100 = vector()
for (i in 1:nrow(data2011))
{l41 = as.numeric(y100.2011[[i]][names(y100.2011[[i]]) == "41"])
l42 = as.numeric(y100.2011[[i]][names(y100.2011[[i]]) == "42"])
l43 = as.numeric(y100.2011[[i]][names(y100.2011[[i]]) == "43"])
propforest100[i] = sum(c(l41, l42, l43))}

propforest1000 = vector()
for (i in 1:nrow(data2011))
{l41 = as.numeric(y1000.2011[[i]][names(y1000.2011[[i]]) == "41"])
l42 = as.numeric(y1000.2011[[i]][names(y1000.2011[[i]]) == "42"])
l43 = as.numeric(y1000.2011[[i]][names(y1000.2011[[i]]) == "43"])
propforest1000[i] = sum(c(l41, l42, l43))}

# Pull out proportion of wetlands (i.e., sum of land cover class 90, 95)
propwetlands100 = vector()
for (i in 1:nrow(data2011))
{l90 = as.numeric(y100.2011[[i]][names(y100.2011[[i]]) == "90"])
l95 = as.numeric(y100.2011[[i]][names(y100.2011[[i]]) == "95"])
propwetlands100[i] = sum(c(l90, l95))}

propwetlands1000 = vector()
for (i in 1:nrow(data2011))
{l90 = as.numeric(y1000.2011[[i]][names(y1000.2011[[i]]) == "90"])
l95 = as.numeric(y1000.2011[[i]][names(y1000.2011[[i]]) == "95"])
propwetlands1000[i] = sum(c(l90, l95))}

# Add these values to data frame for surv years 2012, 2013, 2014
for (i in 1:nrow(data2011))
{data$propforest100[as.numeric(rownames(data2011)[i])] = propforest100[i]}

for (i in 1:nrow(data2011))
{data$propforest1000[as.numeric(rownames(data2011)[i])] = propforest1000[i]}

for (i in 1:nrow(data2011))
{data$propwetlands100[as.numeric(rownames(data2011)[i])] = propwetlands100[i]}

for (i in 1:nrow(data2011))
{data$propwetlands1000[as.numeric(rownames(data2011)[i])] = propwetlands1000[i]}
#### Land cover variables for records near 2006 #### 
# i.e., surveillance records from 2007, 2006, 2005

setwd("~/NLCD2006")
x = list.files()[2]
NLCD2006 = raster(x)

# pull out just the surv years this NLCD 2006 dataset will be used for
years2006 = c("2007", "2006", "2005")
data2006 = data[data$surv_year %in% years2006,]

# extract lat/longs and reproject to be consistent with NLCD data
data2 = data2006[, 16:17] # pull out just lat/longs
coordinates(data2)  <-  c("collection_longitude",  "collection_latitude")
proj4string(data2)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
sites_transformed<-spTransform(data2, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

#extract land cover values from 100m & 1000m buffer around surveillance record coordinates
Landcover100.2006 <-extract (NLCD2006, sites_transformed, buffer=100)
Landcover1000.2006 <-extract (NLCD2006, sites_transformed, buffer=1000)

# create table that lists the proportion of each land cover class around the surveillance record
y100.2006 = lapply(Landcover100.2006, function(x) prop.table(table(x)))
y1000.2006 = lapply(Landcover1000.2006, function(x) prop.table(table(x)))

# Pull out proportion of forest (i.e, the sum of land cover class 41, 42, & 43)
propforest100 = vector()
for (i in 1:nrow(data2006))
{l41 = as.numeric(y100.2006[[i]][names(y100.2006[[i]]) == "41"])
l42 = as.numeric(y100.2006[[i]][names(y100.2006[[i]]) == "42"])
l43 = as.numeric(y100.2006[[i]][names(y100.2006[[i]]) == "43"])
propforest100[i] = sum(c(l41, l42, l43))}

propforest1000 = vector()
for (i in 1:nrow(data2006))
{l41 = as.numeric(y1000.2006[[i]][names(y1000.2006[[i]]) == "41"])
l42 = as.numeric(y1000.2006[[i]][names(y1000.2006[[i]]) == "42"])
l43 = as.numeric(y1000.2006[[i]][names(y1000.2006[[i]]) == "43"])
propforest1000[i] = sum(c(l41, l42, l43))}

# Pull out proportion of wetlands (i.e., sum of land cover class 90, 95)
propwetlands100 = vector()
for (i in 1:nrow(data2006))
{l90 = as.numeric(y100.2006[[i]][names(y100.2006[[i]]) == "90"])
l95 = as.numeric(y100.2006[[i]][names(y100.2006[[i]]) == "95"])
propwetlands100[i] = sum(c(l90, l95))}

propwetlands1000 = vector()
for (i in 1:nrow(data2006))
{l90 = as.numeric(y1000.2006[[i]][names(y1000.2006[[i]]) == "90"])
l95 = as.numeric(y1000.2006[[i]][names(y1000.2006[[i]]) == "95"])
propwetlands1000[i] = sum(c(l90, l95))}

# Add these values to data frame for surv years 2007, 2005, 2006
for (i in 1:nrow(data2006))
{data$propforest100[as.numeric(rownames(data2006)[i])] = propforest100[i]}

for (i in 1:nrow(data2006))
{data$propforest1000[as.numeric(rownames(data2006)[i])] = propforest1000[i]}

for (i in 1:nrow(data2006))
{data$propwetlands100[as.numeric(rownames(data2006)[i])] = propwetlands100[i]}

for (i in 1:nrow(data2006))
{data$propwetlands1000[as.numeric(rownames(data2006)[i])] = propwetlands1000[i]}
#### Land cover variables for records near 2004 #### 
# i.e., surveillance records from 2004, 2003, 2002

setwd("~/NLCD2004")
x = list.files()[2]
NLCD2004 = raster(x)

# pull out just the surv years this NLCD 2006 dataset will be used for
years2004 = c("2004", "2003", "2002")
data2004 = data[data$surv_year %in% years2004,]

# extract lat/longs and reproject to be consistent with NLCD data
data2 = data2004[, 16:17] # pull out just lat/longs
coordinates(data2)  <-  c("collection_longitude",  "collection_latitude")
proj4string(data2)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
sites_transformed<-spTransform(data2, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

#extract land cover values from 100m & 1000m buffer around surveillance record coordinates
Landcover100.2004 <-extract (NLCD2004, sites_transformed, buffer=100)
Landcover1000.2004 <-extract (NLCD2004, sites_transformed, buffer=1000)

# create table that lists the proportion of each land cover class around the surveillance record
y100.2004 = lapply(Landcover100.2004, function(x) prop.table(table(x)))
y1000.2004 = lapply(Landcover1000.2004, function(x) prop.table(table(x)))

# Pull out proportion of forest (i.e, the sum of land cover class 41, 42, & 43)
propforest100 = vector()
for (i in 1:nrow(data2004))
{l41 = as.numeric(y100.2004[[i]][names(y100.2004[[i]]) == "41"])
l42 = as.numeric(y100.2004[[i]][names(y100.2004[[i]]) == "42"])
l43 = as.numeric(y100.2004[[i]][names(y100.2004[[i]]) == "43"])
propforest100[i] = sum(c(l41, l42, l43))}

propforest1000 = vector()
for (i in 1:nrow(data2004))
{l41 = as.numeric(y1000.2004[[i]][names(y1000.2004[[i]]) == "41"])
l42 = as.numeric(y1000.2004[[i]][names(y1000.2004[[i]]) == "42"])
l43 = as.numeric(y1000.2004[[i]][names(y1000.2004[[i]]) == "43"])
propforest1000[i] = sum(c(l41, l42, l43))}

# Pull out proportion of wetlands (i.e., sum of land cover class 90, 95)
propwetlands100 = vector()
for (i in 1:nrow(data2004))
{l90 = as.numeric(y100.2004[[i]][names(y100.2004[[i]]) == "90"])
l95 = as.numeric(y100.2004[[i]][names(y100.2004[[i]]) == "95"])
propwetlands100[i] = sum(c(l90, l95))}

propwetlands1000 = vector()
for (i in 1:nrow(data2004))
{l90 = as.numeric(y1000.2004[[i]][names(y1000.2004[[i]]) == "90"])
l95 = as.numeric(y1000.2004[[i]][names(y1000.2004[[i]]) == "95"])
propwetlands1000[i] = sum(c(l90, l95))}

# Add these values to data frame for surv years 2004, 2003, 2002
for (i in 1:nrow(data2004))
{data$propforest100[as.numeric(rownames(data2004)[i])] = propforest100[i]}

for (i in 1:nrow(data2004))
{data$propforest1000[as.numeric(rownames(data2004)[i])] = propforest1000[i]}

for (i in 1:nrow(data2004))
{data$propwetlands100[as.numeric(rownames(data2004)[i])] = propwetlands100[i]}

for (i in 1:nrow(data2004))
{data$propwetlands1000[as.numeric(rownames(data2004)[i])] = propwetlands1000[i]}
#### Land cover variables for records near 2001 #### 
# i.e., surveillance records for 2001 and earlier

setwd("~/NLCD2001")
x = list.files()[2]
NLCD2001 = raster(x)

# pull out just the surv years this NLCD 2001 dataset will be used for
data2001 = data[data$surv_year < 2002,]

# extract lat/longs and reproject to be consistent with NLCD data
data2 = data2001[, 16:17] # pull out just lat/longs
coordinates(data2)  <-  c("collection_longitude",  "collection_latitude")
proj4string(data2)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
sites_transformed<-spTransform(data2, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

#extract land cover values from 100m & 1000m buffer around surveillance record coordinates
Landcover100.2001 <-extract (NLCD2001, sites_transformed, buffer=100)
Landcover1000.2001 <-extract (NLCD2001, sites_transformed, buffer=1000)

# create table that lists the proportion of each land cover class around the surveillance record
y100.2001 = lapply(Landcover100.2001, function(x) prop.table(table(x)))
y1000.2001 = lapply(Landcover1000.2001, function(x) prop.table(table(x)))

# Pull out proportion of forest (i.e, the sum of land cover class 41, 42, & 43)
propforest100 = vector()
for (i in 1:nrow(data2001))
{l41 = as.numeric(y100.2001[[i]][names(y100.2001[[i]]) == "41"])
l42 = as.numeric(y100.2001[[i]][names(y100.2001[[i]]) == "42"])
l43 = as.numeric(y100.2001[[i]][names(y100.2001[[i]]) == "43"])
propforest100[i] = sum(c(l41, l42, l43))}

propforest1000 = vector()
for (i in 1:nrow(data2001))
{l41 = as.numeric(y1000.2001[[i]][names(y1000.2001[[i]]) == "41"])
l42 = as.numeric(y1000.2001[[i]][names(y1000.2001[[i]]) == "42"])
l43 = as.numeric(y1000.2001[[i]][names(y1000.2001[[i]]) == "43"])
propforest1000[i] = sum(c(l41, l42, l43))}

# Pull out proportion of wetlands (i.e., sum of land cover class 90, 95)
propwetlands100 = vector()
for (i in 1:nrow(data2001))
{l90 = as.numeric(y100.2001[[i]][names(y100.2001[[i]]) == "90"])
l95 = as.numeric(y100.2001[[i]][names(y100.2001[[i]]) == "95"])
propwetlands100[i] = sum(c(l90, l95))}

propwetlands1000 = vector()
for (i in 1:nrow(data2001))
{l90 = as.numeric(y1000.2001[[i]][names(y1000.2001[[i]]) == "90"])
l95 = as.numeric(y1000.2001[[i]][names(y1000.2001[[i]]) == "95"])
propwetlands1000[i] = sum(c(l90, l95))}

# Add these values to data frame for surv years 2004, 2003, 2002
for (i in 1:nrow(data2001))
{data$propforest100[as.numeric(rownames(data2001)[i])] = propforest100[i]}

for (i in 1:nrow(data2001))
{data$propforest1000[as.numeric(rownames(data2001)[i])] = propforest1000[i]}

for (i in 1:nrow(data2001))
{data$propwetlands100[as.numeric(rownames(data2001)[i])] = propwetlands100[i]}

for (i in 1:nrow(data2001))
{data$propwetlands1000[as.numeric(rownames(data2001)[i])] = propwetlands1000[i]}
#### Impervious cover for records near 2019 #### 
# i.e., surveillance records from 2018, 2019, 2020, and 2021 

setwd("~/NLCD2019_Impervious")
x = list.files()[2]
NLCD2019imp = raster(x)

# pull out just the surv years this NLCD 2019 dataset will be used for
data2019 = data[data$surv_year > 2017,]

# extract lat/longs and reproject to be consistent with NLCD data
data2 = data2019[, 16:17] # pull out just lat/longs
coordinates(data2)  <-  c("collection_longitude",  "collection_latitude")
proj4string(data2)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
sites_transformed<-spTransform(data2, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

#extract impervious cover value from 100m & 1000m buffer around surveillance record coordinates
Impcover100.2019 <-extract (NLCD2019imp, sites_transformed, buffer=100)
Impcover1000.2019 <-extract (NLCD2019imp, sites_transformed, buffer=1000)

# calculate average impervious cover (above function provides percent for each 30m pixel)
y100.2019.imp = as.numeric(lapply(Impcover100.2019, function(x) mean(x)))
y1000.2019.imp = as.numeric(lapply(Impcover1000.2019, function(x) mean(x)))

# create columns to add data to
data$perimpervious100 = NA  
data$perimpervious1000 = NA

# Add these values to data frame for surv years 2018, 2019, 2020, and 2021 
for (i in 1:nrow(data2019))
{data$perimpervious100[as.numeric(rownames(data2019)[i])] = y100.2019.imp[i]}

for (i in 1:nrow(data2019))
{data$perimpervious1000[as.numeric(rownames(data2019)[i])] = y1000.2019.imp[i]}
#### Impervious cover for records near 2016 #### 
# i.e., surveillance records from 2015, 2016, 2017 

setwd("~/NLCD2016_Impervious")
x = list.files()[2]
NLCD2016imp = raster(x)

# pull out just the surv years this NLCD 2016 dataset will be used for
years2016 = c("2015", "2016", "2017")
data2016 = data[data$surv_year %in% years2016,]

# extract lat/longs and reproject to be consistent with NLCD data
data2 = data2016[, 16:17] # pull out just lat/longs
coordinates(data2)  <-  c("collection_longitude",  "collection_latitude")
proj4string(data2)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
sites_transformed<-spTransform(data2, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

#extract impervious cover value from 100m & 1000m buffer around surveillance record coordinates
Impcover100.2016 <-extract (NLCD2016imp, sites_transformed, buffer=100)
Impcover1000.2016 <-extract (NLCD2016imp, sites_transformed, buffer=1000)

# calculate average impervious cover (above function provides percent for each 30m pixel)
y100.2016.imp = as.numeric(lapply(Impcover100.2016, function(x) mean(x)))
y1000.2016.imp = as.numeric(lapply(Impcover1000.2016, function(x) mean(x)))

# Add these values to data frame for surv years 2015 - 2017
for (i in 1:nrow(data2016))
{data$perimpervious100[as.numeric(rownames(data2016)[i])] = y100.2016.imp[i]}

for (i in 1:nrow(data2016))
{data$perimpervious1000[as.numeric(rownames(data2016)[i])] = y1000.2016.imp[i]}
#### Impervious cover for records near 2013 #### 
# i.e., surveillance records from 2012, 2013, 2014

setwd("~/NLCD2013_Impervious")
x = list.files()[2]
NLCD2013imp = raster(x)

# pull out just the surv years this NLCD 2013 dataset will be used for
years2013 = c("2012", "2013", "2014")
data2013 = data[data$surv_year %in% years2013,]

# extract lat/longs and reproject to be consistent with NLCD data
data2 = data2013[, 16:17] # pull out just lat/longs
coordinates(data2)  <-  c("collection_longitude",  "collection_latitude")
proj4string(data2)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
sites_transformed<-spTransform(data2, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

#extract impervious cover value from 100m & 1000m buffer around surveillance record coordinates
Impcover100.2013 <-extract (NLCD2013imp, sites_transformed, buffer=100)
Impcover1000.2013 <-extract (NLCD2013imp, sites_transformed, buffer=1000)

# calculate average impervious cover (above function provides percent for each 30m pixel)
y100.2013.imp = as.numeric(lapply(Impcover100.2013, function(x) mean(x)))
y1000.2013.imp = as.numeric(lapply(Impcover1000.2013, function(x) mean(x)))

# Add these values to data frame for surv years 2012 - 2014
for (i in 1:nrow(data2013))
{data$perimpervious100[as.numeric(rownames(data2013)[i])] = y100.2013.imp[i]}

for (i in 1:nrow(data2013))
{data$perimpervious1000[as.numeric(rownames(data2013)[i])] = y1000.2013.imp[i]}
#### Impervious cover for records near 2011 #### 
# i.e., surveillance records from 2011, 2010, 2009, 2008

setwd("~/NLCD2011_Impervious")
x = list.files()[2]
NLCD2011imp = raster(x)

# pull out just the surv years this NLCD 2011 dataset will be used for
years2011 = c("2011", "2010", "2009", "2008")
data2011 = data[data$surv_year %in% years2011,]

# extract lat/longs and reproject to be consistent with NLCD data
data2 = data2011[, 16:17] # pull out just lat/longs
coordinates(data2)  <-  c("collection_longitude",  "collection_latitude")
proj4string(data2)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
sites_transformed<-spTransform(data2, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

#extract impervious cover value from 100m & 1000m buffer around surveillance record coordinates
Impcover100.2011 <-extract (NLCD2011imp, sites_transformed, buffer=100)
Impcover1000.2011 <-extract (NLCD2011imp, sites_transformed, buffer=1000)

# calculate average impervious cover (above function provides percent for each 30m pixel)
y100.2011.imp = as.numeric(lapply(Impcover100.2011, function(x) mean(x)))
y1000.2011.imp = as.numeric(lapply(Impcover1000.2011, function(x) mean(x)))


# Add these values to data frame for surv years 2008 - 2011
for (i in 1:nrow(data2011))
{data$perimpervious100[as.numeric(rownames(data2011)[i])] = y100.2011.imp[i]}

for (i in 1:nrow(data2011))
{data$perimpervious1000[as.numeric(rownames(data2011)[i])] = y1000.2011.imp[i]}
#### Impervious cover for records near 2006 #### 
# i.e., surveillance records from 2007, 2006, 2005

setwd("~/NLCD2006_Impervious")
x = list.files()[2]
NLCD2006imp = raster(x)

# pull out just the surv years this NLCD 2006 dataset will be used for
years2006 = c("2007", "2006", "2005")
data2006 = data[data$surv_year %in% years2006,]

# extract lat/longs and reproject to be consistent with NLCD data
data2 = data2006[, 16:17] # pull out just lat/longs
coordinates(data2)  <-  c("collection_longitude",  "collection_latitude")
proj4string(data2)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
sites_transformed<-spTransform(data2, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

#extract impervious cover value from 100m & 1000m buffer around surveillance record coordinates
Impcover100.2006 <-extract (NLCD2006imp, sites_transformed, buffer=100)
Impcover1000.2006 <-extract (NLCD2006imp, sites_transformed, buffer=1000)

# calculate average impervious cover (above function provides percent for each 30m pixel)
y100.2006.imp = as.numeric(lapply(Impcover100.2006, function(x) mean(x)))
y1000.2006.imp = as.numeric(lapply(Impcover1000.2006, function(x) mean(x)))

# Add these values to data frame for surv years 2005 - 2007
for (i in 1:nrow(data2006))
{data$perimpervious100[as.numeric(rownames(data2006)[i])] = y100.2006.imp[i]}

for (i in 1:nrow(data2006))
{data$perimpervious1000[as.numeric(rownames(data2006)[i])] = y1000.2006.imp[i]}
#### Impervious cover for records near 2004 #### 
# i.e., surveillance records from 2004, 2003, 2002

setwd("~/NLCD2004_Impervious")
x = list.files()[2]
NLCD2004imp = raster(x)

# pull out just the surv years this NLCD 2006 dataset will be used for
years2004 = c("2004", "2003", "2002")
data2004 = data[data$surv_year %in% years2004,]

# extract lat/longs and reproject to be consistent with NLCD data
data2 = data2004[, 16:17] # pull out just lat/longs
coordinates(data2)  <-  c("collection_longitude",  "collection_latitude")
proj4string(data2)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
sites_transformed<-spTransform(data2, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

#extract impervious cover value from 100m & 1000m buffer around surveillance record coordinates
Impcover100.2004 <-extract (NLCD2004imp, sites_transformed, buffer=100)
Impcover1000.2004 <-extract (NLCD2004imp, sites_transformed, buffer=1000)

# calculate average impervious cover (above function provides percent for each 30m pixel)
y100.2004.imp = as.numeric(lapply(Impcover100.2004, function(x) mean(x)))
y1000.2004.imp = as.numeric(lapply(Impcover1000.2004, function(x) mean(x)))

# Add these values to data frame for surv years 2002- 2004
for (i in 1:nrow(data2004))
{data$perimpervious100[as.numeric(rownames(data2004)[i])] = y100.2004.imp[i]}

for (i in 1:nrow(data2004))
{data$perimpervious1000[as.numeric(rownames(data2004)[i])] = y1000.2004.imp[i]}
#### Impervious cover for records near 2001 #### 
# i.e., surveillance records for 2001 and earlier

setwd("~/NLCD2001_Impervious")
x = list.files()[2]
NLCD2001imp = raster(x)

# pull out just the surv years this NLCD 2001 dataset will be used for
data2001 = data[data$surv_year < 2002,]

# extract lat/longs and reproject to be consistent with NLCD data
data2 = data2001[, 16:17] # pull out just lat/longs
coordinates(data2)  <-  c("collection_longitude",  "collection_latitude")
proj4string(data2)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
sites_transformed<-spTransform(data2, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

#extract impervious cover value from 100m & 1000m buffer around surveillance record coordinates
Impcover100.2001 <-extract (NLCD2001imp, sites_transformed, buffer=100)
Impcover1000.2001 <-extract (NLCD2001imp, sites_transformed, buffer=1000)

# calculate average impervious cover (above function provides percent for each 30m pixel)
y100.2001.imp = as.numeric(lapply(Impcover100.2001, function(x) mean(x)))
y1000.2001.imp = as.numeric(lapply(Impcover1000.2001, function(x) mean(x)))

# Add these values to data frame for surv years 2001 and prior
for (i in 1:nrow(data2001))
{data$perimpervious100[as.numeric(rownames(data2001)[i])] = y100.2001.imp[i]}

for (i in 1:nrow(data2001))
{data$perimpervious1000[as.numeric(rownames(data2001)[i])] = y1000.2001.imp[i]}

# output 
#write.csv(data, "DogHeartworm_LandCoverVariables.csv")

