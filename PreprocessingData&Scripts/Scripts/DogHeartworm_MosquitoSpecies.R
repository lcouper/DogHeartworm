#### DogHeartwormProject: Mosquito Species #####

#### Aedes sierrensis #####
as = read.csv("~/Documents/Current_Projects/DogHeartworm/MosquitoSpeciesData/CalSurv_AeSierrensis_Data.csv", header = T)
ads = as[,c(22,3, 17,10,7,8,6,9,11,13,14,15, 16,18:21)]
ads = ads[order(ads[,1], ads[,3], ads[,4], ads[,5]),]

# Remove any rows where there was a problem with the trap
ads = ads[ads$trap_problem_bit == "FALSE",]

# Keep only rows with certain trap types (for consistency) : 
# gravid traps, NJ light traps, magnet traps, BG sentinel, oviposition trap, & resting box
ads$trap_type = as.factor(ads$trap_type)
ads = ads[ads$trap_type == "Gravid trap" | ads$trap_type == "New Jersey light trap"
      | ads$trap_type == "Resting box collection" | ads$trap_type == "Mosquito magnet trap" |
      ads$trap_type == "Carbon dioxide baited trap"| ads$trap_type == "BG Sentinel"
      | ads$trap_type == "CDC Autocidal Gravid Ovitrap (counts)"
      | ads$trap_type == "Oviposition Trap (counts)", ]

# Keep only info on: FEMALES (gravid (pregnant), bloodfed, unfed, or mixed)
ads$arthro_sex_condition = as.factor(ads$arthro_sex_condition)
ads = ads[ads$arthro_sex_condition != "Eggs",]
ads = ads[ads$arthro_sex_condition != "Males",]

# Remove duplicates
ads = ads[!duplicated(ads),]

# All rows now contain abundance data only (no need to remove prior rows with presece data)

# Remove spaces from county names so it can match with weather station names!
ads[ads$county == "Contra Costa",1] = "ContraCosta"
ads[ads$county == "Los Angeles",1] = "LosAngeles"
ads[ads$county == "San Benito",1] = "SanBenito"
ads[ads$county == "San Bernardino",1] = "SanBernardino"
ads[ads$county == "San Joaquin",1] = "SanJoaquin"
ads[ads$county == "Santa Barbara",1] = "SantaBarbara"
ads[ads$county == "Salt Lake",1] = "SaltLake"
ads[ads$county == "San Luis Obispo",1] = "SanLuisObispo"
ads[ads$county == "San Diego",1] = "SanDiego"
ads[ads$county == "San Mateo",1] = "SanMateo"
ads[ads$county == "Santa Clara",1] = "SantaClara"
ads[ads$county == "Santa Cruz",1] = "SantaCruz"

# FOR NOW: keep only rows with 1 nights of trap output
# This is the most common length, and this is what Nick Skaff did
ads1 = ads[ads$trap_nights == 1,]

# output this file
#write.csv(ads1, "~/Downloads/CalSurv_CleanedAeSierrensis_Data.csv")


# Calculate avg abundance across all sites sampled that year in a given county
AvgAbunAe = as.data.frame(matrix(nrow = length(unique(ads1$county)), ncol = length(unique(ads1$surv_year))))
NumSamplesAe = as.data.frame(matrix(nrow = length(unique(ads1$county)), ncol = length(unique(ads1$surv_year)))) # to track number of surveillance attempts
UniqueSitesAe = as.data.frame(matrix(nrow = length(unique(ads1$county)), ncol = length(unique(ads1$surv_year)))) # to track number of unique sites

countylist2 = unique(ads1$county) # create index for all counties
years = sort(unique(ads1$surv_year)) # create index for all years

for (i in 1:length(countylist2))
{County = ads1[ads1$county == countylist2[i],]
for (j in 1:length(years)) {
  CountyYear = County[County$surv_year == years[j],]
  AvgAbunAe[i,(j+1)] = mean(CountyYear$num_count, na.rm = T)
  NumSamplesAe[i,(j+1)] = nrow(CountyYear)
  UniqueSitesAe[i,(j+1)] = length(unique(CountyYear$site_name))
}
}

AvgAbunAe[,1] = countylist2
NumSamplesAe[,1] = countylist2
UniqueSitesAe[,1] = countylist2

# Add column names
colnames(AvgAbunAe) = c("County", years)
colnames(NumSamplesAe) = c("County", years)
colnames(UniqueSitesAe) = c("County", years)

# Convert NaNs to missing
for (i in 2:ncol(AvgAbunAe))
{AvgAbunAe[,i][is.nan(AvgAbunAe[,i])]<-NA}

# appears to be one outlier data point (Alameda in 2019). Remove for now
AvgAbunAe[1,22] = NA

# convert the AvgAe dataframe from wide to long (to enable merging with dog hw case data)
library(reshape2)
AvgAbunAeM = melt(AvgAbunAe)
colnames(AvgAbunAeM) = c("County", "Year", "AvgAe")
AvgAbunAeM$Year = as.integer(as.character(AvgAbunAeM$Year))

# create 1 year lagged column
LaggedAvgAbunAe = AvgAbunAe[,-2]
# There is no 1999 data so have to start at 2001 here 
colnames(LaggedAvgAbunAe) = c("County", 2001:2021)
LaggedAvgAbunAeM = melt(LaggedAvgAbunAe)
colnames(LaggedAvgAbunAeM) = c("County", "Year", "LaggedAvgAe")
LaggedAvgAbunAeM$Year = as.integer(as.character(LaggedAvgAbunAeM$Year))


#### Aedes albopictus #####

ae = read.csv("~/Documents/Current_Projects/DogHeartworm/MosquitoSpeciesData/CalSurv_AeAlbopictus_Data.csv", header = T)
aeb = ae[,c(3, 4, 5,1, 6, 7, 8, 11, 10, 17, 18, 15, 2,12, 13, 12, 13)]
aeb = aeb[order(aeb[,1], aeb[,2], aeb[,4], aeb[,5]),]

# Remove any rows where there was a problem with the trap
aeb = aeb[aeb$trap_problem_bit == "FALSE",] # removed 58 rows

# Keep only rows with certain trap types (for consistency) : 
# gravid traps, NJ light traps, oviposition traps, BG sentinel, magnet traps, co2 baited traps & resting box, 
aeb$trap = as.factor(aeb$trap)
aeb = aeb[aeb$trap == "Gravid trap" | aeb$trap == "CDC Autocidal Gravid Ovitrap (counts)"
          | aeb$trap == "Modified CDC Autocidal Gravid Ovitrap (counts)" | aeb$trap == "Carbon dioxide baited trap" |
            aeb$trap == "Biogents Gravid Aedes Trap" | aeb$trap == "Oviposition Trap (counts)" 
          | aeb$trap == "BG Sentinel",]

# Keep only info on: FEMALES (gravid (pregnant), bloodfed, unfed, or mixed)
aeb$sex = as.factor(aeb$sex)
aeb = aeb[aeb$sex == "Females - Unfed" |  aeb$sex == "Females - Gravid" 
          | aeb$sex == "Females - Bloodfed" | aeb$sex == "Females - Mixed",]

# Remove duplicates
aeb = aeb[!duplicated(aeb),]

# Remove spaces from county names so it can match with weather station names!
aeb[aeb$county == "Los Angeles",1] = "LosAngeles"
aeb[aeb$county == "San Bernardino",1] = "SanBernardino"
aeb[aeb$county == "San Diego",1] = "SanDiego"

# output this file
#write.csv(aeb, "~/Downloads/CalSurv_CleanedAeAlbopictus_Data.csv")

# For now keep only rows with 1 nights of trap output
# This is the most common length, and this is what Nick Skaff did
aeb1 = aeb[aeb$trap_nights == 1,]

# Calculate avg abundance across all sites sampled that year in a given county
AvgAbunAeb = as.data.frame(matrix(nrow = length(unique(aeb1$county)), ncol = length(unique(aeb1$surv_year))))
NumSamplesAeb = as.data.frame(matrix(nrow = length(unique(aeb1$county)), ncol = length(unique(aeb1$surv_year)))) # to track number of surveillance attempts
UniqueSitesAeb = as.data.frame(matrix(nrow = length(unique(aeb1$county)), ncol = length(unique(aeb1$surv_year)))) # to track number of unique sites

countylist2 = unique(aeb1$county) # create index for all counties
years = sort(unique(aeb1$surv_year)) # create index for all years

for (i in 1:length(countylist2))
{County = aeb1[aeb1$county == countylist2[i],]
for (j in 1:length(years)) {
  CountyYear = County[County$surv_year == years[j],]
  AvgAbunAeb[i,(j+1)] = mean(CountyYear$num_count, na.rm = T)
  NumSamplesAeb[i,(j+1)] = nrow(CountyYear)
  UniqueSitesAeb[i,(j+1)] = length(unique(CountyYear$site_name))
}
}

AvgAbunAeb[,1] = countylist2
NumSamplesAeb[,1] = countylist2
UniqueSitesAeb[,1] = countylist2

# Add column names
colnames(AvgAbunAeb) = c("County", years)
colnames(NumSamplesAeb) = c("County", years)
colnames(UniqueSitesAeb) = c("County", years)

# Convert NaNs to missing
for (i in 2:ncol(AvgAbunAeb))
{AvgAbunAeb[,i][is.nan(AvgAbunAeb[,i])]<-NA}

# no major outliers

# convert the AvgAeb df from wide to long (to enable merging with dog hw case data)
library(reshape2)
AvgAbunAebM = melt(AvgAbunAeb)
colnames(AvgAbunAebM) = c("County", "Year", "AvgAeb")
AvgAbunAebM$Year = as.integer(as.character(AvgAbunAebM$Year))

# create 1 year lagged column
LaggedAvgAbunAeb = AvgAbunAeb[,-c(2,ncol(AvgAbunAeb))]
colnames(LaggedAvgAbunAeb) = c("County", 2015:2021)
LaggedAvgAbunAebM = melt(LaggedAvgAbunAeb)
colnames(LaggedAvgAbunAebM) = c("County", "Year", "LaggedAvgAeb")
LaggedAvgAbunAebM$Year = as.integer(as.character(LaggedAvgAbunAebM$Year))


#### Aedes aegypti #####

ae2 = read.csv("~/Documents/Current_Projects/DogHeartworm/MosquitoSpeciesData/CalSurv_AeAegypti_Data.csv", header = T)
aeg = ae2[,c(3, 4, 5,1, 6, 7, 8, 11, 10, 17, 18, 15, 2,12, 13, 12, 13)]
aeg = aeg[order(aeg[,1], aeg[,2], aeg[,4], aeg[,5]),]

# Remove any rows where there was a problem with the trap
aeg = aeg[aeg$trap_problem_bit == "FALSE",] # removed 1467 rows

# Keep only rows with certain trap types (for consistency) : 
# gravid traps, NJ light traps, oviposition traps, BG sentinel, magnet traps, co2 baited traps & resting box, 
aeg$trap = as.factor(aeg$trap)
aeg = aeg[aeg$trap == "Gravid trap" | aeg$trap == "CDC Autocidal Gravid Ovitrap (counts)"
          | aeg$trap == "Modified CDC Autocidal Gravid Ovitrap (counts)" | aeg$trap == "Carbon dioxide baited trap" |
            aeg$trap == "Biogents Gravid Aedes Trap" | aeg$trap == "Oviposition Trap (counts)" 
          | aeg$trap == "BG Sentinel" | aeg$trap == "New Jersey light trap"
          | aeg$trap == "Mosquito magnet trap",]

# Keep only info on: FEMALES (gravid (pregnant), bloodfed, unfed, or mixed)
aeg$sex = as.factor(aeg$sex)
aeg = aeg[aeg$sex == "Females - Unfed" |  aeg$sex == "Females - Gravid" 
          | aeg$sex == "Females - Bloodfed" | aeg$sex == "Females - Mixed",]

# Remove duplicates
aeg = aeg[!duplicated(aeg),]

# Remove spaces from county names so it can match with weather station names!
aeg[aeg$county == "Los Angeles",1] = "LosAngeles"
aeg[aeg$county == "San Bernardino",1] = "SanBernardino"
aeg[aeg$county == "San Joaquin",1] = "SanJoaquin"
aeg[aeg$county == "San Diego",1] = "SanDiego"
aeg[aeg$county == "San Mateo",1] = "SanMateo"
aeg[aeg$county == "Santa Barbara",1] = "SantaBarbara"

# output this file
#write.csv(aeg, "~/Downloads/CalSurv_CleanedAeAegypti_Data.csv")

# For now keep only rows with 1 nights of trap output
# This is the most common length, and this is what Nick Skaff did
aeg1 = aeg[aeg$trap_nights == 1,]

# Calculate avg abundance across all sites sampled that year in a given county
AvgAbunAeg = as.data.frame(matrix(nrow = length(unique(aeg1$county)), ncol = length(unique(aeg1$surv_year))))
NumSamplesAeg = as.data.frame(matrix(nrow = length(unique(aeg1$county)), ncol = length(unique(aeg1$surv_year)))) # to track number of surveillance attempts
UniqueSitesAeg = as.data.frame(matrix(nrow = length(unique(aeg1$county)), ncol = length(unique(aeg1$surv_year)))) # to track number of unique sites

countylist2 = unique(aeg1$county) # create index for all counties
years = sort(unique(aeg1$surv_year)) # create index for all years

for (i in 1:length(countylist2))
{County = aeg1[aeg1$county == countylist2[i],]
for (j in 1:length(years)) {
  CountyYear = County[County$surv_year == years[j],]
  AvgAbunAeg[i,(j+1)] = mean(CountyYear$num_count, na.rm = T)
  NumSamplesAeg[i,(j+1)] = nrow(CountyYear)
  UniqueSitesAeg[i,(j+1)] = length(unique(CountyYear$site_name))
}
}
# Add counties as first row
AvgAbunAeg[,1] = countylist2
NumSamplesAeg[,1] = countylist2
UniqueSitesAeg[,1] = countylist2

# Add column names
colnames(AvgAbunAeg) = c("County", years)
colnames(NumSamplesAeg) = c("County", years)
colnames(UniqueSitesAeg) = c("County", years)

# Convert NaNs to missing
for (i in 2:ncol(AvgAbunAeg))
{AvgAbunAeg[,i][is.nan(AvgAbunAeg[,i])]<-NA}

# Kings county in 2021 could be an outlier (count = 23, which is ~double the next highest)

# convert the AvgAeg df from wide to long (to enable merging with dog hw case data)
AvgAbunAegM = melt(AvgAbunAeg)
colnames(AvgAbunAegM) = c("County", "Year", "AvgAeg")
AvgAbunAegM$Year = as.integer(as.character(AvgAbunAegM$Year))

# create 1 year lagged column
LaggedAvgAbunAeg = AvgAbunAeg[,-ncol(AvgAbunAeg)]
colnames(LaggedAvgAbunAeg) = c("County", 2014:2021)
LaggedAvgAbunAegM = melt(LaggedAvgAbunAeg)
colnames(LaggedAvgAbunAegM) = c("County", "Year", "LaggedAvgAeg")
LaggedAvgAbunAegM$Year = as.integer(as.character(LaggedAvgAbunAegM$Year))


#### Aedes vexans #####

ave = read.csv("~/Documents/Current_Projects/DogHeartworm/MosquitoSpeciesData/CalSurv_AeVexans_Data.csv", header = T)
av = ave[,c(3, 4, 5,1, 6, 7, 8, 11, 10, 17, 18, 15, 2,12, 13, 12, 13)]
av = av[order(ave[,1], ave[,2], ave[,4], ave[,5]),]

# Remove any rows where there was a problem with the trap
av = av[av$trap_problem_bit == "FALSE",] # removed 31 rows

# Keep only rows with certain trap types (for consistency) : 
# gravid traps, NJ light traps, oviposition traps, BG sentinel, magnet traps, co2 baited traps & resting box, 
av$trap = as.factor(av$trap)
av = av[av$trap == "Gravid trap" | av$trap == "CDC Autocidal Gravid Ovitrap (counts)"
          | av$trap == "Carbon dioxide baited trap" | av$trap == "Mosquito magnet trap" 
           | av$trap == "Oviposition Trap (counts)" 
          | av$trap == "BG Sentinel" | av$trap == "New Jersey light trap"
        | av$trap == "Resting box collection", ]

# Keep only info on: FEMALES (gravid (pregnant), bloodfed, unfed, or mixed)
av$sex = as.factor(av$sex)
av = av[av$sex == "Females - Unfed" |  av$sex == "Females - Gravid" 
          | av$sex == "Females - Bloodfed" | av$sex == "Females - Mixed",]

# Remove duplicates
av = av[!duplicated(av),]

av[av$county == "San Bernardino",1] = "SanBernardino"

# spaces already removed

# output this file
#write.csv(av, "~/Downloads/CalSurv_CleanedAeVexans_Data.csv")

# Keep only rows with 1 nights of trap output
# This is the most common length, and this is what Nick Skaff did
av1 = av[av$trap_nights == 1,]

# Calculate avg abundance across all sites sampled that year in a given county
AvgAbunAv = as.data.frame(matrix(nrow = length(unique(av1$county)), ncol = length(unique(av1$surv_year))))
NumSamplesAv = as.data.frame(matrix(nrow = length(unique(av1$county)), ncol = length(unique(av1$surv_year)))) # to track number of surveillance attempts
UniqueSitesAv = as.data.frame(matrix(nrow = length(unique(av1$county)), ncol = length(unique(av1$surv_year)))) # to track number of unique sites

countylist2 = unique(av1$county) # create index for all counties
years = sort(unique(av1$surv_year)) # create index for all years

for (i in 1:length(countylist2))
{County = av1[av1$county == countylist2[i],]
for (j in 1:length(years)) {
  CountyYear = County[County$surv_year == years[j],]
  AvgAbunAv[i,(j+1)] = mean(CountyYear$num_count, na.rm = T)
  NumSamplesAv[i,(j+1)] = nrow(CountyYear)
  UniqueSitesAv[i,(j+1)] = length(unique(CountyYear$site_name))
}
}

# Add counties as first row
AvgAbunAv[,1] = countylist2
NumSamplesAv[,1] = countylist2
UniqueSitesAv[,1] = countylist2

# Add column names
colnames(AvgAbunAv) = c("County", years)
colnames(NumSamplesAv) = c("County", years)
colnames(UniqueSitesAv) = c("County", years)

# Convert NaNs to missing
for (i in 2:ncol(AvgAbunAv))
{AvgAbunAv[,i][is.nan(AvgAbunAv[,i])]<-NA}

# Alameda 2021 potentially an outlier (count = 1864, which is ~5x the next highest)
AvgAbunAv[24,13] <-NA

# convert the AvgAeg df from wide to long (to enable merging with dog hw case data)
AvgAbunAvM = melt(AvgAbunAv)
colnames(AvgAbunAvM) = c("County", "Year", "AvgAv")
AvgAbunAvM$Year = as.integer(as.character(AvgAbunAvM$Year))

# create 1 year lagged column
LaggedAvgAbunAv = AvgAbunAv[,-ncol(AvgAbunAv)]
colnames(LaggedAvgAbunAv) = c("County", 2011:2021)
LaggedAvgAbunAvM = melt(LaggedAvgAbunAv)
colnames(LaggedAvgAbunAvM) = c("County", "Year", "LaggedAvgAv")
LaggedAvgAbunAvM$Year = as.integer(as.character(LaggedAvgAbunAvM$Year))


#### Anopheles freeborni #####

af2 = read.csv("~/Documents/Current_Projects/DogHeartworm/MosquitoSpeciesData/CalSurv_AnFreeborni_Data.csv", header = T)
af = af2[,c(3, 4, 5,1, 6, 7, 8, 11, 10, 17, 18, 15, 2,12, 13, 12, 13)]
af = af[order(af[,1], af[,2], af[,4], af[,5]),]

# Remove any rows where there was a problem with the trap
af = af[af$trap_problem_bit == "FALSE",] # removed 211 rows

# Keep only rows with certain trap types (for consistency) : 
# gravid traps, NJ light traps, oviposition traps, BG sentinel, magnet traps, co2 baited traps & resting box, 
af$trap = as.factor(af$trap)
af = af[af$trap == "Gravid trap" | af$trap == "CDC Autocidal Gravid Ovitrap (counts)" |
        af$trap == "Carbon dioxide baited trap" | af$trap == "Mosquito magnet trap"  |
        af$trap == "Oviposition Trap (counts)" |
        af$trap == "BG Sentinel" | af$trap == "New Jersey light trap" |
        af$trap == "Resting box collection" |
        af$trap == "Modified CDC Autocidal Gravid Ovitrap (counts)" |
        af$trap == "Biogents Gravid Aedes Trap" |
        af$trap == "Oviposition Trap (counts)", ]

# Keep only info on: FEMALES (gravid (pregnant), bloodfed, unfed, or mixed)
af$sex = as.factor(af$sex)
af = af[af$sex == "Females - Unfed" |  af$sex == "Females - Gravid" 
        | af$sex == "Females - Bloodfed" | af$sex == "Females - Mixed",]

# Remove duplicates
af = af[!duplicated(af),]

# spaces in county names already removed 

# output this file
#write.csv(af, "~/Downloads/CalSurv_CleanedAnFreeborni_Data.csv")

# Keep only rows with 1 nights of trap output
# This is the most common length, and this is what Nick Skaff did
af1 = af[af$trap_nights == 1,]

# Calculate avg abundance across all sites sampled that year in a given county
AvgAbunAf = as.data.frame(matrix(nrow = length(unique(af1$county)), ncol = length(unique(af1$surv_year))))
NumSamplesAf = as.data.frame(matrix(nrow = length(unique(af1$county)), ncol = length(unique(af1$surv_year)))) # to track number of surveillance attempts
UniqueSitesAf = as.data.frame(matrix(nrow = length(unique(af1$county)), ncol = length(unique(af1$surv_year)))) # to track number of unique sites

countylist2 = unique(af1$county) # create index for all counties
years = sort(unique(af1$surv_year)) # create index for all years

for (i in 1:length(countylist2))
{County = af1[af1$county == countylist2[i],]
for (j in 1:length(years)) {
  CountyYear = County[County$surv_year == years[j],]
  AvgAbunAf[i,(j+1)] = mean(CountyYear$num_count, na.rm = T)
  NumSamplesAf[i,(j+1)] = nrow(CountyYear)
  UniqueSitesAf[i,(j+1)] = length(unique(CountyYear$site_name))
}
}

# Add counties as first row
AvgAbunAf[,1] = countylist2
NumSamplesAf[,1] = countylist2
UniqueSitesAf[,1] = countylist2

# Add column names
colnames(AvgAbunAf) = c("County", years)
colnames(NumSamplesAf) = c("County", years)
colnames(UniqueSitesAf) = c("County", years)

# Convert NaNs to missing
for (i in 2:ncol(AvgAbunAf))
{AvgAbunAf[,i][is.nan(AvgAbunAf[,i])]<-NA}

# there are a couple in the 600s... unsure if I should consider these as outliers & remove
# removing for now
AvgAbunAf[6,2] = NA
AvgAbunAf[29,4]= NA

# convert the AvgAf df from wide to long (to enable merging with dog hw case data)
AvgAbunAfM = melt(AvgAbunAf)
colnames(AvgAbunAfM) = c("County", "Year", "AvgAf")
AvgAbunAfM$Year = as.integer(as.character(AvgAbunAfM$Year))

# create 1 year lagged column
LaggedAvgAbunAf = AvgAbunAf[,-ncol(AvgAbunAf)]
colnames(LaggedAvgAbunAf) = c("County", 2011:2021)
LaggedAvgAbunAfM = melt(LaggedAvgAbunAf)
colnames(LaggedAvgAbunAfM) = c("County", "Year", "LaggedAvgAf")
LaggedAvgAbunAfM$Year = as.integer(as.character(LaggedAvgAbunAfM$Year))


#### Culiseta incidens #####

cs2 = read.csv("~/Documents/Current_Projects/DogHeartworm/MosquitoSpeciesData/CalSurv_CsIncidens_Data.csv", header = T)
cs = cs2[,c(3, 4, 5,1, 6, 7, 8, 11, 10, 17, 18, 15, 2,12, 13, 12, 13)]
cs = cs[order(cs[,1], cs[,2], cs[,4], cs[,5]),]

# Remove any rows where there was a problem with the trap
cs = cs[cs$trap_problem_bit == "FALSE",] # removed 436 rows

# Keep only rows with certain trap types (for consistency) : 
# gravid traps, NJ light traps, oviposition traps, BG sentinel, magnet traps, co2 baited traps & resting box, 
cs$trap = as.factor(cs$trap)
cs = cs[cs$trap == "Gravid trap" | cs$trap == "CDC Autocidal Gravid Ovitrap (counts)" |
          cs$trap == "Carbon dioxide baited trap" | cs$trap == "Mosquito magnet trap"  |
          cs$trap == "Oviposition Trap (counts)" |
          cs$trap == "BG Sentinel" | cs$trap == "New Jersey light trap" |
          cs$trap == "Resting box collection" |
          cs$trap == "Reiter-Cummings Gravid Trap" |
          cs$trap == "Modified CDC Autocidal Gravid Ovitrap (counts)" |
          cs$trap == "Biogents Gravid Aedes Trap", ]

# Keep only info on: FEMALES (gravid (pregnant), bloodfed, unfed, or mixed)
cs$sex = as.factor(cs$sex)
cs = cs[cs$sex == "Females - Unfed" |  cs$sex == "Females - Gravid" 
        | cs$sex == "Females - Bloodfed" | cs$sex == "Females - Mixed",]

# Remove duplicates
cs = cs[!duplicated(cs),]

# spaces in county names already removed 

# output this file
#write.csv(cs, "~/Downloads/CalSurv_CleanedCsIncidens_Data.csv")

# Keep only rows with 1 nights of trap output
# This is the most common length, and this is what Nick Skaff did
cs1 = cs[cs$trap_nights == 1,]

# Calculate avg abundance across all sites sampled that year in a given county
AvgAbunCs = as.data.frame(matrix(nrow = length(unique(cs1$county)), ncol = length(unique(cs1$surv_year))))
NumSamplesCs = as.data.frame(matrix(nrow = length(unique(cs1$county)), ncol = length(unique(cs1$surv_year)))) # to track number of surveillance attempts
UniqueSitesCs = as.data.frame(matrix(nrow = length(unique(cs1$county)), ncol = length(unique(cs1$surv_year)))) # to track number of unique sites

countylist2 = unique(cs1$county) # create index for all counties
years = sort(unique(cs1$surv_year)) # create index for all years

for (i in 1:length(countylist2))
{County = cs1[cs1$county == countylist2[i],]
for (j in 1:length(years)) {
  CountyYear = County[County$surv_year == years[j],]
  AvgAbunCs[i,(j+1)] = mean(CountyYear$num_count, na.rm = T)
  NumSamplesCs[i,(j+1)] = nrow(CountyYear)
  UniqueSitesCs[i,(j+1)] = length(unique(CountyYear$site_name))
}
}

# Add counties as first row
AvgAbunCs[,1] = countylist2
NumSamplesCs[,1] = countylist2
UniqueSitesCs[,1] = countylist2

# Add column names
colnames(AvgAbunCs) = c("County", years)
colnames(NumSamplesCs) = c("County", years)
colnames(UniqueSitesCs) = c("County", years)

# Convert NaNs to missing
for (i in 2:ncol(AvgAbunCs))
{AvgAbunCs[,i][is.nan(AvgAbunCs[,i])]<-NA}

# no huge outliers

# convert the AvgCs df from wide to long (to enable merging with dog hw case data)
AvgAbunCsM = melt(AvgAbunCs)
colnames(AvgAbunCsM) = c("County", "Year", "AvgCs")
AvgAbunCsM$Year = as.integer(as.character(AvgAbunCsM$Year))

# create 1 year lagged column
LaggedAvgAbunCs = AvgAbunCs[,-ncol(AvgAbunCs)]
colnames(LaggedAvgAbunCs) = c("County", 2011:2021)
LaggedAvgAbunCsM = melt(LaggedAvgAbunCs)
colnames(LaggedAvgAbunCsM) = c("County", "Year", "LaggedAvgCs")
LaggedAvgAbunCsM$Year = as.integer(as.character(LaggedAvgAbunCsM$Year))

# output this file for analysis
write.csv(LaggedAvgAbunCsM, "~/Documents/Current_Projects/DogHeartworm/MosquitoSpeciesData/CsIncidens_ForAnalysis.csv")



#### Culiseta inornata #####

ci2 = read.csv("~/Documents/Current_Projects/DogHeartworm/MosquitoSpeciesData/CalSurv_CsInornata_Data.csv", header = T)
ci = ci2[,c(3, 4, 5,1, 6, 7, 8, 11, 10, 17, 18, 15, 2,12, 13, 12, 13)]
ci = ci[order(ci[,1], ci[,2], ci[,4], ci[,5]),]

# Remove any rows where there was a problem with the trap
ci = ci[ci$trap_problem_bit == "FALSE",] # removed 962 rows

# Keep only rows with certain trap types (for consistency) : 
# gravid traps, NJ light traps, oviposition traps, BG sentinel, magnet traps, co2 baited traps & resting box, 
ci$trap = as.factor(ci$trap)
ci = ci[ci$trap == "Gravid trap" | ci$trap == "CDC Autocidal Gravid Ovitrap (counts)" |
          ci$trap == "Carbon dioxide baited trap" | ci$trap == "Mosquito magnet trap"  |
          ci$trap == "Oviposition Trap (counts)" |
          ci$trap == "BG Sentinel" | ci$trap == "New Jersey light trap" |
          ci$trap == "Resting box collection" |
          ci$trap == "Biogents Gravid Aedes Trap", ]

# Keep only info on: FEMALES (gravid (pregnant), bloodfed, unfed, or mixed)
ci$sex = as.factor(ci$sex)
ci = ci[ci$sex == "Females - Unfed" |  ci$sex == "Females - Gravid" 
        | ci$sex == "Females - Bloodfed" | ci$sex == "Females - Mixed",]

# Remove duplicates
ci = ci[!duplicated(ci),]

# spaces in county names already removed 

# output this file
#write.csv(ci, "~/Downloads/CalSurv_CleanedCsInornata_Data.csv")

# Keep only rows with 1 nights of trap output
# This is the most common length, and this is what Nick Skaff did
ci1 = ci[ci$trap_nights == 1,]

# Calculate avg abundance across all sites sampled that year in a given county
AvgAbunCi = as.data.frame(matrix(nrow = length(unique(ci1$county)), ncol = length(unique(ci1$surv_year))))
NumSamplesCi = as.data.frame(matrix(nrow = length(unique(ci1$county)), ncol = length(unique(ci1$surv_year)))) # to track number of surveillance attempts
UniqueSitesCi = as.data.frame(matrix(nrow = length(unique(ci1$county)), ncol = length(unique(ci1$surv_year)))) # to track number of unique sites

countylist2 = unique(ci1$county) # create index for all counties
years = sort(unique(ci1$surv_year)) # create index for all years

for (i in 1:length(countylist2))
{County = ci1[ci1$county == countylist2[i],]
for (j in 1:length(years)) {
  CountyYear = County[County$surv_year == years[j],]
  AvgAbunCi[i,(j+1)] = mean(CountyYear$num_count, na.rm = T)
  NumSamplesCi[i,(j+1)] = nrow(CountyYear)
  UniqueSitesCi[i,(j+1)] = length(unique(CountyYear$site_name))
}
}

# Add counties as first row
AvgAbunCi[,1] = countylist2
NumSamplesCi[,1] = countylist2
UniqueSitesCi[,1] = countylist2

# Add column names
colnames(AvgAbunCi) = c("County", years)
colnames(NumSamplesCi) = c("County", years)
colnames(UniqueSitesCi) = c("County", years)

# Convert NaNs to missing
for (i in 2:ncol(AvgAbunCi))
{AvgAbunCi[,i][is.nan(AvgAbunCi[,i])]<-NA}

# stanislaus in 2017 a potential outlier, removing for now
AvgAbunCi[28,9] <- NA

# convert the AvgCi df from wide to long (to enable merging with dog hw case data)
AvgAbunCiM = melt(AvgAbunCi)
colnames(AvgAbunCiM) = c("County", "Year", "AvgCi")
AvgAbunCiM$Year = as.integer(as.character(AvgAbunCiM$Year))

# create 1 year lagged column
LaggedAvgAbunCi = AvgAbunCi[,-ncol(AvgAbunCi)]
colnames(LaggedAvgAbunCi) = c("County", 2011:2021)
LaggedAvgAbunCiM = melt(LaggedAvgAbunCi)
colnames(LaggedAvgAbunCiM) = c("County", "Year", "LaggedAvgCi")
LaggedAvgAbunCiM$Year = as.integer(as.character(LaggedAvgAbunCiM$Year))

# output this file for analysis
write.csv(LaggedAvgAbunCiM, "~/Documents/Current_Projects/DogHeartworm/MosquitoSpeciesData/CsInornata_ForAnalysis.csv")


#### Cx quinquefasciatus #####

cx2 = read.csv("~/Documents/Current_Projects/DogHeartworm/MosquitoSpeciesData/CalSurv_CxQuinquefasciatus_Data.csv", header = T)
cx = cx2[,c(3, 4, 5,1, 6, 7, 8, 11, 10, 17, 18, 15, 2,12, 13, 12, 13)]
cx = cx[order(cx[,1], cx[,2], cx[,4], cx[,5]),]

# Remove any rows where there was a problem with the trap
cx = cx[cx$trap_problem_bit == "FALSE",] # removed 2080 rows

# Keep only rows with certain trap types (for consistency) : 
# gravid traps, NJ light traps, oviposition traps, BG sentinel, magnet traps, co2 baited traps & resting box, 
cx$trap = as.factor(cx$trap)
cx = cx[cx$trap == "Gravid trap" | cx$trap == "CDC Autocidal Gravid Ovitrap (counts)" |
          cx$trap == "Carbon dioxide baited trap" | cx$trap == "Mosquito magnet trap"  |
          cx$trap == "Oviposition Trap (counts)" |
          cx$trap == "BG Sentinel" | cx$trap == "New Jersey light trap" |
          cx$trap == "Resting box collection" |
          cx$trap == "Modified CDC Autocidal Gravid Ovitrap (counts)" |
          cx$trap == "Biogents Gravid Aedes Trap", ]

# Keep only info on: FEMALES (gravid (pregnant), bloodfed, unfed, or mixed)
cx$sex = as.factor(cx$sex)
cx = cx[cx$sex == "Females - Unfed" |  cx$sex == "Females - Gravid" 
        | cx$sex == "Females - Bloodfed" | cx$sex == "Females - Mixed",]

# Remove duplicates
cx = cx[!duplicated(cx),]

# spaces in county names already removed 

# output this file
#write.csv(cx, "~/Downloads/CalSurv_CleanedCxQuinquefasciatus_Data.csv")

# Keep only rows with 1 nights of trap output
# This is the most common length, and this is what Nick Skaff did
cx1 = cx[cx$trap_nights == 1,]

# Calculate avg abundance across all sites sampled that year in a given county
AvgAbunCx = as.data.frame(matrix(nrow = length(unique(cx1$county)), ncol = length(unique(cx1$surv_year))))
NumSamplesCx = as.data.frame(matrix(nrow = length(unique(cx1$county)), ncol = length(unique(cx1$surv_year)))) # to track number of surveillance attempts
UniqueSitesCx = as.data.frame(matrix(nrow = length(unique(cx1$county)), ncol = length(unique(cx1$surv_year)))) # to track number of unique sites

countylist2 = unique(cx1$county) # create index for all counties
years = sort(unique(cx1$surv_year)) # create index for all years

for (i in 1:length(countylist2))
{County = cx1[cx1$county == countylist2[i],]
for (j in 1:length(years)) {
  CountyYear = County[County$surv_year == years[j],]
  AvgAbunCx[i,(j+1)] = mean(CountyYear$num_count, na.rm = T)
  NumSamplesCx[i,(j+1)] = nrow(CountyYear)
  UniqueSitesCx[i,(j+1)] = length(unique(CountyYear$site_name))
}}

# Add counties as first row
AvgAbunCx[,1] = countylist2
NumSamplesCx[,1] = countylist2
UniqueSitesCx[,1] = countylist2

# Add column names
colnames(AvgAbunCx) = c("County", years)
colnames(NumSamplesCx) = c("County", years)
colnames(UniqueSitesCx) = c("County", years)

# Convert NaNs to missing
for (i in 2:ncol(AvgAbunCx))
{AvgAbunCx[,i][is.nan(AvgAbunCx[,i])]<-NA}

# tulare 2010 may be outlier - removing for now
AvgAbunCx[29,2] <- NA

# convert the AvgCx df from wide to long (to enable merging with dog hw case data)
AvgAbunCxM = melt(AvgAbunCx)
colnames(AvgAbunCxM) = c("County", "Year", "AvgCx")
AvgAbunCxM$Year = as.integer(as.character(AvgAbunCxM$Year))

# create 1 year lagged column
LaggedAvgAbunCx = AvgAbunCx[,-ncol(AvgAbunCx)]
colnames(LaggedAvgAbunCx) = c("County", 2011:2021)
LaggedAvgAbunCxM = melt(LaggedAvgAbunCx)
colnames(LaggedAvgAbunCxM) = c("County", "Year", "LaggedAvgCx")
LaggedAvgAbunCxM$Year = as.integer(as.character(LaggedAvgAbunCxM$Year))

# output this file for analysis
write.csv(LaggedAvgAbunCxM, "~/Documents/Current_Projects/DogHeartworm/MosquitoSpeciesData/CxQuinque_ForAnalysis.csv")


#### Cx tarsalis #####

ct2 = read.csv("~/Documents/Current_Projects/DogHeartworm/MosquitoSpeciesData/CalSurv_CxTarsalis_Data.csv", header = T)
ct = ct2[,c(3, 4, 5,1, 6, 7, 8, 11, 10, 17, 18, 15, 2,12, 13, 12, 13)]
ct = ct[order(ct[,1], ct[,2], ct[,4], ct[,5]),]

# Remove any rows where there was a problem with the trap
ct = ct[ct$trap_problem_bit == "FALSE",] # did not remove any rows since I did this step previously

# Keep only rows with certain trap types (for consistency) : 
# gravid traps, NJ light traps, oviposition traps, BG sentinel, 
# CDC traps, Aedes traps, magnet traps, co2 baited traps & resting box, 
ct$trap = as.factor(ct$trap)
ct = ct[ct$trap == "Gravid trap" | ct$trap == "CDC Autocidal Gravid Ovitrap (counts)" |
          ct$trap == "Carbon dioxide baited trap" | ct$trap == "Mosquito magnet trap"  |
          ct$trap == "Oviposition Trap (counts)" |
          ct$trap == "BG Sentinel" | ct$trap == "New Jersey light trap" |
          ct$trap == "Resting box collection" |
          ct$trap == "Modified CDC Autocidal Gravid Ovitrap (counts)" |
          ct$trap == "Biogents Gravid Aedes Trap", ]

# Keep only info on: FEMALES (gravid (pregnant), bloodfed, unfed, or mixed)
ct$sex = as.factor(ct$sex)
ct = ct[ct$sex == "Females - Unfed" |  ct$sex == "Females - Gravid" 
        | ct$sex == "Females - Bloodfed" | ct$sex == "Females - Mixed",]

# Remove duplicates
ct = ct[!duplicated(ct),]

# spaces in county names already removed 

# output this file
#write.csv(ct, "~/Downloads/CalSurv_CleanedCxTarsalis_Data.csv")

# Keep only rows with 1 nights of trap output
# This is the most common length, and this is what Nick Skaff did
ct1 = ct[ct$trap_nights == 1,]

# Calculate avg abundance across all sites sampled that year in a given county
AvgAbunCt = as.data.frame(matrix(nrow = length(unique(ct1$county)), ncol = length(unique(ct1$surv_year))))
NumSamplesCt = as.data.frame(matrix(nrow = length(unique(ct1$county)), ncol = length(unique(ct1$surv_year)))) # to track number of surveillance attempts
UniqueSitesCt = as.data.frame(matrix(nrow = length(unique(ct1$county)), ncol = length(unique(ct1$surv_year)))) # to track number of unique sites

countylist2 = unique(ct1$county) # create index for all counties
years = sort(unique(ct1$surv_year)) # create index for all years

for (i in 1:length(countylist2))
{County = ct1[ct1$county == countylist2[i],]
for (j in 1:length(years)) {
  CountyYear = County[County$surv_year == years[j],]
  AvgAbunCt[i,(j+1)] = mean(CountyYear$num_count, na.rm = T)
  NumSamplesCt[i,(j+1)] = nrow(CountyYear)
  UniqueSitesCt[i,(j+1)] = length(unique(CountyYear$site_name))
}}

# Add counties as first row
AvgAbunCt[,1] = countylist2
NumSamplesCt[,1] = countylist2
UniqueSitesCt[,1] = countylist2

# Add column names
colnames(AvgAbunCt) = c("County", years)
colnames(NumSamplesCt) = c("County", years)
colnames(UniqueSitesCt) = c("County", years)

# Convert NaNs to missing
for (i in 2:ncol(AvgAbunCt))
{AvgAbunCt[,i][is.nan(AvgAbunCt[,i])]<-NA}

# Santa Barbara in 2013 may be an outlier - removing for now
AvgAbunCt[28,5] <- NA

# convert the AvgCt df from wide to long (to enable merging with dog hw case data)
AvgAbunCtM = melt(AvgAbunCt)
colnames(AvgAbunCtM) = c("County", "Year", "AvgCt")
AvgAbunCtM$Year = as.integer(as.character(AvgAbunCtM$Year))

# create 1 year lagged column
LaggedAvgAbunCt = AvgAbunCt[,-ncol(AvgAbunCt)]
colnames(LaggedAvgAbunCt) = c("County", 2011:2021)
LaggedAvgAbunCtM = melt(LaggedAvgAbunCt)
colnames(LaggedAvgAbunCtM) = c("County", "Year", "LaggedAvgCt")
LaggedAvgAbunCtM$Year = as.integer(as.character(LaggedAvgAbunCtM$Year))

# output this file for analysis
write.csv(LaggedAvgAbunCtM, "~/Documents/Current_Projects/DogHeartworm/MosquitoSpeciesData/CxTarsalis_ForAnalysis.csv")


#### Combine all #####

# Merge the (melted) avg annual abundance data from each species together into one df:
# list = AvgAbunAeM (sierrensis), AvgAbunAebM (albopictus), AvgAbunAegM (aeygypti), AvgAbunAvM (vexans)
# AvgAbunAfM (an. freeborni), AvgAbunCsM (cs. incidens), AvgAbunCiM  (cs inornata),
# AvgAbunCxM (Cx quinque), AvgAbunCtM (Cx tarsalis)


mergeA = merge(AvgAbunAeM, AvgAbunAebM, by = c("County", "Year"), all = T)
mergeB = merge(mergeA, AvgAbunAegM, by = c("County", "Year"), all = T)
mergeC = merge(mergeB, AvgAbunAvM, by = c("County", "Year"), all = T)
mergeD = merge(mergeC, AvgAbunAfM, by = c("County", "Year"), all = T)
mergeE = merge(mergeD, AvgAbunCsM, by = c("County", "Year"), all = T)
mergeF = merge(mergeE, AvgAbunCiM, by = c("County", "Year"), all = T)
mergeG = merge(mergeF, AvgAbunCxM, by = c("County", "Year"), all = T)
mergeH = merge(mergeG, AvgAbunCtM, by = c("County", "Year"), all = T)
colnames(mergeH) = c("County", "Year", "Ae.sierrensis", "Ae.albopictus", "Ae.aegypti",
                     "Ae.vexans", "An.freeborni", "Cs.incidencs", "Cs.inornata",
                     "Cx.quinquefasciatus", "Cx.tarsalis")
AllMosqSpAvg = mergeH
#output this file
write.csv(AllMosqSpAvg, "~/Documents/Current_Projects/DogHeartworm/MosquitoSpeciesData/AllMosquitoSpeciesAnnualAvg.csv")


## Repeat for 1-year lagged

LagmergeA = merge(LaggedAvgAbunAeM, LaggedAvgAbunAebM, by = c("County", "Year"), all = T)
LagmergeB = merge(LagmergeA, LaggedAvgAbunAegM, by = c("County", "Year"), all = T)
LagmergeC = merge(LagmergeB, LaggedAvgAbunAvM, by = c("County", "Year"), all = T)
LagmergeD = merge(LagmergeC, LaggedAvgAbunAfM, by = c("County", "Year"), all = T)
LagmergeE = merge(LagmergeD, LaggedAvgAbunCsM, by = c("County", "Year"), all = T)
LagmergeF = merge(LagmergeE, LaggedAvgAbunCiM, by = c("County", "Year"), all = T)
LagmergeG = merge(LagmergeF, LaggedAvgAbunCxM, by = c("County", "Year"), all = T)
LagmergeH = merge(LagmergeG, LaggedAvgAbunCtM, by = c("County", "Year"), all = T)

colnames(LagmergeH) = c("County", "Year", "Lag_Ae.sierrensis", "Lag_Ae.albopictus", "Lag_Ae.aegypti",
                     "Lag_Ae.vexans", "Lag_An.freeborni", "Lag_Cs.incidencs", "Lag_Cs.inornata",
                     "Lag_Cx.quinquefasciatus", "Lag_Cx.tarsalis")
AllMosqSpLaggedAvg = LagmergeH
#output this file
write.csv(AllMosqSpLaggedAvg, "~/Documents/Current_Projects/DogHeartworm/MosquitoSpeciesData/AllMosquitoSpeciesLaggedAnnualAvg.csv")





# Merge case data and mosquito data (AvgAeM and dogY data frames)
CaseMosq_int = merge(AvgAbunAeM, dogY, by = c("County", "Year"))
CaseMosq = merge(CaseMosq_int, LaggedAvgAbunAeM, by = c("County", "Year"))

CaseMosq_int2 = merge(CaseMosq, AvgAbunAebM, by = c("County", "Year"), all.x= TRUE)
CaseMosq2 = merge(CaseMosq_int2, LaggedAvgAbunAebM, by = c("County", "Year"), all.x = TRUE)

CaseMosq_int3 = merge(CaseMosq, AvgAbunAegM, by = c("County", "Year"), all.x= TRUE)
CaseMosq3 = merge(CaseMosq_int3, LaggedAvgAbunAegM, by = c("County", "Year"), all.x = TRUE)

CaseMosq_int4 = merge(CaseMosq, AvgAbunAvM, by = c("County", "Year"), all.x= TRUE)
CaseMosq4 = merge(CaseMosq_int4, LaggedAvgAbunAvM, by = c("County", "Year"), all.x = TRUE)

CaseMosq_int5 = merge(AvgAbunAfM, dogY, by = c("County", "Year"), all.x= TRUE)
CaseMosq5 = merge(CaseMosq_int5, LaggedAvgAbunAfM, by = c("County", "Year"), all.x = TRUE)

CaseMosq_int6 = merge(AvgAbunCsM, dogY, by = c("County", "Year"), all.x= TRUE)
CaseMosq6 = merge(CaseMosq_int6, LaggedAvgAbunCsM, by = c("County", "Year"), all.x = TRUE)

CaseMosq_int7 = merge(AvgAbunCiM, dogY, by = c("County", "Year"), all.x= TRUE)
CaseMosq7 = merge(CaseMosq_int7, LaggedAvgAbunCiM, by = c("County", "Year"), all.x = TRUE)

CaseMosq_int8 = merge(AvgAbunCxM, dogY, by = c("County", "Year"), all.x= TRUE)
CaseMosq8 = merge(CaseMosq_int8, LaggedAvgAbunCxM, by = c("County", "Year"), all.x = TRUE)

CaseMosq_int9 = merge(AvgAbunCtM, dogY, by = c("County", "Year"), all.x= TRUE)
CaseMosq9 = merge(CaseMosq_int9, LaggedAvgAbunCtM, by = c("County", "Year"), all.x = TRUE)



# prelim plot: total cases against mosquito abundance (from same year)
plot(CaseMosq3$TotalPositive ~ CaseMosq3$AvgAeg, ylim = c(0,400))
abline(lm(CaseMosq3$TotalPositive ~ CaseMosq3$AvgAeg))
summary(lm(CaseMosq3$TotalPositive ~ CaseMosq3$AvgAeg))

plot(CaseMosq2$TotalPositive ~ CaseMosq2$AvgAeb, ylim = c(0,400))
abline(lm(CaseMosq2$TotalPositive ~ CaseMosq2$AvgAeb))
summary(lm(CaseMosq2$TotalPositive ~ CaseMosq2$AvgAeb))

# prelim plot: total cases against mosquito abundance (from prior year)
plot(CaseMosq9$TotalPositive ~ CaseMosq9$LaggedAvgCt, ylim = c(0,400))
abline(lm(CaseMosq9$TotalPositive ~ CaseMosq9$LaggedAvgCt))
summary(lm(CaseMosq9$TotalPositive ~ CaseMosq9$LaggedAvgCt))

# prelim plot: % positive cases against mosquito abundance (from prior year)
plot(CaseMosq9$PerPos ~ CaseMosq9$LaggedAvgCt, ylim = c(0,10))
abline(lm(CaseMosq9$PerPos ~ CaseMosq9$LaggedAvgCt))
summary(lm(CaseMosq9$PerPos ~ CaseMosq9$LaggedAvgCt))



