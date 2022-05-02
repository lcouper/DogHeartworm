### Dog Heartworm ####

dog = read.csv("~/Documents/Current_Projects/DogHeartworm/DogHeartWorm_Incidence2.csv", header = T)
# only difference between this and DogHeartWorm_Incidence.csv is that here, 
# Mariposa, Modoc, and Sierra counties & 'unknown' have been removed since they did not have complete data

### Sum cases by year/county ####
dog2 = aggregate(x = dog$TotalPositive, by = list(dog$Year, dog$County, dog$Region), FUN = sum, na.rm = T)
dog3 = aggregate(x = dog$TotalTested, by = list(dog$Year, dog$County, dog$Region), FUN = sum, na.rm = T)
dogY = cbind(dog2, dog3[,4])
colnames(dogY) = c("Year", "County", "Region", "TotalPositive", "TotalTested")
dogY$PerPos = dogY$TotalPositive/dogY$TotalTested * 100
dog4 = aggregate(x = dogY$TotalPositive, by = list(dogY$County, dogY$Region), FUN = sum, na.rm = T)
dog5 = aggregate(x = dogY$PerPos, by = list(dogY$County, dogY$Region), FUN = mean, na.rm = T) 
dogZ = cbind(dog4, dog5[,3])
colnames(dogZ) = c("County","Region", "TotalPositive", "AvgPerPos")
countylist = unique(dogY$County) # 54 unique counties

# For mapping trends
library(tigris)
library(ggplot2)
library(RColorBrewer)
library(scales)

ca = counties("California", cb = TRUE)
colnames(ca)[6] = "County"
CAsummary = merge(ca, dogZ, by = "County")

#### Plot total cases across 2012 - 2021 ####
ggplot(CAsummary, aes(fill=TotalPositive))+
  geom_sf()+
  scale_fill_gradientn(colours=rev(brewer.pal(6,"RdYlBu")),
                       na.value = "grey100", 
                       values =  rescale(seq(0, 400, length.out = 6))) +
  theme_void()


#### Plot average percent positive tests across 2012 - 2021 ####
ggplot(CAsummary, aes(fill=AvgPerPos))+
  geom_sf()+
  scale_fill_gradientn(colours=rev(brewer.pal(6,"RdYlBu")),
                       na.value = "grey100", 
                       values =  rescale(seq(0, 400, length.out = 6))) +
  theme_void()


#### Calculate trend in case counts over time (2012 - 2020) ####
countylist = unique(dogY$County) # 54 unique counties
coefs = vector()

for (i in 1:54)
  {x = dogY[dogY$County == countylist[i],]
  x = x[-(nrow(x)),] # remove last row (2021 data, since we dont have full year)
  l = lm(x$TotalPositive ~ x$Year)
  coefs[i] = as.numeric(l$coefficients[2])}

CAcoefs = cbind(countylist, coefs)
colnames(CAcoefs) = c("County", "coefficients")
CAsummary2 = merge(ca, CAcoefs, by = "County")
CAsummary2$coefficients = as.numeric(CAsummary2$coefficients)

#### Plot trends in total cases 2012 - 2020 ####
ggplot(CAsummary2, aes(fill=coefficients))+
  geom_sf()+
  scale_fill_gradientn(colours=rev(brewer.pal(6,"RdYlBu")),
                       na.value = "grey100", 
                       values =  rescale(seq(0, 400, length.out = 6))) +
  theme_void()


#### Calculate trend in % positive tests over time (2012 - 2020) ###
countylist = unique(dogY$County) # 55 unique counties
coefs2 = vector()

for (i in 1:54)
{x = dogY[dogY$County == countylist[i],]
x = x[-(nrow(x)),] # remove last row (2021 data, since we dont have full year)
l = lm(x$PerPos ~ x$Year)
coefs2[i] = as.numeric(l$coefficients[2])}

CAcoefs2 = cbind(countylist, coefs2)
colnames(CAcoefs2) = c("County", "coefficients2")
CAsummary3 = merge(ca, CAcoefs2, by = "County")
CAsummary3$coefficients2 = as.numeric(CAsummary3$coefficients2)

#### Plot trends in percent positive cases 2012 - 2020 ####
ggplot(CAsummary3, aes(fill=coefficients2))+
  geom_sf()+
  scale_fill_gradientn(colours=rev(brewer.pal(6,"RdYlBu")),
                       na.value = "grey100", 
                       values =  rescale(seq(0, 400, length.out = 6))) +
  theme_void()

# remove Modoc county and replot
CAsummary3noM = CAsummary3
CAsummary3noM$coefficients2[24] = NA

ggplot(CAsummary3noM, aes(fill=coefficients2))+
  geom_sf()+
  scale_fill_gradientn(colours=rev(brewer.pal(6,"RdYlBu")),
                       na.value = "grey100", 
                       values =  rescale(seq(0, 400, length.out = 6))) +
  theme_void()


#### Temporal trends for key counties ####
library(zoo)
library(forecast)
library(dplyr)
# convert months to numbers
dog$Month = recode(dog$Month, January = 1, February = 2, March = 3, April = 4, May = 5,
           June = 6, July = 7, August = 8, September = 9, October = 10,
           November = 11, Decembar = 12)
dog$Date <- as.yearmon(paste(dog$Year, dog$Month), "%Y %m")

# Los Angeles
LA = dog[dog$County == "Los Angeles",]
LA = LA[LA$Year != 2021,] # remove 2021
LA = LA[order(LA$Date),]
plot(LA$TotalPositive ~ LA$Date, type = "l",
     ylab = "# cases", xlab = "", cex.axis = 1.5, cex.lab = 1.5)
LA_ts <- ts(LA$TotalPositive, start = c(2012,1),end = c(2020,12),frequency = 12)
LA_trend = forecast::ma(LA_ts,12)
lines(LA_trend ~ LA$Date, col = "red")

# San Diego
SD = dog[dog$County == "San Diego",]
SD = SD[SD$Year != 2021,] # remove 2021
SD = SD[order(SD$Date),]
plot(SD$TotalPositive ~ SD$Date, type = "l",
     ylab = "# cases", xlab = "", cex.axis = 1.5, cex.lab = 1.5)
SD_ts <- ts(SD$TotalPositive, start = c(2012,1),end = c(2020,12),frequency = 12)
SD_trend = forecast::ma(SD_ts,12)
lines(SD_trend ~ SD$Date, col = "red")

# Riverside
RV = dog[dog$County == "Riverside",]
RV = RV[RV$Year != 2021,] # remove 2021
RV = RV[order(RV$Date),]
plot(RV$TotalPositive ~ RV$Date, type = "l",
     ylab = "# cases", xlab = "", cex.axis = 1.5, cex.lab = 1.5)
RV_ts <- ts(RV$TotalPositive, start = c(2012,1),end = c(2020,12),frequency = 12)
RV_trend = forecast::ma(RV_ts,12)
lines(RV_trend ~ RV$Date, col = "red")

# Santa Barbara
SB = dog[dog$County == "Santa Barbara",]
SB = SB[SB$Year != 2021,] # remove 2021
SB = SB[order(SB$Date),]
plot(SB$TotalPositive ~ SB$Date, type = "l",
     ylab = "# cases", xlab = "", cex.axis = 1.5, cex.lab = 1.5)
SB_ts <- ts(SB$TotalPositive, start = c(2012,1),end = c(2020,12),frequency = 12)
SB_trend = forecast::ma(SB_ts,12)
lines(SB_trend ~ SB$Date, col = "red")

# Placer
PC = dog[dog$County == "Placer",]
PC = PC[PC$Year != 2021,] # remove 2021
PC = PC[order(PC$Date),]
plot(PC$TotalPositive ~ PC$Date, type = "l",
     ylab = "# cases", xlab = "", cex.axis = 1.5, cex.lab = 1.5)
PC_ts <- ts(PC$TotalPositive, start = c(2012,1),end = c(2020,12),frequency = 12)
PC_trend = forecast::ma(PC_ts,12)
lines(PC_trend ~ PC$Date, col = "red")

# Shasta 
SH = dog[dog$County == "Shasta",]
SH = SH[SH$Year != 2021,] # remove 2021
SH = SH[order(SH$Date),]
plot(SH$TotalPositive ~ SH$Date, type = "l",
     ylab = "# cases", xlab = "", cex.axis = 1.5, cex.lab = 1.5)
SH_ts <- ts(SH$TotalPositive, start = c(2012,1),end = c(2020,12),frequency = 12)
SH_trend = forecast::ma(SH_ts,12)
lines(SH_trend ~ SH$Date, col = "red")

# Sacramento
SC = dog[dog$County == "Sacramento",]
SC = SC[SC$Year != 2021,] # remove 2021
SC = SC[order(SC$Date),]
plot(SC$TotalPositive ~ SC$Date, type = "l",
     ylab = "# cases", xlab = "", cex.axis = 1.5, cex.lab = 1.5)
SC_ts <- ts(SC$TotalPositive, start = c(2012,1),end = c(2020,12),frequency = 12)
SC_trend = forecast::ma(SC_ts,12)
lines(SC_trend ~ SC$Date, col = "red")

# Sonoma
SN = dog[dog$County == "Sonoma",]
SN = SN[SN$Year != 2021,] # remove 2021
SN = SN[order(SN$Date),]
plot(SN$TotalPositive ~ SN$Date, type = "l",
     ylab = "# cases", xlab = "", cex.axis = 1.5, cex.lab = 1.5)
SN_ts <- ts(SN$TotalPositive, start = c(2012,1),end = c(2020,12),frequency = 12)
SN_trend = forecast::ma(SN_ts,12)
lines(SN_trend ~ SN$Date, col = "red")

# San Mateo
SM = dog[dog$County == "San Mateo",]
SM = SM[SM$Year != 2021,] # remove 2021
SM = SM[order(SM$Date),]
plot(SM$TotalPositive ~ SM$Date, type = "l",
     ylab = "# cases", xlab = "", cex.axis = 1.5, cex.lab = 1.5)
SM_ts <- ts(SM$TotalPositive, start = c(2012,1),end = c(2020,12),frequency = 12)
SM_trend = forecast::ma(SM_ts,12)
lines(SM_trend ~ SM$Date, col = "red")

# Santa Clara
SA = dog[dog$County == "Santa Clara",]
SA = SA[SA$Year != 2021,] # remove 2021
SA = SA[order(SA$Date),]
plot(SA$TotalPositive ~ SA$Date, type = "l",
     ylab = "# cases", xlab = "", cex.axis = 1.5, cex.lab = 1.5)
SA_ts <- ts(SA$TotalPositive, start = c(2012,1),end = c(2020,12),frequency = 12)
SA_trend = forecast::ma(SA_ts,12)
lines(SA_trend ~ SA$Date, col = "red")


#### Timing of peak case counts by county #####

dog$Month = factor(dog$Month, levels = dog$Month)
countylist = unique(dogY$County) # 54 unique counties
years = 2012:2020

peakcases = as.data.frame(matrix(nrow = 54, ncol = 9))
rownames(peakcases) = countylist
colnames(peakcases) = c("2012", "2013", "2014", "2015", "2016",
                        "2017", "2018", "2019", "2020")


for (i in 1:54){
  x = dog[dog$County == countylist[i],]
  x = x[order(x$Year, x$Month),]
  x = x[x$Year != 2021,] # remove 2021

  for (j in 1:9){
    z = x[x$Year == years[j],]
    max = max(z$TotalPositive)
    months = mean(as.numeric(z[z$TotalPositive == max,2]))
    peakcases[i,j] = months
}
}

# calculate mean & sd for each county (row)
peakcases$mean = rowMeans(peakcases)
matrix = as.matrix(peakcases[,-10])
peakcases$sd = rowSds(matrix)
peakcases$County = countylist

# plot
ggplot(peakcases, 
  aes(x = mean, y = County, xmin = mean-sd, xmax = mean +sd)) +
  geom_point(aes(color = County)) +
  geom_errorbarh(aes(color = County), height=.2)+
  theme_light()

# reorganize counties by average annual temp
avgtemp = read.csv("~/Documents/Current_Projects/DogHeartworm/CountyAvgTemp.csv", header = T)
MergedPeak = merge(peakcases, avgtemp, by = "County")

MergedPeak = MergedPeak[order(MergedPeak$AvgTemp),]
MergedPeak$County = factor(MergedPeak$County, levels = MergedPeak$County)

colfunc <- colorRampPalette(c("red", "blue"))
colorsF = colfunc(54)
MergedPeak$ColorsForMe = colorsF

# plot
ggplot(MergedPeak, 
       aes(x = mean, y = County, xmin = mean-sd, xmax = mean +sd)) +
  geom_point(color = rev(colorsF)) +
  geom_errorbarh(stat = "identity", height=.2, color = rev(colorsF)) + 
  theme_light() 


##### Coyote data (from iNaturalist) ####

coyote = read.csv("~/Documents/Current_Projects/DogHeartworm/Coyote_iNaturalist.csv", header = T)

# aggregate by year
coyote$counter = as.numeric(1)
coyoteA = aggregate(x = coyote$counter, by = list(coyote$Year, coyote$County), FUN = sum)
colnames(coyoteA) = c("Year", "County", "Coyotes")
# create 1-year lagged version
coyoteA$LagCoyotes = c(NA, coyoteA$Coyotes[-212])

# merge with dog heartworm case data
CaseCoyote = merge(dogY, coyoteA, by = c("County", "Year"))

# very basic plot
plot(CaseCoyote$TotalPositive ~ CaseCoyote$LagCoyotes)
abline(lm(CaseCoyote$TotalPositive ~ CaseCoyote$LagCoyotes))
summary(lm(CaseCoyote$TotalPositive ~ CaseCoyote$LagCoyotes))

# remove outlier - trend becomes significant 
CaseCoyote2 = CaseCoyote[-which.max(CaseCoyote$TotalPositive),]
plot(CaseCoyote2$TotalPositive ~ CaseCoyote2$LagCoyotes)
abline(lm(CaseCoyote2$TotalPositive ~ CaseCoyote2$LagCoyotes))
summary(lm(CaseCoyote2$TotalPositive ~ CaseCoyote2$LagCoyotes))





#### Plot CA counties by group (NorCal, Central, SoCal) ####
# add a column for regional designation (Norcal, central, SoCal) based on census regions https://census.ca.gov/regions/

# using ca dataframe from above
regionlink = read.csv("~/Documents/Current_Projects/DogHeartworm/CountyToRegion_Linker.csv", header = TRUE)
CAregions = merge(ca, regionlink, by = "County")
CAregions$Region = factor(CAregions$Region, levels = c("NorCal", "Central", "SoCal"))
CAregions$Includes = factor(CAregions$Includes, levels = c("Yes", "No"))

ggplot(CAregions, aes(fill=Includes))+
  geom_sf() +
  scale_fill_manual(values=c("grey85", "white")) +
  theme_void()

#### Set up for stacked barplot with relative mosquito abundance data by county ####

allsp = read.csv("~/Documents/Current_Projects/DogHeartworm/MosquitoSpeciesData/AllMosquitoSpeciesAnnualAvg.csv", header = T)

# use only 2010 - 2021 data
allsp = allsp[allsp$Year >= 2010, ]

# for each county, calculate avg mosquito abundance from 2010 - 2021 by species
Aes = aggregate(x = allsp$Ae.sierrensis, by = list(allsp$County), FUN = mean, na.rm = T)
Aeb = aggregate(x = allsp$Ae.albopictus, by = list(allsp$County), FUN = mean, na.rm = T)
Aeg = aggregate(x = allsp$Ae.aegypti, by = list(allsp$County), FUN = mean, na.rm = T)
Aev = aggregate(x = allsp$Ae.vexans, by = list(allsp$County), FUN = mean, na.rm = T)
Anf = aggregate(x = allsp$An.freeborni, by = list(allsp$County), FUN = mean, na.rm = T)
Csi = aggregate(x = allsp$Cs.incidencs, by = list(allsp$County), FUN = mean, na.rm = T)
Csn = aggregate(x = allsp$Cs.inornata, by = list(allsp$County), FUN = mean, na.rm = T)
Cxq = aggregate(x = allsp$Cx.quinquefasciatus, by = list(allsp$County), FUN = mean, na.rm = T)
Cxt = aggregate(x = allsp$Cx.tarsalis, by = list(allsp$County), FUN = mean, na.rm = T)

# combine back together
allspAvg = cbind(Aes, Aeb[,2], Aeg[,2], Aev[,2], Anf[,2], Csi[,2], Csn[,2], Cxq[,2], Cxt[,2])
colnames(allspAvg) = c("County", "Ae.sierrensis", "Ae.albopictus", "Ae.aegypti",
                      "Ae.vexans", "An.freeborni", "Cs.incidencs", "Cs.inornata",
                      "Cx.quinquefasciatus", "Cx.tarsalis")
# remove outlier
allspAvg[1,5] <- NA

# convert NaNs to 0
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
allspAvg[is.nan(allspAvg)]<- 0

# for ordering counties based on latitude (roughly) when plotting
CountyNum = c(36, 12, 31, 15, 29, 45, 11, 58, 46, 50, 47, 14, 54, 42, 
                27, 41, 43, 21, 18, 55, 19, 56, 23, 44, 51, 57, 34, 30, 
                49, 35, 52, 40, 39, 6, 28, 20, 37, 16, 8, 48, 53, 22, 17)
allspAvg$Num = CountyNum

# convert from wide to long
library(reshape2)
allspAvgM = melt(allspAvg, id = c("County", "Num"))
colnames(allspAvgM) = c('County', 'Order', 'MosqSpecies', 'AvgAbundance')

allspAvgM$Order = as.character(allspAvgM$Order)
allspAvgM[allspAvgM$Order == "8",2] <- "08"
allspAvgM[allspAvgM$Order == "6",2] <- "06"
# remove Calaveras
allspAvgM = allspAvgM[allspAvgM$County != "Calaveras",]
allspAvgM = allspAvgM[order(allspAvgM$Order),]

# Calculate relative abundances
allspRelAvg = allspAvg
allspRelAvg[,12:20] <- NA
colnames(allspRelAvg)[12:20] = c("Rel_Ae.sierrensis", "Rel_Ae.albopictus", "Rel_Ae.aegypti",
                 "Rel_Ae.vexans", "Rel_An.freeborni", "Rel_Cs.incidencs", "Rel_Cs.inornata", 
                 "Rel_Cx.quinquefasciatus", "Rel_Cx.tarsalis")

for (i in 1:nrow(allspRelAvg))
  {Counties = unique(allspRelAvg$County)
  x = allspRelAvg[allspRelAvg$County == Counties[i],]
  allspRelAvg[i,12] = (x[,2]/(sum(x[,2:10], na.rm = T)))*100
  allspRelAvg[i,13] = (x[,3]/(sum(x[,2:10], na.rm = T)))*100
  allspRelAvg[i,14] = (x[,4]/(sum(x[,2:10], na.rm = T)))*100
  allspRelAvg[i,15] = (x[,5]/(sum(x[,2:10], na.rm = T)))*100
  allspRelAvg[i,16] = (x[,6]/(sum(x[,2:10], na.rm = T)))*100
  allspRelAvg[i,17] = (x[,7]/(sum(x[,2:10], na.rm = T)))*100
  allspRelAvg[i,18] = (x[,8]/(sum(x[,2:10], na.rm = T)))*100
  allspRelAvg[i,19] = (x[,9]/(sum(x[,2:10], na.rm = T)))*100
  allspRelAvg[i,20] = (x[,10]/(sum(x[,2:10], na.rm = T)))*100}
# correct for Calaveras, which has no data
allspRelAvg[3,12:20] <- NA
  
# Relative abundances only
allspRelAvg = allspRelAvg[,c(1, 11:20)]

# melt
allspram = melt(allspRelAvg, id = c("County", "Num"))
allspram$Num = as.character(allspram$Num)
allspram[allspram$Num == "8",2] <- "08"
allspram[allspram$Num == "6",2] <- "06"
# remove Calaveras
allspram = allspram[allspram$County != "Calaveras",]
allspram = allspram[order(allspram$Num),]

# Colors for plotting 
colors <- c("#661100", "#882255", "#CC6677", "#DDCC77", "#999933", "#117733", "#6699CC", "#88CCEE", "#332288")

countyorder = unique(allspram$County)
p1 = ggplot(data = allspram, aes(x = Num, y = value, fill = variable)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45)) 
p1 + scale_fill_manual(values=colors) + scale_x_discrete(labels = countyorder)

countyorder2 = unique(allspAvgM$County)
p2 = ggplot(data = allspAvgM, aes(x = Order, y = AvgAbundance, fill = MosqSpecies)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45)) 
p2 + scale_fill_manual(values=colors) + scale_x_discrete(labels = countyorder2)



### Stacked barplots with mosquito abundance data by region (Nor, Central, SoCal)

regionlink2 = read.csv("~/Documents/Current_Projects/DogHeartworm/CountyToRegion_Linker2.csv", header = TRUE)
MosqsbyRegion = merge(allspAvg, regionlink2, by = "County")
MosqsbyRegion$Region = factor(MosqsbyRegion$Region, levels = c("NorCal", "Central", "SoCal"))

# for each region, calculate average mosquito abundnace across all counties
Ae1 = aggregate(x = MosqsbyRegion$Ae.sierrensis, by = list(MosqsbyRegion$Region), FUN = mean, na.rm = T)
Ab1 = aggregate(x = MosqsbyRegion$Ae.albopictus, by = list(MosqsbyRegion$Region), FUN = mean, na.rm = T)
Ae1 = aggregate(x = MosqsbyRegion$Ae.aegypti, by = list(MosqsbyRegion$Region), FUN = mean, na.rm = T)
Ax1 = aggregate(x = MosqsbyRegion$Ae.vexans, by = list(MosqsbyRegion$Region), FUN = mean, na.rm = T)
Af1 = aggregate(x = MosqsbyRegion$An.freeborni, by = list(MosqsbyRegion$Region), FUN = mean, na.rm = T)
Cs1 = aggregate(x = MosqsbyRegion$Cs.incidencs, by = list(MosqsbyRegion$Region), FUN = mean, na.rm = T)
Ci1 = aggregate(x = MosqsbyRegion$Cs.inornata, by = list(MosqsbyRegion$Region), FUN = mean, na.rm = T)
Cx1 = aggregate(x = MosqsbyRegion$Cx.quinquefasciatus, by = list(MosqsbyRegion$Region), FUN = mean, na.rm = T)
Ct1 = aggregate(x = MosqsbyRegion$Cx.tarsalis, by = list(MosqsbyRegion$Region), FUN = mean, na.rm = T)

# combine back together
allRegionAvg = cbind(Ae1, Ab1[,2], Ae1[,2], Ax1[,2], Af1[,2], Cs1[,2], Ci1[,2], Cx1[,2], Ct1[,2])
colnames(allRegionAvg) = c("Region", "Ae.sierrensis", "Ae.albopictus", "Ae.aegypti",
                       "Ae.vexans", "An.freeborni", "Cs.incidencs", "Cs.inornata",
                       "Cx.quinquefasciatus", "Cx.tarsalis")

# convert from wide to long
allRegionAvgM = melt(allRegionAvg, id = "Region")
colnames(allRegionAvgM) = c('Region', 'MosqSpecies', 'AvgAbundance')

# Plot
p3 = ggplot(data = allRegionAvgM, aes(x = Region, y = AvgAbundance, fill = MosqSpecies)) + 
  geom_bar(stat = "identity") +
 theme_minimal()
p3 + scale_fill_manual(values=colors) 

# Calculate relative abundances
allRegionRelAvg = allRegionAvg
allRegionRelAvg[,11:19] <- NA
colnames(allRegionRelAvg)[11:19] = c("Rel_Ae.sierrensis", "Rel_Ae.albopictus", "Rel_Ae.aegypti",
                                 "Rel_Ae.vexans", "Rel_An.freeborni", "Rel_Cs.incidencs", "Rel_Cs.inornata", 
                                 "Rel_Cx.quinquefasciatus", "Rel_Cx.tarsalis")

for (i in 1:nrow(allRegionRelAvg))
{Regions = unique(allRegionRelAvg$Region)
x = allRegionRelAvg[allRegionRelAvg$Region == Regions[i],]
allRegionRelAvg[i,11] = (x[,2]/(sum(x[,2:10], na.rm = T)))*100
allRegionRelAvg[i,12] = (x[,3]/(sum(x[,2:10], na.rm = T)))*100
allRegionRelAvg[i,13] = (x[,4]/(sum(x[,2:10], na.rm = T)))*100
allRegionRelAvg[i,14] = (x[,5]/(sum(x[,2:10], na.rm = T)))*100
allRegionRelAvg[i,15] = (x[,6]/(sum(x[,2:10], na.rm = T)))*100
allRegionRelAvg[i,16] = (x[,7]/(sum(x[,2:10], na.rm = T)))*100
allRegionRelAvg[i,17] = (x[,8]/(sum(x[,2:10], na.rm = T)))*100
allRegionRelAvg[i,18] = (x[,9]/(sum(x[,2:10], na.rm = T)))*100
allRegionRelAvg[i,19] = (x[,10]/(sum(x[,2:10], na.rm = T)))*100}

# relative abundances only
allRegionRelAvg = allRegionRelAvg[,c(1, 11:19)]

# melt
allRegionRelAvgM =  melt(allRegionRelAvg, id = "Region")
colnames(allRegionRelAvgM) = c("Region", "MosqSpecies", "AvgRelAbundance")

# Plot
p4 = ggplot(data = allRegionRelAvgM, aes(x = Region, y = AvgRelAbundance, fill = MosqSpecies)) + 
  geom_bar(stat = "identity") +
  theme_minimal()
p4 + scale_fill_manual(values=colors) 




#### Set up data frames for piecharts (Scrap) ######
Alameda = allspAvgM[allspAvgM$County == "Alameda",]
Alameda$RelAbun = (Alameda$AvgAbundance/ sum(Alameda$AvgAbundance)) *100 # relative abundance

Butte= allspAvgM[allspAvgM$County == "Butte",]
Butte$RelAbun = (Butte$AvgAbundance/ sum(Butte$AvgAbundance)) *100 # relative abundance

# cache county not actually in CA so skipping for now...

Calaveras= allspAvgM[allspAvgM$County == "Calaveras",]
Calaveras$RelAbun = (Calaveras$AvgAbundance/ sum(Calaveras$AvgAbundance)) *100 # relative abundance

Colusa = allspAvgM[allspAvgM$County == "Colusa",]
Colusa$RelAbun = (Colusa$AvgAbundance/ sum(Colusa$AvgAbundance)) *100 # relative abundance

ContraCosta = allspAvgM[allspAvgM$County == "ContraCosta",]
ContraCosta$RelAbun = (ContraCosta$AvgAbundance/ sum(ContraCosta$AvgAbundance)) *100 # relative abundance

Davis = allspAvgM[allspAvgM$County == "Davis",]
Davis$RelAbun = (Davis$AvgAbundance/ sum(Davis$AvgAbundance)) *100 # relative abundance

Fresno = allspAvgM[allspAvgM$County == "Fresno",]
Fresno$RelAbun = (Fresno$AvgAbundance/ sum(Fresno$AvgAbundance)) *100 # relative abundance

Glenn = allspAvgM[allspAvgM$County == "Glenn",]
Glenn$RelAbun = (Glenn$AvgAbundance/ sum(Glenn$AvgAbundance)) *100 # relative abundance

Imperial = allspAvgM[allspAvgM$County == "Imperial",]
Imperial$RelAbun = (Imperial$AvgAbundance/ sum(Imperial$AvgAbundance)) *100 # relative abundance

Inyo = allspAvgM[allspAvgM$County == "Inyo",]
Inyo$RelAbun = (Inyo$AvgAbundance/ sum(Inyo$AvgAbundance)) *100 # relative abundance

Kern = allspAvgM[allspAvgM$County == "Kern",]
Kern$RelAbun = (Kern$AvgAbundance/ sum(Kern$AvgAbundance)) *100 # relative abundance

Kings = allspAvgM[allspAvgM$County == "Kings",]
Kings$RelAbun = (Kings$AvgAbundance/ sum(Kings$AvgAbundance)) *100 # relative abundance

Lake = allspAvgM[allspAvgM$County == "Lake",]
Lake$RelAbun = (Lake$AvgAbundance/ sum(Lake$AvgAbundance)) *100 # relative abundance

LosAngeles = allspAvgM[allspAvgM$County == "LosAngeles",]
LosAngeles$RelAbun = (LosAngeles$AvgAbundance/ sum(LosAngeles$AvgAbundance)) *100 # relative abundance

Madera = allspAvgM[allspAvgM$County == "Madera",]
Madera$RelAbun = (Madera$AvgAbundance/ sum(Madera$AvgAbundance)) *100 # relative abundance

Marin = allspAvgM[allspAvgM$County == "Marin",]
Marin$RelAbun = (Marin$AvgAbundance/ sum(Marin$AvgAbundance)) *100 # relative abundance

Merced = allspAvgM[allspAvgM$County == "Merced",]
Merced$RelAbun = (Merced$AvgAbundance/ sum(Merced$AvgAbundance)) *100 # relative abundance

Monterey = allspAvgM[allspAvgM$County == "Monterey",]
Monterey$RelAbun = (Monterey$AvgAbundance/ sum(Monterey$AvgAbundance)) *100 # relative abundance

Napa = allspAvgM[allspAvgM$County == "Napa",]
Napa$RelAbun = (Napa$AvgAbundance/ sum(Napa$AvgAbundance)) *100 # relative abundance

Nevada = allspAvgM[allspAvgM$County == "Nevada",]
Nevada$RelAbun = (Nevada$AvgAbundance/ sum(Nevada$AvgAbundance)) *100 # relative abundance

Orange = allspAvgM[allspAvgM$County == "Orange",]
Orange$RelAbun = (Orange$AvgAbundance/ sum(Orange$AvgAbundance)) *100 # relative abundance

Placer = allspAvgM[allspAvgM$County == "Placer",]
Placer$RelAbun = (Placer$AvgAbundance/ sum(Placer$AvgAbundance)) *100 # relative abundance

Riverside = allspAvgM[allspAvgM$County == "Riverside",]
Riverside$RelAbun = (Riverside$AvgAbundance/ sum(Riverside$AvgAbundance)) *100 # relative abundance

Sacramento = allspAvgM[allspAvgM$County == "Sacramento",]
Sacramento$RelAbun = (Sacramento$AvgAbundance/ sum(Sacramento$AvgAbundance)) *100 # relative abundance

SanBenito = allspAvgM[allspAvgM$County == "SanBenito",]
SanBenito$RelAbun = (SanBenito$AvgAbundance/ sum(SanBenito$AvgAbundance)) *100 # relative abundance

SanBernardino = allspAvgM[allspAvgM$County == "SanBernardino",]
SanBernardino$RelAbun = (SanBernardino$AvgAbundance/ sum(SanBernardino$AvgAbundance)) *100 # relative abundance

SanDiego = allspAvgM[allspAvgM$County == "SanDiego",]
SanDiego$RelAbun = (SanDiego$AvgAbundance/ sum(SanDiego$AvgAbundance)) *100 # relative abundance

SanFrancisco = allspAvgM[allspAvgM$County == "SanFrancisco",]
SanFrancisco$RelAbun = (SanFrancisco$AvgAbundance/ sum(SanFrancisco$AvgAbundance)) *100 # relative abundance

SanJoaquin = allspAvgM[allspAvgM$County == "SanJoaquin",]
SanJoaquin$RelAbun = (SanJoaquin$AvgAbundance/ sum(SanJoaquin$AvgAbundance)) *100 # relative abundance

SanLuisObispo = allspAvgM[allspAvgM$County == "SanLuisObispo",]
SanLuisObispo$RelAbun = (SanLuisObispo$AvgAbundance/ sum(SanLuisObispo$AvgAbundance)) *100 # relative abundance

SanMateo = allspAvgM[allspAvgM$County == "SanMateo",]
SanMateo$RelAbun = (SanMateo$AvgAbundance/ sum(SanMateo$AvgAbundance)) *100 # relative abundance

Shasta = allspAvgM[allspAvgM$County == "Shasta",]
Shasta$RelAbun = (Shasta$AvgAbundance/ sum(Shasta$AvgAbundance)) *100 # relative abundance

Sonoma = allspAvgM[allspAvgM$County == "Sonoma",]
Sonoma$RelAbun = (Sonoma$AvgAbundance/ sum(Sonoma$AvgAbundance)) *100 # relative abundance

Sutter = allspAvgM[allspAvgM$County == "Sutter",]
Sutter$RelAbun = (Sutter$AvgAbundance/ sum(Sutter$AvgAbundance)) *100 # relative abundance


Tehama = allspAvgM[allspAvgM$County == "Tehama",]
Tehama$RelAbun = (Tehama$AvgAbundance/ sum(Tehama$AvgAbundance)) *100 # relative abundance

Yolo = allspAvgM[allspAvgM$County == "Yolo",]
Yolo$RelAbun = (Yolo$AvgAbundance/ sum(Yolo$AvgAbundance)) *100 # relative abundance


Yuba = allspAvgM[allspAvgM$County == "Yuba",]
Yuba$RelAbun = (Yuba$AvgAbundance/ sum(Yuba$AvgAbundance)) *100 # relative abundance


##### Plots of county-specific pie charts of average abundance (scrap) #####
# average from 2010 - 2021 by county

# Alameda
ggplot(Alameda, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

# Butte
ggplot(Butte, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

# Cache not actually in CA

# Calaveras
ggplot(Calaveras, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

# Colusa
ggplot(Colusa, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

# Contra Costa
ggplot(ContraCosta, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

# Davis
ggplot(Davis, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

# Fresno
ggplot(Fresno, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

# Glenn
ggplot(Glenn, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

# Imperial
ggplot(Imperial, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

# Inyo
ggplot(Inyo, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

# Kern
ggplot(Kern, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

# Kings
ggplot(Kings, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

# Lake
ggplot(Lake, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

# LosAngeles
ggplot(LosAngeles, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

# Madera
ggplot(Madera, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

# Marin
ggplot(Marin, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

# Merced
ggplot(Merced, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

# Monterey
ggplot(Monterey, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

# Napa
ggplot(Napa, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

# Nevada
ggplot(Nevada, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

# Orange
ggplot(Orange, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

# Placer
ggplot(Placer, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

# Riverside
ggplot(Riverside, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

# Sacramento
ggplot(Sacramento, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

# San Benito
ggplot(SanBenito, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

# San Bernardino
ggplot(SanBernardino, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

# San Diego
ggplot(SanDiego, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

# San Francisco
ggplot(SanFrancisco, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

# San Joaquin
ggplot(SanJoaquin, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

# San Luis Obispo
ggplot(SanLuisObispo, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

# San Mateo
ggplot(SanMateo, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

# Shasta
ggplot(Shasta, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

# Sonoma
ggplot(Sonoma, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

# Sutter
ggplot(Sutter, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

# Tehama
ggplot(Tehama, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

# Yolo
ggplot(Yolo, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

# Yuba
ggplot(Yuba, aes(x = "", y = RelAbun, fill = MosqSpecies)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = colors) +
  theme_void()

#### Basis stats: Each mosquito species as predcitor ####

allsplag = read.csv("~/Documents/Current_Projects/DogHeartworm/MosquitoSpeciesData/AllMosquitoSpeciesLaggedAnnualAvg.csv", header = T)
alldata = merge(allsplag, dogY, by = c("County", "Year"))

# remove one outlier in cases:
alldata= alldata[-which.max(alldata$TotalPositive),]

# Aedes sierrensis:
# remove outlier in Ae sierrensis abundance
temp = alldata[-which.max(alldata$Lag_Ae.sierrensis),]

# Ae. sierrensis: Plot $ positive cases
plot(temp$PerPos~ temp$Lag_Ae.sierrensis, xlab = "Lagged Ae. sierrensis abundance",
     ylab = "% positive dog heartworm tests")
abline(lm(temp$PerPos ~ temp$Lag_Ae.sierrensis), col = "red")
summary(lm(temp$PerPos ~ temp$Lag_Ae.sierrensis))

# Ae. sierrensis: Plot total # of cases tested
plot(temp$TotalPositive ~ temp$Lag_Ae.sierrensis, xlab = "Lagged Ae. sierrensis abundance",
     ylab = "total # positive dog heartworm tests")
abline(lm(temp$TotalPositive ~ temp$Lag_Ae.sierrensis), col = "red")
summary(lm(temp$TotalPositive ~ temp$Lag_Ae.sierrensis))

# Aedes albopictus
# Percent positive cases
plot(alldata$PerPos ~ alldata$Lag_Ae.albopictus)
# come back to this -- only 6 data points (Orange & Shasta county)
# Look into trap type used -- in home aspirator?

# Aedes aegypti

# Ae. aegypti: Percent positive cases
plot(alldata$PerPos ~ alldata$Lag_Ae.aegypti, xlab = "Lagged Ae. aegypti abundance")
abline(lm(alldata$PerPos ~ alldata$Lag_Ae.aegypti), col = "red")
summary(lm(alldata$PerPos ~ alldata$Lag_Ae.aegypti))

# Ae. aegypti: Plot total # of cases tested
plot(alldata$TotalPositive ~ alldata$Lag_Ae.aegypti, xlab = "Lagged Ae. aegypti abundance",
     ylab = "total # positive dog heartworm tests")
abline(lm(alldata$TotalPositive ~ alldata$Lag_Ae.aegypti), col = "red")
summary(lm(alldata$TotalPositive ~ alldata$Lag_Ae.aegypti))


# Aedes vexans

# Ae. vexans: Percent positive cases
plot(alldata$PerPos ~ alldata$Lag_Ae.vexans, xlab = "Lagged Ae. vexans abundance")
abline(lm(alldata$PerPos ~ alldata$Lag_Ae.vexans), col = "red")
summary(lm(alldata$PerPos ~ alldata$Lag_Ae.vexans))

# Ae. vexans: Plot total # of cases tested
plot(alldata$TotalPositive ~ alldata$Lag_Ae.vexans, xlab = "Lagged Ae. vexans abundance",
     ylab = "total # positive dog heartworm tests")
abline(lm(alldata$TotalPositive ~ alldata$Lag_Ae.vexans), col = "red")
summary(lm(alldata$TotalPositive ~ alldata$Lag_Ae.vexans))

# An. freeborni
# remove 2 outliers
temp2 = alldata[-which.max(alldata$Lag_An.freeborni),]
temp3 = temp2[-which.max(temp2$Lag_An.freeborni),]

# An freeborni: Percent positive cases
plot(temp3$PerPos ~ temp3$Lag_An.freeborni, xlab = "Lagged An. freeborni abundance")
abline(lm(temp3$PerPos ~ temp3$Lag_An.freeborni), col = "red")
summary(lm(temp3$PerPos ~ temp3$Lag_An.freeborni))

# Ae. freeborni: Plot total # of cases tested
plot(temp3$TotalPositive ~ temp3$Lag_An.freeborni, xlab = "Lagged Ae. vexans abundance",
     ylab = "total # positive dog heartworm tests")
abline(lm(temp3$TotalPositive ~ temp3$Lag_An.freeborni), col = "red")
summary(lm(temp3$TotalPositive ~ temp3$Lag_An.freeborni))

# Culiseta incidens
# remove outlier
temp4 = alldata[-which.max(alldata$Lag_Cs.incidencs),]

# Cs incidens: Percent positive cases
plot(temp4$PerPos ~ temp4$Lag_Cs.incidencs, xlab = "Lagged Cs. incidens abundance")
abline(lm(temp4$PerPos ~ temp4$Lag_Cs.incidencs), col = "red")
summary(lm(temp4$PerPos ~ temp4$Lag_Cs.incidencs))

# Cs. incidens Plot total # of cases tested
plot(temp4$TotalPositive ~ temp4$Lag_Cs.incidencs, xlab = "Lagged Cs.incidens abundance",
     ylab = "total # positive dog heartworm tests")
abline(lm(temp4$TotalPositive ~ temp4$Lag_Cs.incidencs), col = "red")
summary(lm(temp4$TotalPositive ~ temp4$Lag_Cs.incidencs))

# Culiseta inornata

# Cs inornata Percent positive cases
plot(alldata$PerPos ~ alldata$Lag_Cs.inornata, xlab = "Lagged Cs. inornata abundance")
abline(lm(alldata$PerPos ~ alldata$Lag_Cs.inornata), col = "red")
summary(lm(alldata$PerPos ~ alldata$Lag_Cs.incidencs))

# Cs inornata: total # cases
plot(alldata$TotalPositive ~ alldata$Lag_Cs.inornata, xlab = "Lagged Cs. inornata abundance")
abline(lm(alldata$TotalPositive ~ alldata$Lag_Cs.inornata), col = "red")
summary(lm(alldata$TotalPositive ~ alldata$Lag_Cs.incidencs))

# Culex quinquefasciatus
# Cx. quinque: % positive cases
plot(alldata$PerPos ~ alldata$Lag_Cx.quinquefasciatus, xlab = "Lagged Cx. quinque abundance")
abline(lm(alldata$PerPos ~ alldata$Lag_Cx.quinquefasciatus), col = "red")
summary(lm(alldata$PerPos ~ alldata$Lag_Cx.quinquefasciatus))

# Cx. quinque: % positive cases
plot(alldata$TotalPositive ~ alldata$Lag_Cx.quinquefasciatus, xlab = "Lagged Cx. quinque abundance")
abline(lm(alldata$TotalPositive ~ alldata$Lag_Cx.quinquefasciatus), col = "red")
summary(lm(alldata$TotalPositive ~ alldata$Lag_Cx.quinquefasciatus))

# Culex tarsalis 
# remove 2 outliers
temp5 = alldata[-which.max(alldata$Lag_Cx.tarsalis),]
temp6 = temp5[-which.max(temp5$Lag_Cx.tarsalis),]

# Cx. tarsalis: % positive tests
plot(temp6$PerPos ~ temp6$Lag_Cx.tarsalis, xlab = "Lagged Cx. tarsalis abundance")
abline(lm(temp6$PerPos ~ temp6$Lag_Cx.tarsalis), col = "red")
summary(lm(temp6$PerPos ~ temp6$Lag_Cx.tarsalis))

# Cx. tarsalis: total cases
plot(temp6$TotalPositive ~ temp6$Lag_Cx.tarsalis, xlab = "Lagged Cx. tarsalis abundance")
abline(lm(temp6$TotalPositive ~ temp6$Lag_Cx.tarsalis), col = "red")
summary(lm(temp6$TotalPositive ~ temp6$Lag_Cx.tarsalis))
