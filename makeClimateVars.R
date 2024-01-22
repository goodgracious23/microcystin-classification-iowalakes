## Use random forests to build a model of microcystin events in Iowa Lakes

rm(list=ls())

library(raster)
library(tidyr)
library(tidyverse)
library(lubridate)
library(stringr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

## Load in and format data #######################################################

chem.raw <- read.csv("Iowa Lakes Dataset 2000-2022.csv")
chem.raw$sampleDate <- as.POSIXct(chem.raw$sampleDate, format="%m/%d/%Y %H:%M:%S")

#unique(chem.raw$qualFlag) #no quality flags or remarks
#unique(chem.raw$remark)
#unique(chem.raw$name) #what to do with duplicate/variant lake names???
#unique(chem.raw$analyte) #any vars to leave out? Potential duplicate/variant analyte names?

chem.recent.long <- chem.raw[year(chem.raw$sampleDate) >=2017,]
chem.recent.long$year <- year(chem.recent.long$sampleDate)
unique(chem.recent.long$name)
unique(chem.recent.long$analyte)[order(unique(chem.recent.long$analyte))]

#Clarify naming for Silver Lakes
chem.recent.long$name[chem.recent.long$name=="Silver Lake Max Depth" & chem.recent.long$county=="Dickinson"] <-"Silver Lake (Dickinson County)"
chem.recent.long$name[chem.recent.long$name=="Silver Lake Max Depth" & chem.recent.long$county=="Delaware"] <-"Silver Lake (Delaware County)"

#Merge Mitchell Lakes
chem.recent.long$name[chem.recent.long$name=="Mitchell Lake (Deep point)"] <- "Mitchell Lake"

#Drop un-used Lake Okobojis
chem.recent.long <- chem.recent.long[!grepl("West Lake Okoboji", chem.recent.long$name),]

#Drop little-sampled lake
chem.recent.long <- chem.recent.long[chem.recent.long$name != "Cedar Lake",]
chem.recent.long <- chem.recent.long[chem.recent.long$name != "Lake Petocka",]

#reconcile names
chem.recent.long$name <- gsub(" Max Depth","",chem.recent.long$name)
chem.recent.long$name <- gsub(" \\(maximum water depth\\)", "", chem.recent.long$name)

chem.recent.long$name[chem.recent.long$name=="Avenue of the Saints Lake"] <- "Avenue of the Saints Pond"
chem.recent.long$name[chem.recent.long$name=="Badger Creek"] <- "Badger Creek Lake"
chem.recent.long$name[chem.recent.long$name=="Big Creek"] <- "Big Creek Lake"
chem.recent.long$name[chem.recent.long$name=="Crawford Creek Lake"] <- "Crawford Creek Impoundment"
chem.recent.long$name[chem.recent.long$name=="Hooper Pond Lake"] <- "Hooper Area Pond"
chem.recent.long$name[chem.recent.long$name=="Lake Geode"] <- "Geode Lake"
chem.recent.long$name[chem.recent.long$name=="Lake Macbride"] <- "Lake MacBride"
chem.recent.long$name[chem.recent.long$name=="Little River Lake"] <- "Little River Watershed Lake"
chem.recent.long$name[chem.recent.long$name=="Manteno Lake"] <- "Manteno Park Pond"
chem.recent.long$name[chem.recent.long$name=="Moorehead Lake"] <- "Moorhead Park Pond"
chem.recent.long$name[chem.recent.long$name=="Nelson Lake"] <- "Nelson Park Lake"
chem.recent.long$name[chem.recent.long$name=="Pierce Creek Lake"] <- "Pierce Creek Pond"
chem.recent.long$name[chem.recent.long$name=="Rathbun Lake"] <- "Rathbun Reservoir"
chem.recent.long$name[chem.recent.long$name=="Rodgers Lake"] <- "Rodgers Park Lake"
chem.recent.long$name[chem.recent.long$name=="West Lake"] <- "West Lake Osceola"
chem.recent.long$name[chem.recent.long$name=="Rock Creek"] <- "Rock Creek Lake"
chem.recent.long$name[chem.recent.long$name=="Wilson Lake"] <- "Wilson Park Lake"
chem.recent.long$name[chem.recent.long$name=="Yellow Smoke Lake"] <- "Yellow Smoke Park Lake"


chem.recent.long.ann <- chem.recent.long %>%
  group_by(name, year, analyte) %>%
  summarize(result = mean(result, na.rm=TRUE)) %>%
  ungroup()


#reorganize to wide format
chem.recent.wide <- pivot_wider(chem.recent.long.ann, id_cols=c("name","year"),
                                names_from = "analyte", values_from="result")

lake.geography <- chem.recent.long[,c("siteID","name","county","huc8","latitude","longitude")]
lake.geography <- unique(lake.geography)




## total precipitation, Oct-May

path.ppt <- "/Volumes/My Passport for Mac/iMac_Backup_20220711/Documents/Research/DATA/PRISM_4km2_gridded/ppt/"
base.ppt <- "PRISM_ppt_stable_4kmM3_"
years.ppt <- 2016:2021
mos.ppt <- str_pad(1:12, 2, "left", 0)
suffix.ppt <- "_bil.bil"

filenames <- paste0(path.ppt, base.ppt, rep(years.ppt, each=12), rep(mos.ppt, times=length(years.ppt)), suffix.ppt)
filenames <- as.list(filenames)

pptstack <- stack(filenames)

lake.pts <- SpatialPointsDataFrame(coords=cbind(lake.geography$longitude, lake.geography$latitude), 
                                   data=lake.geography, proj4string = CRS(proj4string(pptstack)))

pptmat <- pptstack[lake.pts]



## mean temperature, Mar-May

path.tavg <- "/Volumes/My Passport for Mac/iMac_Backup_20220711/Documents/Research/DATA/PRISM_4km2_gridded/tmean/"
base.tavg <- "PRISM_tmean_stable_4kmM3_"
years.tavg <- 2017:2021
mos.tavg <- str_pad(1:12, 2, "left", 0)
suffix.tavg <- "_bil.bil"

filenames <- paste0(path.tavg, base.tavg, rep(years.tavg, each=12), rep(mos.tavg, times=length(years.tavg)), suffix.tavg)
filenames <- as.list(filenames)

tavgstack <- stack(filenames)



tavgmat <- tavgstack[lake.pts]



lakes <- unique(lake.geography$name)
years <- 2017:2021
ppt <- NULL
springT <- NULL


for(ii in 1:length(lakes)){
  for(jj in 1:length(years)){
    ppt <- c(ppt, sum(pptmat[ii, (rep(years.ppt, each=12)==years[jj] & as.numeric(mos.ppt) %in% 1:5) |
                             (rep(years.ppt, each=12)==years[jj]-1 & as.numeric(mos.ppt) %in% 10:12)], na.rm=T))
    springT <- c(springT, mean(tavgmat[ii, rep(years.tavg, each=12)==years[jj] & as.numeric(mos.ppt) %in% 3:5], na.rm=T))
  }
}


out <- data.frame(name = rep(lakes, each=length(years)), 
                  year = rep(years, times=length(lakes)),
                  ppt = ppt,
                  springT = springT
                  )

write.csv(out, "climatevars.csv", row.names=FALSE)


# plot annual total precipitation

dim(pptmat)


path.ppt <- "/Volumes/My Passport for Mac/iMac_Backup_20220711/Documents/Research/DATA/PRISM_4km2_gridded/ppt/"
base.ppt <- "PRISM_ppt_stable_4kmM3_"
years.ppt <- 1981:2021
mos.ppt <- str_pad(1:12, 2, "left", 0)
suffix.ppt <- "_bil.bil"

filenames <- paste0(path.ppt, base.ppt, rep(years.ppt, each=12), rep(mos.ppt, times=length(years.ppt)), suffix.ppt)
filenames <- as.list(filenames)

pptstack <- stack(filenames)

lake.pts <- SpatialPointsDataFrame(coords=cbind(lake.geography$longitude, lake.geography$latitude), 
                                   data=lake.geography, proj4string = CRS(proj4string(pptstack)))

pptmat <- pptstack[lake.pts]

wateryear <- c(rep(1981,9), rep(1982:2021, each=12), rep(2022,3))
fyear <- 1982:2021

pptmat.annSum <- matrix(NA, nrow=nrow(pptmat), ncol=length(1982:2021))

for(ii in 1:length(1982:2021)){
  pptmat.annSum[,ii] <- apply(pptmat[,wateryear == fyear[ii] ], MARGIN=1, FUN="sum", na.rm=T)
}


pdf("./Figures/precip_timeseries.pdf", width=3.25, height=3.25)
par(mar=c(3.1,3.1,1,1), mgp=c(1.9,0.6,0))
plot(fyear, colMeans(pptmat.annSum), type="b", xlab="Year", ylab="Water year precipitation (mm)",
     cex=0.7)
dev.off()