## Use random forests to build a model of microcystin events in Iowa Lakes

rm(list=ls())

library(randomForest)
library(abind)
library(partykit)
library(mlr)
library(party)
library(tidyr)
library(tidyverse)
library(lubridate)
library(pROC)
library(pdp)
library(RColorBrewer)
#library(rgdal)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

## Load in and format data #######################################################

chem.raw <- read.csv("Iowa Lakes Dataset 2000-2022.csv")
#chem.raw$sampleDate <- as.POSIXct(chem.raw$sampleDate, format="%m/%d/%Y %H:%M:%S") #<-- for some reason this doesn't work on 5/30/24

#unique(chem.raw$qualFlag) #no quality flags or remarks
#unique(chem.raw$remark)
#unique(chem.raw$name) #what to do with duplicate/variant lake names???
#unique(chem.raw$analyte) #any vars to leave out? Potential duplicate/variant analyte names?

#chem.recent.long <- chem.raw[year(chem.raw$sampleDate) >=2017,]
#chem.recent.long$year <- year(chem.recent.long$sampleDate)
chem.recent.long <- chem.raw[chem.raw$year >= 2017,]
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
#lake.geography <- lake.geography[order(lake.geography$name),] #why do these look unique to R but not to humans???
#which(duplicated(cbind(lake.geography$longitude, lake.geography$latitude)))


# Add climate variables--spring (mar-may) Tavg and oct-may ppt
years = 2017:2021

#make this joinable by lake and year, i.e. a n*years by 4 data frame

# Drop/combine some WQ variables
chem.recent.wide <- chem.recent.wide[,colnames(chem.recent.wide)!="Ammonia (AS N)"]
chem.recent.wide$`Total Phosphorus (AS P)`[is.na(chem.recent.wide$`Total Phosphorus (AS P)`)] <- chem.recent.wide$`Phosphate-phosphorus (AS P)`[is.na(chem.recent.wide$`Total Phosphorus (AS P)`)]
chem.recent.wide <- chem.recent.wide[,colnames(chem.recent.wide)!="Phosphate-phosphorus (AS P)"]


# Lake characteristics
lake.char <- read.csv("./Lake_Characteristics.csv")
lake.char <- lake.char[,!colnames(lake.char)=="Total_MC_Detect"]
lake.char$Ecoregion[lake.char$Ecoregion==40] <- "40a"
lake.char$Ecoregion[lake.char$Ecoregion==52] <- "52c"
lake.char$Ecoregion[lake.char$Ecoregion==72] <- "72d"


# # plankton data
# phyt2017 <- read.csv("./2017 mean phytoplankton.csv")[,-1]
# phyt2018 <- read.csv("./2018 mean phytoplankton.csv")[,-1]
# phyt2019 <- read.csv("./2019 mean phytoplankton.csv")[,-1]
# phyt2020 <- read.csv("./2020 mean phytoplankton.csv")[,-1]
# 
# phyt2017$year <- 2017
# phyt2018$year <- 2018
# phyt2019$year <- 2019
# phyt2020$year <- 2020
# colnames(phyt2020) <- colnames(phyt2019)
# 
# phyt <- rbind(phyt2017, phyt2018, phyt2019, phyt2020)
# rm(phyt2017, phyt2018, phyt2019, phyt2020)
# colnames(phyt)[10] <- "Total_Phytos"


zoop2017 <- read.csv("./2017 mean zooplankton.csv")[,-1]
zoop2018 <- read.csv("./2018 mean zooplankton.csv")[,-1]
zoop2019 <- read.csv("./2019 mean zooplankton.csv")[,-1]
zoop2020 <- read.csv("./2020 mean zooplankton.csv")[,-1]

zoop2017$year <- 2017
zoop2018$year <- 2018
zoop2019$year <- 2019
zoop2020$year <- 2020

colnames(zoop2019)[1] <- "LakeID"
zoop2019 <- zoop2019[,match(colnames(zoop2020), colnames(zoop2019)) ]

zoop <- rbind(zoop2017, zoop2018, zoop2019, zoop2020)
colnames(zoop)[6] <- "Total_Zoops"
rm(zoop2017,zoop2018,zoop2019,zoop2020)


climatevars <- read.csv("climatevars.csv")

analysis.data <- right_join(lake.char, chem.recent.wide, by=c("Lake_Name"="name"))
analysis.data <- left_join(analysis.data, zoop, by=c("LakeNo"="LakeID", "year"="year"))
# analysis.data <- left_join(analysis.data, phyt, by=c("LakeNo"="Lake.ID", "year"="year"))
analysis.data <- left_join(analysis.data, climatevars, by=c("Lake_Name"="name", "year"="year"))
analysis.data$MicrocystinDetect <- ifelse(analysis.data$Microcystin > 0.6, 1, 0)


analysis.data <- analysis.data[,!colnames(analysis.data) %in% c("Escherichia coli","Microcystin","Microcystis")]
analysis.data$Ecoregion <- factor(analysis.data$Ecoregion)
analysis.data$MicrocystinDetect <- as.logical(analysis.data$MicrocystinDetect)
#dat.kitchenSink <- left_join(lake.char, chem.recent.wide$)
analysis.data$`Total nitrogen` <- analysis.data$`Inorganic nitrogen (nitrate and nitrite) (AS N)` + analysis.data$`Kjeldahl nitrogen (AS N)`
analysis.data$`N:P ratio` <- (analysis.data$`Total nitrogen`/14.007)/(analysis.data$`Total Phosphorus (AS P)`/30.974)
analysis.data <- analysis.data[!is.na(analysis.data$MicrocystinDetect),]
analysis.data <- analysis.data[,!colnames(analysis.data) == "Depth, bottom"]
analysis.data$drainageRatio <- analysis.data$WatershedArea..km2./analysis.data$Surface.Area..km2.
#dat.raw$TN<-dat.raw$TKN + dat.raw$NO3
#dat.raw$Ratio<-(dat.raw$TN * 1/1000 * 1/14.007)/(dat.raw$TP * 1/1000 * 1/30.974)


prettyNames <- c("Ecoregion", "Surface area", "Max depth", "Mean depth", "Volume", "Watershed Area",
                 "Crop %", "Grassland %", "Water %", "Forest %", "Urban %", expression("NH"[4]),
                 "Alkalinity", "Chlorophyll a", "Secchi depth", "Thermocline depth",
                 "Dissolved oxygen", "Fixed suspended solids", expression("NO"[2]+"NO"[3]),
                 "Kjeldahl N",
                 "Orthophosphate", "pH", "Phycocyanin", "Specific conductance", "Water temperature",
                 "Total dissolved solids", "Total suspended solids", "Turbidity", "Total P",
                 "Cladocera", "Copepoda", "Rotifera", "Daphnia", "Total Zooplankton", "Precip Oct-May",
                 "Spring air temp", "Total N", "N:P ratio","Drainage ratio")
prettyNames2 <- c("Ecoregion", "Surface area", "Max depth", "Mean depth", "Volume", "Watershed Area",
                 "Crop %", "Grassland %", "Water %", "Forest %", "Urban %", expression("NH"[4]),
                 "Alkalinity", "Chlorophyll a", "Secchi depth", "Thermocline depth",
                 "Dissolved oxygen", "Fixed suspended solids", expression("NO"[2]+"NO"[3]),
                 "Kjeldahl N",
                 "Orthophosphate", "pH", "Specific conductance", "Water temperature",
                 "Total dissolved solids", "Total suspended solids", "Turbidity", "Total P",
                 "Cladocera", "Copepoda", "Rotifera", "Daphnia", "Total Zooplankton", "Precip Oct-May",
                 "Spring air temp", "Total N", "N:P ratio", "Drainage ratio")

ecoreg.map <- readOGR("./reg7_eco_l4/reg7_eco_l4.shp")
ecoreg.map <- ecoreg.map[ecoreg.map$STATE_NAME=="Iowa",]
ecoreg.map$US_L4CODE[ecoreg.map$US_L4CODE=="52b"] <- "52c"

## Start doing analyses!

traindat2017 <- analysis.data[analysis.data$year==2017,]
drop1 = c("LakeNo","Lake_Name","Latitude","Longitude","year","Dissolved oxygen (DO)")

mod2017<-cforest(MicrocystinDetect~., data=traindat2017[,!colnames(traindat2017) %in% drop1],
                     controls=cforest_control(ntree=10000, mtry=round(sqrt(ncol(traindat2017)-6-1))))
pred2017<-predict(mod2017)
roc.mod2017<-roc(as.numeric(traindat2017$MicrocystinDetect), as.numeric(pred2017),ci=TRUE)
auc(roc.mod2017)
print(ci(roc.mod2017))
#totacc2017<-sum(traindat2017$MicrocystinDetect==pred2017)/nrow(traindat2017)
#confmat2017<-table(pred2017, traindat2017$MicrocystinDetect, dnn=c("predict","true"))

varimp.mod2017 <- varimp(mod2017)
print(varimp.mod2017[order(varimp.mod2017, decreasing=T)])

pal <- c("#44AA99","#332288","#88CCEE","#DDCC77","#CC6677")
vargroup1 <- factor(x=c("Watershed","Lake","Lake","Lake","Lake","Watershed","Watershed","Watershed",
                        "Watershed","Watershed","Watershed","Chem","Chem","Plankton","Lake","Lake",
                        "Chem","Chem","Chem","Chem","Chem","Chem","Plankton","Chem","Chem","Chem","Chem","Chem",
                        "Chem","Plankton","Plankton","Plankton","Plankton","Plankton","Weather",
                        "Weather","Chem","Chem","Lake"))

pdf("./Figures/varimp_wPhyco_20230705.pdf", width=4, height=7.5)
par(mar=c(4,9.5,0.1,1), mfrow=c(1,1), mgp=c(2.75,0.5,0))
barplot(varimp.mod2017[order(varimp.mod2017, decreasing=F)], las=2, cex.axis=0.8, horiz=TRUE,
        names.arg=prettyNames[order(varimp.mod2017, decreasing=F)], 
        col=pal[vargroup1[order(varimp.mod2017)]], xlab="Variable importance") #visualize variable importance
legend("bottomright", fill=pal, legend=c("Lake chemical","Lake physical","Plankton","Watershed","Weather"),
       bty="n")
dev.off()

pdp.phyco <- partial(mod2017, pred.var="Phycocyanin", train=traindat2017, which.class=2, prob=T)
# pdp.pH <- partial(mod2017, pred.var="pH", train=traindat2017, which.class=2, prob=T)
# pdp.KN <- partial(mod2017, pred.var="Kjeldahl nitrogen (AS N)", train=traindat2017, which.class=2, prob=T)
# pdp.TP <- partial(mod2017, pred.var="Total Phosphorus (AS P)", train=traindat2017, which.class=2, prob=T)
# pdp.Zoop <- partial(mod2017, pred.var="Total_Zoops", train=traindat2017, which.class=2, prob=T)
# pdp.ER <- partial(mod2017, pred.var="Ecoregion", train=traindat2017, which.class=2, prob=T)
# 

pdf("./Figures/pdp_phyco_20230629.pdf", width=3.25, height=3.25)
par(mfrow=c(1,1), mar=c(4.1,4.1,1,1.5), mgp=c(1.9,0.6,0))
plot(pdp.phyco, type="l", xlab="Phycocyanin (mg/L)", lwd=2, ylab="Probability of detection")
abline(v=c(3.3,56), col="grey", lty=2)
rug(traindat2017$Phycocyanin)
dev.off()


# 
# pdf("./Figures/pdp_wPhyco_20230623.pdf", width=6.5, height=6.5)
# par(mfrow=c(3,2), mar=c(4.1,2,1,1.5), oma=c(0,2,0,0))
# plot(pdp.phyco, type="l", xlab="Phycocyanin", ylab="", ylim=c(0.52,0.68))
# text(par("usr")[1]+0.03*diff(par("usr")[1:2]), par("usr")[4]-0.05*diff(par("usr")[3:4]), "a)")
# plot(pdp.pH, type="l", xlab="pH", ylab="", ylim=c(0.52,0.68))
# text(par("usr")[1]+0.03*diff(par("usr")[1:2]), par("usr")[4]-0.05*diff(par("usr")[3:4]), "b)")
# plot(pdp.KN, type="l", xlab="Kjeldahl Nitrogen", ylab="", ylim=c(0.52,0.68))
# text(par("usr")[1]+0.03*diff(par("usr")[1:2]), par("usr")[4]-0.05*diff(par("usr")[3:4]), "c)")
# plot(pdp.TP, type="l", xlab="Total Phosphorous", ylab="", ylim=c(0.52,0.68))
# text(par("usr")[1]+0.03*diff(par("usr")[1:2]), par("usr")[4]-0.05*diff(par("usr")[3:4]), "d)")
# plot(pdp.Zoop, type="l", xlab="Total Zooplankton", ylab="", ylim=c(0.52,0.68))
# text(par("usr")[1]+0.03*diff(par("usr")[1:2]), par("usr")[4]-0.05*diff(par("usr")[3:4]), "e)")
# plot(pdp.ER, xlab="Ecoregion", ylab="", ylim=c(0.52,0.68))
# text(par("usr")[1]+0.03*diff(par("usr")[1:2]), par("usr")[4]-0.05*diff(par("usr")[3:4]), "f)")
# mtext("Probability of microcystin detection", 2, outer=TRUE, line=0.5)
# dev.off()

# refit without phycocyanin
drop2 = c("LakeNo","Lake_Name","Latitude","Longitude","year","Dissolved oxygen (DO)","Phycocyanin")
mod2017b<-cforest(MicrocystinDetect~., data=traindat2017[,!colnames(traindat2017) %in% drop2],
                 controls=cforest_control(ntree=30000, mtry=round(sqrt(ncol(traindat2017)-7-1))))
pred2017b<-predict(mod2017b)
roc.mod2017b<-roc(as.numeric(traindat2017$MicrocystinDetect), as.numeric(pred2017b),ci=TRUE)
auc(roc.mod2017b)
print(ci(roc.mod2017b))

varimp.mod2017b <- varimp(mod2017b)
print(varimp.mod2017b[order(varimp.mod2017b, decreasing=T)])

#pal <- brewer.pal(5,"Set3") ## switch this to a colorblind friendly pallette based on web bookmarked page
pal <- c("#44AA99","#332288","#88CCEE","#DDCC77","#CC6677")
vargroup2 <- factor(x=c("Watershed","Lake","Lake","Lake","Lake","Watershed","Watershed","Watershed",
                        "Watershed","Watershed","Watershed","Chem","Chem","Plankton","Lake","Lake",
                        "Chem","Chem","Chem","Chem","Chem","Chem","Chem","Chem","Chem","Chem","Chem",
                        "Chem","Plankton","Plankton","Plankton","Plankton","Plankton","Weather",
                        "Weather","Chem","Chem","Lake"))

pdf("./Figures/varimp_noPhyco_20230628.pdf", width=4, height=7.5)
par(mar=c(4,9.5,0.1,1), mfrow=c(1,1), mgp=c(2.75,0.5,0))
barplot(varimp.mod2017b[order(varimp.mod2017b, decreasing=F)], las=2, cex.axis=0.8, horiz=TRUE,
        names.arg=prettyNames2[order(varimp.mod2017b, decreasing=F)], 
        col=pal[vargroup2[order(varimp.mod2017b)]], xlab="Variable importance") #visualize variable importance
legend("bottomright", fill=pal, legend=c("Lake chemical","Lake physical","Plankton","Watershed","Weather"),
       bty="n")
dev.off()



pdp.pH2 <- partial(mod2017b, pred.var="pH", train=traindat2017, which.class=2, prob=T)
pdp.KN2 <- partial(mod2017b, pred.var="Kjeldahl nitrogen (AS N)", train=traindat2017, which.class=2, prob=T)
pdp.TP2 <- partial(mod2017b, pred.var="Total Phosphorus (AS P)", train=traindat2017, which.class=2, prob=T)
pdp.TDS <- partial(mod2017b, pred.var="Total dissolved solids", train=traindat2017, which.class=2, prob=T)
pdp.ER2 <- partial(mod2017b, pred.var="Ecoregion", train=traindat2017, which.class=2, prob=T)
pdp.Zoop2 <- partial(mod2017b, pred.var="Total_Zoops", train=traindat2017, which.class=2, prob=T)



ecoreg.map.pdp2 <- ecoreg.map
ecoreg.map.pdp2@data <- left_join(ecoreg.map@data, pdp.ER2, by=c("US_L4CODE"="Ecoregion"))
ramp <- brewer.pal(9, "BuPu")
ramp <- colorRampPalette(c("#BFD3E6","#4D004B"))

pdf("./Figures/pdp_noPhyco_20230629.pdf", width=6.5, height=6.5)
par(mfrow=c(3,2), mar=c(4.1,2,1,1.5), oma=c(0,2,0,0), mgp=c(1.9,0.6,0))

plot(pdp.pH2, type="l", xlab="pH", ylab="", ylim=c(0.51,0.68), lwd=2)
rug(traindat2017$pH)
abline(v=c(8.42, 8.61), col="grey", lty=2)
text(par("usr")[1]+0.03*diff(par("usr")[1:2]), par("usr")[4]-0.05*diff(par("usr")[3:4]), "a)")
plot(pdp.KN2, type="l", xlab="Kjeldahl nitrogen (mg/L)", ylab="", ylim=c(0.51,0.68), lwd=2)
abline(v=c(0.62, 1.69), col="grey", lty=2)
rug(traindat2017$`Kjeldahl nitrogen (AS N)`)
text(par("usr")[1]+0.03*diff(par("usr")[1:2]), par("usr")[4]-0.05*diff(par("usr")[3:4]), "b)")
plot(pdp.TP2, type="l", xlab="Total phosphorous (mg/L)", ylab="", ylim=c(0.51,0.68), lwd=2)
abline(v=c(0.03, 0.07), col="grey", lty=2)
rug(traindat2017$`Total Phosphorus (AS P)`)
text(par("usr")[1]+0.03*diff(par("usr")[1:2]), par("usr")[4]-0.05*diff(par("usr")[3:4]), "c)")
plot(pdp.TDS, type="l", xlab="Total dissolved solids (mg/L)", ylab="", ylim=c(0.51,0.68), lwd=2)
abline(v=c(253,347), col="grey", lty=2)
rug(traindat2017$`Total dissolved solids`)
text(par("usr")[1]+0.03*diff(par("usr")[1:2]), par("usr")[4]-0.05*diff(par("usr")[3:4]), "d)")
#plot(pdp.ER2, xlab="Ecoregion", ylab="", ylim=c(0.51,0.68))
plot(ecoreg.map, col=ramp(12)[cut(ecoreg.map.pdp2$yhat, breaks=12, labels=FALSE)])
text(par("usr")[1]+0.03*diff(par("usr")[1:2]), par("usr")[4]-0.05*diff(par("usr")[3:4]), "e)")
mtext("Ecoregion", cex=2/3)
plot(pdp.Zoop2, type="l", xlab="Total zooplankton (mg/L)", ylab="", ylim=c(0.51,0.68), lwd=2)
abline(v=c(5,155), col="grey", lty=2)
rug(traindat2017$Total_Zoops)
text(par("usr")[1]+0.03*diff(par("usr")[1:2]), par("usr")[4]-0.05*diff(par("usr")[3:4]), "f)")

par(fig=c(0.08,0.42,0.018,0.07), new=TRUE, mar=c(1.75,0,0,0))
image(z=matrix(1:12), col=ramp(12)[1:12], xaxt="n", yaxt="n")
axis(1, at=c(0,0.5,1), labels=c(0.53, 0.57, 0.61))

mtext("Probability of microcystin detection", 2, outer=TRUE, line=0.5)
dev.off()


pdp.specCond <- partial(mod2017b, pred.var = "Specific conductance", train=traindat2017, which.class=2, prob=T)
pdp.secchi <- partial(mod2017b, pred.var = "Depth, Secchi disk depth", train=traindat2017, which.class=2, prob=T)
pdp.NP <- partial(mod2017b, pred.var = "N:P ratio", train=traindat2017, which.class=2, prob=T)
pdp.cope <- partial(mod2017b, pred.var="Copepoda", train=traindat2017, which.class=2, prob=T)
pdp.orp <- partial(mod2017b, pred.var="Orthophosphate (AS P)", train=traindat2017, which.class=2, prob=T)
pdp.clad <- partial(mod2017b, pred.var="Cladocera", train=traindat2017, which.class=2, prob=T)

pdf("./Figures/pdp_noPhyco_next6_20230705.pdf", width=6.5, height=6.5)
par(mfrow=c(3,2), mar=c(4.1,2,1,1.5), oma=c(0,2,0,0), mgp=c(1.9,0.6,0))

plot(pdp.specCond, type="l", xlab="Specific conductance (uS/cm)", ylab="", ylim=c(0.51,0.68), lwd=2)
abline(v=c(389,535), col="grey", lty=2)
rug(traindat2017$`Specific conductance`)
text(par("usr")[1]+0.03*diff(par("usr")[1:2]), par("usr")[4]-0.05*diff(par("usr")[3:4]), "a)")
plot(pdp.secchi, type="l", xlab="Secchi depth (m)", ylab="", ylim=c(0.51,0.68), lwd=2)
abline(v=c(1.46,2.16), col="grey", lty=2)
rug(traindat2017$`Depth, Secchi disk depth`)
text(par("usr")[1]+0.03*diff(par("usr")[1:2]), par("usr")[4]-0.05*diff(par("usr")[3:4]), "b)")
plot(pdp.NP, type="l", xlab="N:P ratio", ylab="", ylim=c(0.51,0.68), lwd=2)
abline(v=c(30,83), col="grey", lty=2)
rug(traindat2017$`N:P ratio`)
text(par("usr")[1]+0.03*diff(par("usr")[1:2]), par("usr")[4]-0.05*diff(par("usr")[3:4]), "c)")
plot(pdp.cope, type="l", xlab="Copepoda (mg/L)", ylab="", ylim=c(0.51,0.68), lwd=2)
abline(v=c(1.02,100), col="grey", lty=2)
rug(traindat2017$Copepoda)
text(par("usr")[1]+0.03*diff(par("usr")[1:2]), par("usr")[4]-0.05*diff(par("usr")[3:4]), "d)")
plot(pdp.orp, xlab="Orthophosphate (mg/L)", ylab="", ylim=c(0.51,0.68), type="l", lwd=2)
abline(v=c(0.005,0.033), col="grey", lty=2)
rug(traindat2017$`Orthophosphate (AS P)`)
text(par("usr")[1]+0.03*diff(par("usr")[1:2]), par("usr")[4]-0.05*diff(par("usr")[3:4]), "e)")
plot(pdp.clad, type="l", xlab="Cladocera (mg/L)", ylab="", ylim=c(0.51,0.68), lwd=2)
abline(v=c(0,128), col="grey", lty=2)
rug(traindat2017$Cladocera)
text(par("usr")[1]+0.03*diff(par("usr")[1:2]), par("usr")[4]-0.05*diff(par("usr")[3:4]), "f)")

mtext("Probability of microcystin detection", 2, outer=TRUE, line=0.5)
dev.off()

## test performance on future years -- using the full model with phycocyanin
testdat2018 <- analysis.data[analysis.data$year==2018,]
pred2018 <- predict(mod2017, newdata=testdat2018[,!colnames(testdat2018) %in% drop1])
roc.pred2018 <- roc(as.numeric(testdat2018$MicrocystinDetect), as.numeric(pred2018), ci=TRUE)
auc(roc.pred2018)
print(ci(roc.pred2018))

testdat2019 <- analysis.data[analysis.data$year==2019,]
pred2019 <- predict(mod2017, newdata=testdat2019[,!colnames(testdat2019) %in% drop1])
roc.pred2019 <- roc(as.numeric(testdat2019$MicrocystinDetect), as.numeric(pred2019), ci=TRUE)
auc(roc.pred2019)
print(ci(roc.pred2019))

testdat2020 <- analysis.data[analysis.data$year==2020,]
pred2020 <- predict(mod2017, newdata=testdat2020[,!colnames(testdat2020) %in% drop1])
roc.pred2020 <- roc(as.numeric(testdat2020$MicrocystinDetect), as.numeric(pred2020), ci=TRUE)
auc(roc.pred2020)
print(ci(roc.pred2020))

testdat2021 <- analysis.data[analysis.data$year==2021,]
pred2021 <- predict(mod2017, newdata=testdat2021[,!colnames(testdat2021) %in% drop1])
roc.pred2021 <- roc(as.numeric(testdat2021$MicrocystinDetect), as.numeric(pred2021), ci=TRUE)
auc(roc.pred2021)
print(ci(roc.pred2021))


## test performance on future years -- dropping phycocyanin
pred2018b <- predict(mod2017b, newdata=testdat2018[,!colnames(testdat2018) %in% drop2])
roc.pred2018b <- roc(as.numeric(testdat2018$MicrocystinDetect), as.numeric(pred2018b), ci=TRUE)
auc(roc.pred2018b)
print(ci(roc.pred2018b))

pred2019b <- predict(mod2017b, newdata=testdat2019[,!colnames(testdat2019) %in% drop2])
roc.pred2019b <- roc(as.numeric(testdat2019$MicrocystinDetect), as.numeric(pred2019b), ci=TRUE)
auc(roc.pred2019b)
print(ci(roc.pred2019b))

pred2020b <- predict(mod2017b, newdata=testdat2020[,!colnames(testdat2020) %in% drop2])
roc.pred2020b <- roc(as.numeric(testdat2020$MicrocystinDetect), as.numeric(pred2020b), ci=TRUE)
auc(roc.pred2020b)
print(ci(roc.pred2020b))

pred2021b <- predict(mod2017b, newdata=testdat2021[,!colnames(testdat2021) %in% drop2])
roc.pred2021b <- roc(as.numeric(testdat2021$MicrocystinDetect), as.numeric(pred2021b), ci=TRUE)
auc(roc.pred2021b)
print(ci(roc.pred2021b))


pdf("./Figures/auc_over_time_20230623.pdf", width=3.25, height=3.25)

par(mar=c(3.6,3.6,1,1), mgp=c(2.2,0.9,0))

plot(2017:2021, c(auc(roc.mod2017), auc(roc.pred2018), auc(roc.pred2019), auc(roc.pred2020), 
                  auc(roc.pred2021)),
     type="l", ylab="AUC", xlab="Year")
lines(2017:2021, c(auc(roc.mod2017b), auc(roc.pred2018b), auc(roc.pred2019b), auc(roc.pred2020b), 
                   auc(roc.pred2021b)),
      col="blue", lty=2)
legend("topright", col=c("black","blue"), lty=1:2, legend=c("with phycocyanin", "without"), bty="n")

dev.off()

## test out a model trained on a random sample of the data -- this is without phycocyanin

# set.seed(11)
# samp <- sample(1:nrow(analysis.data), round((1/3)*nrow(analysis.data)), replace=FALSE)
# traindat.samp <- analysis.data[samp,]
# testdat.samp <- analysis.data[! 1:nrow(analysis.data) %in% samp,]
# 
# trainmod <- cforest(MicrocystinDetect~., data=traindat.samp[,-c(1,2,4,5,16,23,30)],
#                     controls=cforest_control(ntree=10000, mtry=round(sqrt(ncol(traindat.samp)-1))))
# predict.split <- predict(trainmod, newdata=testdat.samp[,-c(1,2,4,5,16,23,30)])
# roc.splitsamp <- roc(as.numeric(testdat.samp$MicrocystinDetect), as.numeric(predict.split), ci=TRUE)
# auc(roc.splitsamp)
# print(ci(roc.splitsamp))
# varimp(trainmod)[order(varimp(trainmod))]

## Iteratively accumulate successive years as training data to predict the next one -- without phycocyanin

traindat2 <- analysis.data[analysis.data$year %in% 2017:2018,]
trainmod.accum1 <- cforest(MicrocystinDetect~., data=traindat2[,!colnames(testdat2018) %in% drop2],
                                     controls=cforest_control(ntree=10000, mtry=round(sqrt(ncol(traindat2)-7-1))))

pred2019c <- predict(trainmod.accum1, newdata=testdat2019[,!colnames(testdat2018) %in% drop2])
roc.pred2019c <- roc(as.numeric(testdat2019$MicrocystinDetect), as.numeric(pred2019c), ci=TRUE)
auc(roc.pred2019c)
print(ci(roc.pred2019c))


traindat3 <- analysis.data[analysis.data$year %in% 2017:2019,]
trainmod.accum2 <- cforest(MicrocystinDetect~., data=traindat3[,!colnames(testdat2018) %in% drop2],
                           controls=cforest_control(ntree=10000, mtry=round(sqrt(ncol(traindat3)-7-1))))

pred2020c <- predict(trainmod.accum2, newdata=testdat2020[,!colnames(testdat2018) %in% drop2])
roc.pred2020c <- roc(as.numeric(testdat2020$MicrocystinDetect), as.numeric(pred2020c), ci=TRUE)
auc(roc.pred2020c)
print(ci(roc.pred2020c))


traindat4 <- analysis.data[analysis.data$year %in% 2017:2020,]
trainmod.accum3 <- cforest(MicrocystinDetect~., data=traindat4[,!colnames(testdat2018) %in% drop2],
                           controls=cforest_control(ntree=10000, mtry=round(sqrt(ncol(traindat4)-7-1))))

pred2021c <- predict(trainmod.accum3, newdata=testdat2021[,!colnames(testdat2018) %in% drop2])
roc.pred2021c <- roc(as.numeric(testdat2021$MicrocystinDetect), as.numeric(pred2021c), ci=TRUE)
auc(roc.pred2021c)
print(ci(roc.pred2021c))


pdf("./Figures/auc_accumTraining_20230623.pdf", width=3.25, height=3.25)

par(mar=c(3.6,3.6,1,1), mgp=c(2.2,0.9,0))

plot(2017:2021, c(auc(roc.mod2017b), auc(roc.pred2018b), auc(roc.pred2019b), auc(roc.pred2020b), 
                  auc(roc.pred2021b)),
     type="l", ylab="AUC", xlab="Year")
lines(2019:2021, c(auc(roc.pred2019c), auc(roc.pred2020c), auc(roc.pred2021c)),
      col="blue", lty=2)
legend("topright", col=c("black","blue"), lty=1:2, legend=c("2017 training", "accumulated training"), bty="n")

dev.off()


## how well can we predict microcystin based on early-season obs?

chem.recent.long$firstSamp <- 0
lakes <- unique(chem.recent.long$name)
years <- 2017:2021
for(lake in lakes){
  for(year in years){
    if(any(chem.recent.long$name==lake & chem.recent.long$year==year)){
      tmp <- chem.recent.long[chem.recent.long$name==lake & chem.recent.long$year==year,]
      chem.recent.long$firstSamp[chem.recent.long$name==lake & chem.recent.long$year==year][tmp$sampleDate==min(tmp$sampleDate)] <- 1
    }
  }
}

table(chem.recent.long$firstSamp)

chem.first.long <- chem.recent.long[chem.recent.long$firstSamp==1,]
chem.first.long <- chem.first.long %>%
  group_by(name, year, analyte) %>%
  summarize(result = mean(result, na.rm=TRUE)) %>%
  ungroup()
chem.first.wide <- pivot_wider(chem.first.long, id_cols=c("name","year"),
                               names_from="analyte", values_from="result")

# Drop/combine some WQ variables
chem.first.wide <- chem.first.wide[,colnames(chem.first.wide)!="Ammonia (AS N)"]
chem.first.wide$`Total Phosphorus (AS P)`[is.na(chem.first.wide$`Total Phosphorus (AS P)`)] <- chem.first.wide$`Phosphate-phosphorus (AS P)`[is.na(chem.first.wide$`Total Phosphorus (AS P)`)]
chem.first.wide <- chem.first.wide[,colnames(chem.first.wide)!="Phosphate-phosphorus (AS P)"]


first.data <- right_join(lake.char, chem.first.wide, by=c("Lake_Name"="name"))
first.data <- left_join(first.data, climatevars, by=c("Lake_Name" = "name", "year" = "year"))
first.data$MicrocystinDetect <- ifelse(first.data$Microcystin > 0.6, 1, 0)
first.data <- first.data[,!colnames(first.data) %in% c("Escherichia coli","Microcystin","Microcystis")]
first.data$Ecoregion <- factor(first.data$Ecoregion)
first.data$MicrocystinDetect <- as.logical(first.data$MicrocystinDetect)
#dat.kitchenSink <- left_join(lake.char, chem.recent.wide$)
first.data$`Total nitrogen` <- first.data$`Inorganic nitrogen (nitrate and nitrite) (AS N)` + first.data$`Kjeldahl nitrogen (AS N)`
first.data$`N:P ratio` <- (first.data$`Total nitrogen`/14.007)/(first.data$`Total Phosphorus (AS P)`/30.974)
first.data <- first.data[!is.na(first.data$MicrocystinDetect),]
first.data$drainageRatio <- first.data$WatershedArea..km2./first.data$Surface.Area..km2.
first.data$Cladocera <- as.numeric(NA)
first.data$Copepoda <- as.numeric(NA)
first.data$Rotifera <- as.numeric(NA)
first.data$Daphnia <- as.numeric(NA)
first.data$Total_Zoops <- as.numeric(NA)

all(colnames(analysis.data) %in% colnames(first.data))

test2017.first <- first.data[first.data$year==2017,]
pred2017.first <- predict(mod2017b, newdata=test2017.first) # <----- which model do we want to use for predictions???
roc2017.first <- roc(as.numeric(traindat2017$MicrocystinDetect), as.numeric(pred2017.first), ci=TRUE)
auc(roc2017.first)
ci(roc2017.first)
  
test2018.first <- first.data[first.data$year==2018,]
pred2018.first <- predict(mod2017b, newdata=test2018.first) # <----- which model do we want to use for predictions???
roc2018.first <- roc(as.numeric(testdat2018$MicrocystinDetect), as.numeric(pred2018.first), ci=TRUE)
auc(roc2018.first)
ci(roc2018.first)

test2019.first <- first.data[first.data$year==2019,]
pred2019.first <- predict(trainmod.accum1, newdata=test2019.first) # <----- which model do we want to use for predictions???
roc2019.first <- roc(as.numeric(testdat2019$MicrocystinDetect), as.numeric(pred2019.first), ci=TRUE)
auc(roc2019.first)
ci(roc2019.first)

test2020.first <- first.data[first.data$year==2020,]
pred2020.first <- predict(trainmod.accum2, newdata=test2020.first) # <----- which model do we want to use for predictions???
roc2020.first <- roc(as.numeric(testdat2020$MicrocystinDetect), as.numeric(pred2020.first), ci=TRUE)
auc(roc2020.first)
ci(roc2020.first)

test2021.first <- first.data[first.data$year==2021,]
pred2021.first <- predict(trainmod.accum3, newdata=test2021.first) # <----- which model do we want to use for predictions???
roc2021.first <- roc(as.numeric(testdat2021$MicrocystinDetect), as.numeric(pred2021.first), ci=TRUE)
auc(roc2021.first)
ci(roc2021.first)


pdf("./Figures/auc_all_vs_earlyPrediction_20230623.pdf", width=3.25, height=3.25)

par(mar=c(3.6,3.6,1,1), mgp=c(2.2,0.9,0))

plot(2017:2021, c(auc(roc.mod2017), auc(roc.pred2018), auc(roc.pred2019), auc(roc.pred2020), 
                  auc(roc.pred2021)),
     type="l", ylab="AUC", xlab="Year", ylim=c(0.6,1))
lines(2017:2021, c(auc(roc2017.first), auc(roc2018.first), auc(roc2019.first), auc(roc2020.first), 
                   auc(roc2021.first)),
      col="blue", lty=2)
legend("topright", col=c("black","blue"), lty=1:2, legend=c("all data", "first sample only"), bty="n")

dev.off()



## make 3-panel combined AUC figure
x1 <- c(2018, 2018.95, 2019.95, 2020.95)
x2 <- c(2019.05, 2020.05, 2021.05)
x3 <- 2017:2021 - 0.05
x4 <- 2017:2021 + 0.05

pdf("./Figures/auc_multipanel_20230703.pdf", width=6.5, height=3.25)

par(mfrow=c(1,2), mar=c(2,2,1,1.5), oma=c(1.5,1.5,0,0), mgp=c(1.9,0.6,0))

# plot(2018:2021, c(auc(roc.pred2018b), auc(roc.pred2019b), auc(roc.pred2020b), auc(roc.pred2021b)), 
#      type="b", xlim=c(2017,2021), xlab="", ylab="AUC", ylim=c(0.6,01))
# points(2017, auc(roc.mod2017b))
# abline(v=2017.5, lty=2, col="grey")

plot(x1, c(auc(roc.pred2018b), auc(roc.pred2019b), auc(roc.pred2020b), auc(roc.pred2021b)),
     type="b", ylab="AUC", xlab="Year", xlim=c(2016.5,2021.5), ylim=c(0.5,1))
arrows(x0=c(2017,x1), 
       y0=c(ci(roc.mod2017b)[1], ci(roc.pred2018b)[1], ci(roc.pred2019b)[1], ci(roc.pred2020b)[1], ci(roc.pred2021b)[1]),
       x1=c(2017,x1), 
       y1=c(ci(roc.mod2017b)[3], ci(roc.pred2018b)[3], ci(roc.pred2019b)[3], ci(roc.pred2020b)[3], ci(roc.pred2021b)[3]), 
       code=3, angle=90, length=0.025)
lines(x2, c(auc(roc.pred2019c), auc(roc.pred2020c), auc(roc.pred2021c)),
      col="#CC6677", type="b", lty=2)
arrows(x0=x2,
       y0=c(ci(roc.pred2019c)[1], ci(roc.pred2020c)[1], ci(roc.pred2021c)[1]),
       x1=x2,
       y1=c(ci(roc.pred2019c)[3], ci(roc.pred2020c)[3], ci(roc.pred2021c)[3]),
       code=3, angle=90, length=0.025, col="#CC6677")
points(2017, auc(roc.mod2017b))
abline(v=2017.5, lty=2, col="grey")
legend("topright", col=c("black","#CC6677"), lty=1:2, legend=c("2017 training", "accumulated training"), bty="n", cex=0.7)
text(par("usr")[1]+0.06*diff(par("usr")[1:2]), par("usr")[4]-0.05*diff(par("usr")[3:4]), "a)")

plot(x3, c(auc(roc.mod2017b), auc(roc.pred2018b), auc(roc.pred2019c), auc(roc.pred2020c), auc(roc.pred2021c)),
     type="b", ylab="AUC", xlab="Year", xlim=c(2016.5,2021.5), ylim=c(0.5,1))
arrows(x0=x3,
       y0=c(ci(roc.mod2017b)[1], ci(roc.pred2018b)[1], ci(roc.pred2019b)[1], ci(roc.pred2020b)[1], ci(roc.pred2021b)[1]), 
       x1=x3,
       y1=c(ci(roc.mod2017b)[3], ci(roc.pred2018b)[3], ci(roc.pred2019b)[3], ci(roc.pred2020b)[3], ci(roc.pred2021b)[3]),
       code=3, angle=90, length=0.025)
lines(x4, c(auc(roc2017.first), auc(roc2018.first), auc(roc2019.first), auc(roc2020.first), auc(roc2021.first)),
      type="b", col="#CC6677", lty=2)
arrows(x0=x4,
       y0=c(ci(roc2017.first)[1], ci(roc2018.first)[1], ci(roc2019.first)[1], ci(roc2020.first)[1], ci(roc2021.first)[1]),
       x1=x4,
       y1=c(ci(roc2017.first)[3], ci(roc2018.first)[3], ci(roc2019.first)[3], ci(roc2020.first)[3], ci(roc2021.first)[3]),
       code=3, angle=90, length=0.025, col="#CC6677")
legend("topright", col=c("black","#CC6677"), lty=1:2, legend=c("all observations", "early observations only"), bty="n", cex=0.7)
text(par("usr")[1]+0.06*diff(par("usr")[1:2]), par("usr")[4]-0.05*diff(par("usr")[3:4]), "b)")
mtext("Year", side=1, outer=TRUE)
mtext("AUC", side=2, outer=TRUE)
      
dev.off()




### Investigate how frequently lakes change class and whether the model predicts change well


## Empirical data first
detect.data <- analysis.data[,colnames(analysis.data) %in% c("Lake_Name","year","MicrocystinDetect")]
table(detect.data$MicrocystinDetect, detect.data$year)

change.check1 <- function(dat.y1, dat.y2){
  out <- 0
  for(ii in 1:nrow(dat.y1)){
    if(any(dat.y1$Lake_Name[ii]==dat.y2$Lake_Name)){
      tmp <- dat.y2[dat.y2$Lake_Name==dat.y1$Lake_Name[ii],]
      if(dat.y1$MicrocystinDetect[ii] != tmp$MicrocystinDetect){
        out <- out + 1
      }
    }
  }
  return(out)
}

change.check1(dat.y1 = detect.data[detect.data$year==2017,], dat.y2 = detect.data[detect.data$year==2018,])
change.check1(dat.y1 = detect.data[detect.data$year==2018,], dat.y2 = detect.data[detect.data$year==2019,])
change.check1(dat.y1 = detect.data[detect.data$year==2019,], dat.y2 = detect.data[detect.data$year==2020,])
change.check1(dat.y1 = detect.data[detect.data$year==2020,], dat.y2 = detect.data[detect.data$year==2021,])


performance <- function(real, pred.prob, thresh = seq(from=0, to=1, by=0.01)){
  
  sensitivity <- rep(NA, length(thresh))
  specificity <- rep(NA, length(thresh))
  youdensJ <- rep(NA, length(thresh))
  
  for(ii in 1:length(thresh)){
    
    pred.ii <- ifelse(pred.prob >= thresh[ii], 1, 0)
    
    TP <- sum(real==1 & pred.ii==1, na.rm=TRUE)
    FP <- sum(real==0 & pred.ii==1, na.rm=TRUE)
    TN <- sum(real==0 & pred.ii==0, na.rm=TRUE)
    FN <- sum(real==1 & pred.ii==0, na.rm=TRUE)
    
    sensitivity[ii] <- TP/(TP + FN)
    specificity[ii] <- TN/(FP + TN)
    
    yj <- sensitivity + specificity - 1
  }
  
  return(list(
    thresh.optim = thresh[which.max(yj)],
    pred.optim = ifelse(pred.prob >= thresh[which.max(yj)], 1, 0),
    sensitivity.optim = sensitivity[which.max(yj)],
    specificity.optim = specificity[which.max(yj)],
    sensitivity = sensitivity,
    specificity = specificity,
    yj = yj
  ))
  
}

class.2017 <- performance(traindat2017$MicrocystinDetect, pred2017b)
table(class.2017$pred.optim)
table(class.2017$pred.optim, traindat2017$MicrocystinDetect)

class.2018 <- performance(testdat2018$MicrocystinDetect, pred2018b)
table(class.2018$pred.optim)
table(class.2018$pred.optim, testdat2018$MicrocystinDetect)

class.2019 <- performance(testdat2019$MicrocystinDetect, pred2019b)
table(class.2019$pred.optim)
table(class.2019$pred.optim, testdat2019$MicrocystinDetect)

class.2020 <- performance(testdat2020$MicrocystinDetect, pred2020b)
table(class.2020$pred.optim)
table(class.2020$pred.optim, testdat2020$MicrocystinDetect)

class.2021 <- performance(testdat2021$MicrocystinDetect, pred2021b)
table(class.2021$pred.optim)
table(class.2021$pred.optim, testdat2021$MicrocystinDetect)



### this is not behaving well when no. of sampled lakes changes over time!


change.check2 <- function(dat.y1, dat.y2, pred.y2){
  out <- 0
  for(ii in 1:nrow(dat.y1)){
    if(any(dat.y1$Lake_Name[ii]==dat.y2$Lake_Name)){
      tmp1 <- dat.y2[dat.y2$Lake_Name==dat.y1$Lake_Name[ii],]
      if(dat.y1$MicrocystinDetect[ii] != tmp1$MicrocystinDetect){
        tmp2 <- pred.y2[dat.y2$Lake_Name==dat.y1$Lake_Name[ii]]
        if(tmp1$MicrocystinDetect == tmp2){
          out <- out + 1
        }
      }
    }
  }
  return(out)
}

change.check2(dat.y1 = detect.data[detect.data$year==2017,], 
              dat.y2 = detect.data[detect.data$year==2018,],
              pred.y2 = class.2018$pred.optim)
change.check2(dat.y1 = detect.data[detect.data$year==2018,], 
              dat.y2 = detect.data[detect.data$year==2019,],
              pred.y2 = class.2019$pred.optim)
change.check2(dat.y1 = detect.data[detect.data$year==2019,], 
              dat.y2 = detect.data[detect.data$year==2020,],
              pred.y2 = class.2020$pred.optim)
change.check2(dat.y1 = detect.data[detect.data$year==2020,], 
              dat.y2 = detect.data[detect.data$year==2021,],
              pred.y2 = class.2021$pred.optim)


(sum(class.2018$pred.optim[detect.data2018$Lake_Name[detect.data2018$MicrocystinDetect==0] %in%
                             detect.data2017$Lake_Name[detect.data2017$MicrocystinDetect==1]] == 0)
+ sum(class.2018$pred.optim[detect.data2018$Lake_Name[detect.data2018$MicrocystinDetect==1] %in%
                              detect.data2017$Lake_Name[detect.data2017$MicrocystinDetect==0]] == 1))

(sum(class.2019$pred.optim[detect.data2019$Lake_Name[detect.data2019$MicrocystinDetect==0] %in%
                              detect.data2018$Lake_Name[detect.data2018$MicrocystinDetect==1]] == 0)
  + sum(class.2019$pred.optim[detect.data2019$Lake_Name[detect.data2019$MicrocystinDetect==1] %in%
                                detect.data2018$Lake_Name[detect.data2018$MicrocystinDetect==0]] == 1))

(sum(class.2020$pred.optim[detect.data2019$Lake_Name[detect.data2019$MicrocystinDetect==1] %in%
                             detect.data2020$Lake_Name[detect.data2020$MicrocystinDetect==0]] == 0)
  + sum(class.2020$pred.optim[detect.data2019$Lake_Name[detect.data2019$MicrocystinDetect==0] %in% 
                                detect.data2020$Lake_Name[detect.data2020$MicrocystinDetect==1]] == 1))

(sum(class.2021$pred.optim[detect.data2020$Lake_Name[detect.data2020$MicrocystinDetect==1] %in%
                             detect.data2021$Lake_Name[detect.data2021$MicrocystinDetect==0]] == 0)
  + sum(class.2021$pred.optim[detect.data2020$Lake_Name[detect.data2020$MicrocystinDetect==0] %in% 
                                detect.data2021$Lake_Name[detect.data2021$MicrocystinDetect==1]] == 1))
