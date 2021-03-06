###########################
# File: Draft Day.R
# Description: Continually recalculates optimal team given which players are available
# Date: 3/3/2013
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# To do:
###########################

#Specify Maximum Risk
maxRisk <- 4.3

#Library
library("Rglpk")

#Functions
source(paste(getwd(), "/R Scripts/Functions/Global Settings.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings_", league, ".R", sep=""))

#Load data
load(paste(getwd(),"/Data/Historical Files/BidUpToSimulation_", league, "-2014.RData", sep=""))
load(paste(getwd(),"/Data/Historical Files/IDP-2014.RData", sep=""))
load(paste(getwd(),"/Data/Historical Files/kickers-2014.RData", sep=""))

#Subset data
draftData <- projections[,c("name","pos","team","projections","vor","simulation","sdPick","sdPts","risk","avgCost","inflatedCost","bidUpTo","bidUpToSim")] #projectedPtsLatent
draftData <- draftData[order(-draftData$vor),]
row.names(draftData) <- 1:dim(draftData)[1]

#Save data
save(draftData, file = paste(getwd(),"/Data/Historical Files/DraftDay_", league, "-2014.RData", sep=""))
write.csv(draftData, file=paste(getwd(),"/Data/Historical Files/DraftDay_", league, "-2014.csv", sep=""), row.names=FALSE)

options(digits=2)
draftData

#Day of Draft
removedPlayers <-  draftData[row.names(na.omit(draftData[,c("projections","simulation","risk","inflatedCost")])),] #projectedPtsLatent
row.names(removedPlayers) <- 1:dim(removedPlayers)[1]
removedPlayers

### RUN TO HERE ###

#Example: Update with drafted (i.e., unavailable) players
myteam <- data.frame(
  player = c("Arian Foster", "Tom Brady", "Jacob Tamme"),
  pos = c("RB", "QB", "TE"),
  cost = c(64, 46, 5)
)
myteam$player <- as.character(myteam$player)

drafted <- c(myteam$player,"Vincent Jackson","Eric Decker")

optimizeDraft(maxRisk=4.3)
optimizeDraft(maxRisk=4.3, omit=c("Adrian Peterson","Eric Decker"))
optimizeDraft(maxRisk=4.3, omit=drafted)

draftData[!(draftData$name %in% drafted),]

###################
### Draft Dashboard
###################

###--UPDATE--###
myteam <- data.frame(
  player = c(),
  position = c(),
  cost = c()
  )
myteam$player <- as.character(myteam$player)

drafted <- c(myteam$player)
###----------###

### Optimize Team ###
# Projected Points
optimizeDraft(maxRisk=5.0, omit=drafted)
optimizeDraft(maxRisk=4.1, omit=drafted) #From Optimum Risk.R #1554
optimizeDraft(maxRisk=3.3, omit=drafted) #From Simulation.R   #1532
optimizeDraft(maxRisk=100, omit=drafted)                      #1568

# Simulated Points
optimizeDraft(maxRisk=5.0, omit=drafted, points=removedPlayers$simulation) #From Optimum Risk.R
sum(draftData[draftData$name %in% optimizeDraft(maxRisk=5.0, omit=drafted, points=removedPlayers$simulation)$players, "projections"]) #1522

optimizeDraft(maxRisk=4.1, omit=drafted, points=removedPlayers$simulation) #From Optimum Risk.R
sum(draftData[draftData$name %in% optimizeDraft(maxRisk=4.1, omit=drafted, points=removedPlayers$simulation)$players, "projections"]) #1494

optimizeDraft(maxRisk=3.3, omit=drafted, points=removedPlayers$simulation) #From Simulation.R
sum(draftData[draftData$name %in% optimizeDraft(maxRisk=3.3, omit=drafted, points=removedPlayers$simulation)$players, "projections"]) #1514

optimizeDraft(maxRisk=100, omit=drafted, points=removedPlayers$simulation)
sum(draftData[draftData$name %in% optimizeDraft(maxRisk=100, omit=drafted, points=removedPlayers$simulation)$players, "projections"]) #1563

### Remaining Players ###
#Player Info
draftData[draftData$name == "Adrian Peterson",]

#All
draftData[!(draftData$name %in% drafted) & !is.na(draftData$projections),]

#QB
draftData[!(draftData$name %in% drafted) & !is.na(draftData$projections) & draftData$pos=="QB",]

#RB
draftData[!(draftData$name %in% drafted) & !is.na(draftData$projections) & draftData$pos=="RB",]

#WR
draftData[!(draftData$name %in% drafted) & !is.na(draftData$projections) & draftData$pos=="WR",]

#TE
draftData[!(draftData$name %in% drafted) & !is.na(draftData$projections) & draftData$pos=="TE",]

### Starters ###
#All
draftData[!(draftData$name %in% drafted) & draftData$vor>0 & draftData$risk < 5 & !is.na(draftData$projections),]

#QB
draftData[!(draftData$name %in% drafted) & draftData$vor>0 & draftData$risk < 5 & !is.na(draftData$projections) & draftData$pos=="QB",]

#RB
draftData[!(draftData$name %in% drafted) & draftData$vor>0 & draftData$risk < 5 & !is.na(draftData$projections) & draftData$pos=="RB",]

#WR
draftData[!(draftData$name %in% drafted) & draftData$vor>0 & draftData$risk < 5 & !is.na(draftData$projections) & draftData$pos=="WR",]

#TE
draftData[!(draftData$name %in% drafted) & draftData$vor>0 & draftData$risk < 5 & !is.na(draftData$projections) & draftData$pos=="TE",]

### Sleepers ###
#All
draftData[!(draftData$name %in% drafted) & draftData$risk >=6 & !(is.na(draftData$risk)) & !is.na(draftData$projections),]

#QB
draftData[!(draftData$name %in% drafted) & draftData$risk >=6 & !(is.na(draftData$risk)) & !is.na(draftData$projections) & draftData$pos=="QB",]

#RB
draftData[!(draftData$name %in% drafted) & draftData$risk >=6 & !(is.na(draftData$risk)) & !is.na(draftData$projections) & draftData$pos=="RB",]

#WR
draftData[!(draftData$name %in% drafted) & draftData$risk >=6 & !(is.na(draftData$risk)) & !is.na(draftData$projections) & draftData$pos=="WR",]

#TE
draftData[!(draftData$name %in% drafted) & draftData$risk >=6 & !(is.na(draftData$risk)) & !is.na(draftData$projections) & draftData$pos=="TE",]

### Kickers ###
kickers[!(kickers$name %in% drafted),]

### Defensive Players ###

#D
IDP[!(IDP$name %in% drafted),]

#DL
IDP[!(IDP$name %in% drafted) & (IDP$pos=="DE" | IDP$pos=="DT"),]

#DB
IDP[!(IDP$name %in% drafted) & (IDP$pos=="S" | IDP$pos=="CB"),]

#DB
IDP[!(IDP$name %in% drafted) & IDP$pos=="LB",]

### Kickers ###
kickers[!(kickers$name %in% drafted),]
