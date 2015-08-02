###########################
# File: Value Over Replacement.R
# Description: Calculates a Player's Value Over a Typical Replacement Starter (for Snake Drafts)
# Date: 3/3/2013
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# To do:
###########################

# Libraries
library(dplyr)
library(ggplot2)

#Functions
source(paste(getwd(), "/R Scripts/Functions/Global Settings.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings_", league, ".R", sep=""))

#Load data
load(paste(getwd(),"/Data/Risk_", league, ".RData", sep=""))

#Calculate Value over Replacement
projectionsRobustAvg <- projections[which(sourceName == "averageRobust"),]

qbValueOfReplacement <- mean(c(projectionsRobustAvg$points[which(projectionsRobustAvg$pos == "QB" & projectionsRobustAvg$positionRank == qbReplacements)], projectionsRobustAvg$points[which(projectionsRobustAvg$pos == "QB" & projectionsRobustAvg$positionRank == qbReplacements-1)], projectionsRobustAvg$points[which(projectionsRobustAvg$pos == "QB" & projectionsRobustAvg$positionRank == qbReplacements+1)]))
rbValueOfReplacement <- mean(c(projectionsRobustAvg$points[which(projectionsRobustAvg$pos == "RB" & projectionsRobustAvg$positionRank == rbReplacements)], projectionsRobustAvg$points[which(projectionsRobustAvg$pos == "RB" & projectionsRobustAvg$positionRank == rbReplacements-1)], projectionsRobustAvg$points[which(projectionsRobustAvg$pos == "RB" & projectionsRobustAvg$positionRank == rbReplacements+1)]))
wrValueOfReplacement <- mean(c(projectionsRobustAvg$points[which(projectionsRobustAvg$pos == "WR" & projectionsRobustAvg$positionRank == wrReplacements)], projectionsRobustAvg$points[which(projectionsRobustAvg$pos == "WR" & projectionsRobustAvg$positionRank == wrReplacements-1)], projectionsRobustAvg$points[which(projectionsRobustAvg$pos == "WR" & projectionsRobustAvg$positionRank == wrReplacements+1)]))
teValueOfReplacement <- mean(c(projectionsRobustAvg$points[which(projectionsRobustAvg$pos == "TE" & projectionsRobustAvg$positionRank == teReplacements)], projectionsRobustAvg$points[which(projectionsRobustAvg$pos == "TE" & projectionsRobustAvg$positionRank == teReplacements-1)], projectionsRobustAvg$points[which(projectionsRobustAvg$pos == "TE" & projectionsRobustAvg$positionRank == teReplacements+1)]))

projections[which(pos == "QB"), vor := points - qbValueOfReplacement]
projections[which(pos == "RB"), vor := points - rbValueOfReplacement]
projections[which(pos == "WR"), vor := points - wrValueOfReplacement]
projections[which(pos == "TE"), vor := points - teValueOfReplacement]

#Calculate ranks by VOR
projections <- projections[order(-vor)][,overallRank := 1:.N, by=list(sourceName)]
projections <- projections[order(-vor)][,positionRank := 1:.N, by=list(sourceName, pos)]

<<<<<<< HEAD
# Calculate drop off as each player's projected points minus the average of the next two players
# at the same potision
qb <- qb %>% 
  arrange(-projections) %>%
  mutate(#nextBestProj = lead(projections),
         #secNextBestProj = lead(projections, 2),
         nextBestAvg = (lead(projections) + lead(projections, 2))/2,
         dropOff = projections - ((lead(projections) + lead(projections, 2))/2)
  )
rb <- rb %>% 
  arrange(-projections) %>%
  mutate(#nextBestProj = lead(projections),
    #secNextBestProj = lead(projections, 2),
    nextBestAvg = (lead(projections) + lead(projections, 2))/2,
    dropOff = projections - ((lead(projections) + lead(projections, 2))/2)
  )
wr <- wr %>% 
  arrange(-projections) %>%
  mutate(#nextBestProj = lead(projections),
    #secNextBestProj = lead(projections, 2),
    nextBestAvg = (lead(projections) + lead(projections, 2))/2,
    dropOff = projections - ((lead(projections) + lead(projections, 2))/2)
  )
te <- te %>% 
  arrange(-projections) %>%
  mutate(#nextBestProj = lead(projections),
    #secNextBestProj = lead(projections, 2),
    nextBestAvg = (lead(projections) + lead(projections, 2))/2,
    dropOff = projections - ((lead(projections) + lead(projections, 2))/2)
  )
  
#Merge across positions
projections <- rbind(qb,rb,wr,te)

#Calculate overall rank by VOR
projections$overallRank <- rank(-projections$vor, ties.method="min")

#Order players by overall rank
projections <- projections[order(projections$overallRank),]
row.names(projections) <- 1:dim(projections)[1]

#Reorder variables
projections <- projections[,c("name","player","pos","team","overallRank","pick","positionRank","projections",paste("projectedPts", sourcesOfProjectionsAbbreviation, sep="_"),"projectedPtsMean","projectedPtsMedian","vor","sdPick","sdPts","risk","nextBestAvg","dropOff")] #,"projectedPtsLatent"
=======
#Select and order variables
keepVars <- finalVarNames[finalVarNames %in% names(projections)]
projections <- projections[,keepVars, with=FALSE]
>>>>>>> upstream/master

#Starters (low risk)
projections[which(projections$risk <= 5 & projections$vor >= 0),]

#Sleepers (high risk)
projections[which(projections$risk >=5 & projections$vor >= 0),]

#Density Plot
ggplot(projections[which(projections$vor >= 0),], aes(x=vor, fill=pos)) + geom_density(alpha=.3) + xlab("Player's Value Over Replacement") + ggtitle("Density Plot of Projected VOR") + theme(legend.title=element_blank())
ggsave(paste0(getwd(), "/Figures/VOR-Density.jpg"), width=10, height=10)
dev.off()

#Boxplot
qplot(pos, vor, data=projections[which(projections$vor >= 0),], geom=c("boxplot", "jitter"), fill=pos, main="Value Over Replacement By Position", xlab="", ylab="Value Over Replacement")
<<<<<<< HEAD
ggsave(paste(getwd(),"/Figures/VOR-Boxplot_",league, ".jpg", sep=""), width=10, height=10)
dev.off()

#Save file
save(projections, file = paste(getwd(),"/Data/VOR_", league, ".RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/VOR_", league, ".csv", sep=""), row.names=FALSE)

save(projections, file = paste(getwd(),"/Data/Historical Files/VOR_", league, "-2014.RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/Historical Files/VOR_", league, "-2014.csv", sep=""), row.names=FALSE)
=======
ggsave(paste0(getwd(), "/Figures/VOR-Boxplot.jpg"), width=10, height=10)
dev.off()

#Save file
save(projections, file = paste0(getwd(), "/Data/VOR.RData"))
write.csv(projections, file = paste0(getwd(), "/Data/VOR.csv"), row.names=FALSE)

save(projections, file = paste0(getwd(), "/Data/Historical Files/VOR-", season, ".RData"))
write.csv(projections, file = paste0(getwd(), "/Data/Historical Files/VOR-", season, ".csv"), row.names=FALSE)
>>>>>>> upstream/master

#Subset data
draftData <- projections[as.numeric(row.names(na.omit(projections[,c("points","vor","risk"), with=FALSE]))), c("name","pos","team","points","vor","sdPick","sdPts","risk"), with=FALSE] #projectedPtsLatent
row.names(draftData) <- 1:dim(draftData)[1]

options(digits=2)
#draftData

#Example: Update with drafted (i.e., unavailable) players
#drafted <- c("Arian Foster","Ray Rice")

#draftData[!(draftData$name %in% drafted),]

###Draft Dashboard
drafted <- c()

#All players
draftData[!(draftData$name %in% drafted),]

#Starters (low risk)
draftData[!(draftData$name %in% drafted) & draftData$risk <=4,]

#Sleepers (high risk)
draftData[!(draftData$name %in% drafted) & draftData$risk >=6,]
