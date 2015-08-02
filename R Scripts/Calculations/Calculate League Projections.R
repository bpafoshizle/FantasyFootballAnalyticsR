###########################
# File: Calculate League Projections.R
# Description: Calculates league projections based on league settings
# Date: 3/2/2013
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# To do:
###########################

#Library
library("plyr")
library("stringr")
library("ggplot2")
library("reshape")
library("MASS")
library("psych")
library("data.table")


#Functions
source(paste(getwd(), "/R Scripts/Functions/Global Settings.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings_", league, ".R", sep=""))

#Import projections data
filenames <- paste(getwd(),"/Data/", sourcesOfProjections, "-Projections.RData", sep="")
listProjections <- sapply(filenames, function(x) get(load(x)), simplify = FALSE)

projections <- rbindlist(listProjections, use.names=TRUE, fill=TRUE)
setkeyv(projections, cols=c("name","pos"))

#Set player name as most common instance across sources
playerNames <- melt(projections,
                    id.vars = c("name","pos"),
                    measure.vars = paste("name", sourcesOfProjectionsAbbreviation, sep="_"),
                    na.rm=TRUE,
                    value.name="player")[,player := names(which.max(table(player))),
                                         by=list(name, pos)][order(name), -3, with=FALSE]

setkeyv(playerNames, cols=c("name","pos"))
projections <- projections[unique(playerNames)]

#Set team name as most common instance across sources
teamNames <- melt(projections,
                    id.vars = c("name","pos"),
                    measure.vars = paste("team", sourcesOfProjectionsAbbreviation, sep="_"),
                    na.rm=TRUE,
                    value.name="team")[,team := names(which.max(table(team))),
                                       by=list(name, pos)][order(name), -3, with=FALSE]

setkeyv(teamNames, cols=c("name","pos"))
projections <- projections[unique(teamNames)]

#Modify team names
projections[name == "ZACHMILLER" & team_espn == "CHI", team := "CHI"]

#Identify duplicate cases
cases <- projections[, c("name","pos","team"), with=FALSE]
uniqueCases <- unique(projections[, c("name","pos","team"), with=FALSE])
duplicateCases <- uniqueCases[duplicated(name) | duplicated(name, fromLast=TRUE)]

<<<<<<< HEAD
#Remove duplicate cases
#projections[projections$name %in% projections$name[duplicated(projections$name)],]
=======
#Different player (Same name, different team)
setkeyv(duplicateCases, c("name", "team"))
duplicateCases[!duplicated(duplicateCases) & !duplicated(duplicateCases, fromLast=TRUE)]
>>>>>>> upstream/master

#Same player (same name and team, different position)
setkeyv(duplicateCases, c("name", "team"))
duplicateCases[duplicated(duplicateCases) | duplicated(duplicateCases, fromLast=TRUE)]

<<<<<<< HEAD

#Same player, different position
dropNames <- c("DENARDROBINSON","DEXTERMCCLUSTER","THEORIDDICK","ORSONCHARLES","JOEWEBB","EMILIGWENAGU","EVANRODRIGUEZ","BRADSMELLEY","RICHIEBROCKEL","BEARPASCOE","JEDCOLLINS","MARCUSTHIGPEN", "ALEXSMITH")
dropVariables <- c("pos","pos","pos","pos","pos","pos","pos","pos","pos","pos","pos","pos", "pos")
dropLabels <- c("RB","WR","WR","TE","WR","RB","TE","TE","RB","RB","TE","WR", "TE")
=======
#Calculate stat categories for each source
projections[,passIncomp := passAtt - passComp]
>>>>>>> upstream/master

#Calculate average of categories
availableVars <- names(projections)[names(projections) %in% scoreCategories]
projectionsAvg <- projections[, lapply(.SD, mean, na.rm=TRUE), by=c("name","player","pos","team"), .SDcols=availableVars]
projectionsAvg$sourceName <- "average"

#Calculate Hodges-Lehmann (pseudo-median) robust average of categories
projectionsRobustAvg <- projections[, lapply(.SD, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE))), by=c("name","player","pos","team"), .SDcols=availableVars]
projectionsRobustAvg$sourceName <- "averageRobust"

#Calculate Weighted Average
setkeyv(projections, cols=c("name","player","pos","team","sourceName"))
projectionsAllSources <- projections[CJ.dt(unique(data.table(name, player, pos, team)), unique(sourceName))] #if error, check for duplicate cases: table(projectionsAllSources$name)[table(projectionsAllSources$name) != length(unique(projections$sourceName))]
weights <- as.vector(sapply(paste("weight", unique(projections$sourceName), sep="_"), get))
allWeights <- rep(weights, nrow(projectionsAllSources)/length(weights))

setkeyv(projectionsAllSources, cols=c("name","player","pos","team","sourceName"))
projectionsWeightedAvg <- projectionsAllSources[, lapply(.SD, function(x) weighted.mean(x, weights, na.rm=TRUE)), by=c("name","player","pos","team"), .SDcols=availableVars]

projectionsWeightedAvg$sourceName <- "averageWeighted"

#Merge
projectionCalculations <- rbind(projectionsAvg, projectionsRobustAvg, projectionsWeightedAvg, fill=TRUE)

projections <- rbind(projections, projectionCalculations, fill=TRUE)

#Set key
setkeyv(projections, cols=c("name","pos","team"))
projections[, playerID := (.GRP), by=c("name","pos","team")]

#If variable is all missing for source, impute mean of other sources
pb <- txtProgressBar(min = 0, max = length(unique(projections$sourceName)), style = 3)
for(i in 1:length(unique(projections$sourceName))){
  setTxtProgressBar(pb, i)
  
  sourceIndex <- unique(projections$sourceName)[i]
  playerIDs <- projections$playerID[which(projections$sourceName == sourceIndex)]
  
  if(sourceIndex != "average" & sourceIndex != "averageRobust" & sourceIndex != "averageWeighted"){
    missingVars <- availableVars[projections[which(projections$sourceName == sourceIndex), apply(.SD, 2, function(x) all(is.na(x))), .SDcols=availableVars]]
    projections[which(projections$sourceName == sourceIndex), (missingVars) := projections[which(projections$sourceName == "average" & projections$playerID %in% playerIDs), missingVars, with=FALSE]]
  }
}

<<<<<<< HEAD
#Remove WalterFootball projections because they don't separate rushing TDs from receiving TDs
projections$projectedPts_wf <- NA

#Calculate average of categories
projections$passAtt <- rowMeans(projections[,paste("passAtt", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$passComp <- rowMeans(projections[,paste("passComp", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$passIncomp <- rowMeans(projections[,paste("passIncomp", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$passYds <- rowMeans(projections[,paste("passYds", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$passTds <- rowMeans(projections[,paste("passTds", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$passInt <- rowMeans(projections[,paste("passInt", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$rushAtt <- rowMeans(projections[,paste("rushAtt", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$rushYds <- rowMeans(projections[,paste("rushYds", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$rushTds <- rowMeans(projections[,paste("rushTds", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$rec <- rowMeans(projections[,paste("rec", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$recYds <- rowMeans(projections[,paste("recYds", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$recTds <- rowMeans(projections[,paste("recTds", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$returnTds <- rowMeans(projections[,paste("returnTds", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$twoPts <- rowMeans(projections[,paste("twoPts", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)
projections$fumbles <- rowMeans(projections[,paste("fumbles", sourcesOfProjectionsAbbreviation, sep="_")], na.rm=TRUE)

#Calculate Hodges-Lehmann (pseudo-median) robust average of categories
projections$passAttMedian <- apply(projections[,paste("passAtt", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$passCompMedian <- apply(projections[,paste("passComp", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$passIncompMedian <- apply(projections[,paste("passIncomp", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$passYdsMedian <- apply(projections[,paste("passYds", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$passTdsMedian <- apply(projections[,paste("passTds", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$passIntMedian <- apply(projections[,paste("passInt", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$rushAttMedian <- apply(projections[,paste("rushAtt", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$rushYdsMedian <- apply(projections[,paste("rushYds", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$rushTdsMedian <- apply(projections[,paste("rushTds", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$recMedian <- apply(projections[,paste("rec", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$recYdsMedian <- apply(projections[,paste("recYds", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$recTdsMedian <- apply(projections[,paste("recTds", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$returnTdsMedian <- apply(projections[,paste("returnTds", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$twoPtsMedian <- apply(projections[,paste("twoPts", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))
projections$fumblesMedian <- apply(projections[,paste("fumbles", sourcesOfProjectionsAbbreviation, sep="_")], 1, function(x) tryCatch(wilcox.test(x, conf.int=TRUE, na.action="na.exclude")$estimate, error=function(e) median(x, na.rm=TRUE)))

#Check projections
# projections[,c("name",paste("passAtt", sourcesOfProjectionsAbbreviation, sep="_"), c("passAtt","passAttMedian"))]
# projections[,c("name",paste("passComp", sourcesOfProjectionsAbbreviation, sep="_"), c("passComp","passCompMedian"))]
# projections[,c("name",paste("passIncomp", sourcesOfProjectionsAbbreviation, sep="_"), c("passIncomp","passIncompMedian"))]
# projections[,c("name",paste("passYds", sourcesOfProjectionsAbbreviation, sep="_"), c("passYds","passYdsMedian"))]
# projections[,c("name",paste("passTds", sourcesOfProjectionsAbbreviation, sep="_"), c("passTds","passTdsMedian"))]
# projections[,c("name",paste("passInt", sourcesOfProjectionsAbbreviation, sep="_"), c("passInt","passIntMedian"))]
# projections[,c("name",paste("rushAtt", sourcesOfProjectionsAbbreviation, sep="_"), c("rushAtt","rushAttMedian"))]
# projections[,c("name",paste("rushYds", sourcesOfProjectionsAbbreviation, sep="_"), c("rushYds","rushYdsMedian"))]
# projections[,c("name",paste("rushTds", sourcesOfProjectionsAbbreviation, sep="_"), c("rushTds","rushTdsMedian"))]
# projections[,c("name",paste("rec", sourcesOfProjectionsAbbreviation, sep="_"), c("rec","recMedian"))]
# projections[,c("name",paste("recYds", sourcesOfProjectionsAbbreviation, sep="_"), c("recYds","recYdsMedian"))]
# projections[,c("name",paste("recTds", sourcesOfProjectionsAbbreviation, sep="_"), c("recTds","recTdsMedian"))]
# projections[,c("name",paste("returnTds", sourcesOfProjectionsAbbreviation, sep="_"), c("returnTds","returnTdsMedian"))]
# projections[,c("name",paste("twoPts", sourcesOfProjectionsAbbreviation, sep="_"), c("twoPts","twoPtsMedian"))]
# projections[,c("name",paste("fumbles", sourcesOfProjectionsAbbreviation, sep="_"), c("fumbles","fumblesMedian"))]

#Calculate projected points for your league (average projections)
projections$passAttPts <- projections$passAtt * passAttMultiplier
projections$passCompPts <- projections$passComp * passCompMultiplier
projections$passIncompPts <- projections$passIncomp * passIncompMultiplier
projections$passYdsPts <- projections$passYds * passYdsMultiplier
projections$passTdsPts <- projections$passTds * passTdsMultiplier
projections$passIntPts <- projections$passInt * passIntMultiplier
projections$rushAttPts <- projections$rushAtt * rushAttMultiplier
projections$rushYdsPts <- projections$rushYds * rushYdsMultiplier
projections$rushTdsPts <- projections$rushTds * rushTdsMultiplier
projections$recPts <- projections$rec * recMultiplier
projections$recYdsPts <- projections$recYds * recYdsMultiplier
projections$recTdsPts <- projections$recTds * recTdsMultiplier
projections$returnTdsPts <- projections$returnTds * returnTdsMultiplier
projections$twoPtsPts <- projections$twoPts * twoPtsMultiplier
projections$fumblesPts <- projections$fumbles * fumlMultiplier

projections$passAttMedianPts <- projections$passAttMedian * passAttMultiplier
projections$passCompMedianPts <- projections$passCompMedian * passCompMultiplier
projections$passIncompMedianPts <- projections$passIncompMedian * passIncompMultiplier
projections$passYdsMedianPts <- projections$passYdsMedian * passYdsMultiplier
projections$passTdsMedianPts <- projections$passTdsMedian * passTdsMultiplier
projections$passIntMedianPts <- projections$passIntMedian * passIntMultiplier
projections$rushAttMedianPts <- projections$rushAttMedian * rushAttMultiplier
projections$rushYdsMedianPts <- projections$rushYdsMedian * rushYdsMultiplier
projections$rushTdsMedianPts <- projections$rushTdsMedian * rushTdsMultiplier
projections$recMedianPts <- projections$recMedian * recMultiplier
projections$recYdsMedianPts <- projections$recYdsMedian * recYdsMultiplier
projections$recTdsMedianPts <- projections$recTdsMedian * recTdsMultiplier
projections$returnTdsMedianPts <- projections$returnTdsMedian * returnTdsMultiplier
projections$twoPtsMedianPts <- projections$twoPtsMedian * twoPtsMultiplier
projections$fumblesMedianPts <- projections$fumblesMedian * fumlMultiplier

#Check projections
# projections[,c("name",paste("passAttPts", sourcesOfProjectionsAbbreviation, sep="_"), c("passAttPts","passAttMedianPts"))]
# projections[,c("name",paste("passCompPts", sourcesOfProjectionsAbbreviation, sep="_"), c("passCompPts","passCompMedianPts"))]
# projections[,c("name",paste("passIncompPts", sourcesOfProjectionsAbbreviation, sep="_"), c("passIncompPts","passIncompMedianPts"))]
# projections[,c("name",paste("passYdsPts", sourcesOfProjectionsAbbreviation, sep="_"), c("passYdsPts","passYdsMedianPts"))]
# projections[,c("name",paste("passTdsPts", sourcesOfProjectionsAbbreviation, sep="_"), c("passTdsPts","passTdsMedianPts"))]
# projections[,c("name",paste("passIntPts", sourcesOfProjectionsAbbreviation, sep="_"), c("passIntPts","passIntMedianPts"))]
# projections[,c("name",paste("rushAttPts", sourcesOfProjectionsAbbreviation, sep="_"), c("rushAttPts","rushAttMedianPts"))]
# projections[,c("name",paste("rushYdsPts", sourcesOfProjectionsAbbreviation, sep="_"), c("rushYdsPts","rushYdsMedianPts"))]
# projections[,c("name",paste("rushTdsPts", sourcesOfProjectionsAbbreviation, sep="_"), c("rushTdsPts","rushYdsMedianPts"))]
# projections[,c("name",paste("recPts", sourcesOfProjectionsAbbreviation, sep="_"), c("recPts","recMedianPts"))]
# projections[,c("name",paste("recYdsPts", sourcesOfProjectionsAbbreviation, sep="_"), c("recYdsPts","recYdsMedianPts"))]
# projections[,c("name",paste("recTdsPts", sourcesOfProjectionsAbbreviation, sep="_"), c("recTdsPts","recTdsMedianPts"))]
# projections[,c("name",paste("returnTdsPts", sourcesOfProjectionsAbbreviation, sep="_"), c("returnTdsPts","returnTdsMedianPts"))]
# projections[,c("name",paste("twoPtsPts", sourcesOfProjectionsAbbreviation, sep="_"), c("twoPtsPts","twoPtsMedianPts"))]
# projections[,c("name",paste("fumblesPts", sourcesOfProjectionsAbbreviation, sep="_"), c("fumblesPts","fumblesMedianPts"))]
# 
projections$projectedPtsMean <- mySum(projections[,c("passAttPts","passCompPts","passIncompPts","passYdsPts","passTdsPts","passIntPts","rushAttPts","rushYdsPts","rushTdsPts","recPts","recYdsPts","recTdsPts","returnTdsPts","twoPtsPts","fumblesPts")])
projections$projectedPtsMedian <- mySum(projections[,c("passAttMedianPts","passCompMedianPts","passIncompMedianPts","passYdsMedianPts","passTdsMedianPts","passIntMedianPts","rushAttMedianPts","rushYdsMedianPts","rushTdsMedianPts","recMedianPts","recYdsMedianPts","recTdsMedianPts","returnTdsMedianPts","twoPtsMedianPts","fumblesMedianPts")])

#Check projections
# projections[,c("name",paste("projectedPts", sourcesOfProjectionsAbbreviation, sep="_"), c("projectedPtsMean","projectedPtsMedian"))]
# projections[,c("name","projectedPtsMean","projectedPtsMedian")]
# 
projectionVars <- projections[,c(paste("projectedPts", sourcesOfProjectionsAbbreviation, sep="_"), c("projectedPtsMean","projectedPtsMedian"))]

#Convert Zeros to NA
for(i in 1:length(sourcesOfProjectionsAbbreviation)){
  projections[,paste("projectedPts", sourcesOfProjectionsAbbreviation[i], sep="_")][which(projections[,paste("projectedPts", sourcesOfProjectionsAbbreviation[i], sep="_")] == 0)] <- NA
=======
#Convert NAs to Zeroes
for(col in availableVars){
  projections[is.na(get(col)) & (sourceName == "average" | sourceName == "averageRobust" | sourceName == "averageWeighted"), (col) := 0]
>>>>>>> upstream/master
}

#Calculate projections for each source
projections[,passAttPts := passAtt * passAttMultiplier]
projections[,passCompPts := passComp * passCompMultiplier]
projections[,passIncompPts := passIncomp * passIncompMultiplier]
projections[,passYdsPts := passYds * passYdsMultiplier]
projections[,passTdsPts := passTds * passYdsMultiplier]
projections[,passIntPts := passInt * passIntMultiplier]
projections[,rushAttPts := rushAtt * rushAttMultiplier]
projections[,rushYdsPts := rushYds * rushYdsMultiplier]
projections[,rushTdsPts := rushTds * rushTdsMultiplier]
projections[,recPts := rec * recMultiplier]
projections[,recYdsPts := recYds * recYdsMultiplier]
projections[,recTdsPts := recTds * recTdsMultiplier]
projections[,returnTdsPts := returnTds * returnTdsMultiplier]
projections[,twoPtsPts := twoPts * twoPtsMultiplier]
projections[,fumblesPts := fumbles * fumlMultiplier]

scoreCategoriesPoints <- names(projections)[names(projections) %in% paste0(scoreCategories, "Pts")]
projections[,points := mySum(projections[,scoreCategoriesPoints, with=FALSE])]

#Calculate 95% CI around robust average
projections[-which(sourceName %in% c("average","averageRobust","averageWeighted")), pointsLo := tryCatch(wilcox.test(points, conf.int=TRUE, na.action="na.exclude")$conf.int[1], error=function(e) median(points, na.rm=TRUE)), by=c("name","player","pos","team","playerID")]
projections[-which(sourceName %in% c("average","averageRobust","averageWeighted")), pointsHi := tryCatch(wilcox.test(points, conf.int=TRUE, na.action="na.exclude")$conf.int[2], error=function(e) median(points, na.rm=TRUE)), by=c("name","player","pos","team","playerID")]

projections[,pointsLo := mean(pointsLo, na.rm=TRUE), by=c("name","player","pos","team","playerID")]
projections[,pointsHi := mean(pointsHi, na.rm=TRUE), by=c("name","player","pos","team","playerID")]

#Describe
projections[,list(n = length(points),
                  min = min(points),
                  median = median(points),
                  mean = mean(points),
                  max = max(points))
            , by="sourceName"]

#Correlations among projections
projectionsWide <- dcast.data.table(projections, name + pos + team + playerID ~ sourceName, value.var="points", fun.aggregate = mean)
cor(projectionsWide[,c(unique(projections$sourceName)), with=FALSE], use="pairwise.complete.obs")

#Calculate ranks
projections <- projections[order(-points)][,overallRank := 1:.N, by=list(sourceName)]
projections <- projections[order(-points)][,positionRank := 1:.N, by=list(sourceName, pos)]

#Add season
projections[,season := season]

#Select and order variables
keepVars <- finalVarNames[finalVarNames %in% names(projections)]
projections <- projections[,keepVars, with=FALSE]

<<<<<<< HEAD
#View projections
#projections
#projections[,c("name","pos","team","projectedPts_fp","projectedPtsMean","projectedPtsMedian")]

#Density Plot
pointDensity <- c(projections$projectedPts_accu, projections$projectedPts_cbs1, projections$projectedPts_cbs2, projections$projectedPts_espn, projections$projectedPts_nfl, projections$projectedPts_fs, projections$projectedPts_fp, projections$projectedPts_fftoday, projections$projectedPts_fbg1, projections$projectedPts_fbg2, projections$projectedPts_fbg3, projections$projectedPts_fbg4, projections$projectedPts_fox, projections$projections) #,projections$projectedPts_accu, projections$projectedPts_cbs, projections$projectedPts_yahoo, projections$projectedPtsLatent, projections$projectedPtsMean
sourceDensity <- c(rep("Accuscore",dim(projections)[1]), rep("CBS1",dim(projections)[1]), rep("CBS2",dim(projections)[1]), rep("ESPN",dim(projections)[1]), rep("NFL.com",dim(projections)[1]), rep("FantasySharks",dim(projections)[1]), rep("FantasyPros",dim(projections)[1]), rep("FFtoday",dim(projections)[1]), rep("Footballguys1",dim(projections)[1]), rep("Footballguys2",dim(projections)[1]), rep("Footballguys3",dim(projections)[1]), rep("Footballguys4",dim(projections)[1]), rep("FOX",dim(projections)[1]), rep(league, dim(projections)[1])) #,rep("Accuscore",dim(projections)[1]), rep("CBS",dim(projections)[1]), rep("Yahoo",dim(projections)[1]), rep("Latent",dim(projections)[1]), rep("Average",dim(projections)[1])
=======
#Set Key
setkeyv(projections, cols=c("name","pos","team","sourceName"))

#Density Plot
pointDensity <- projections$points[order(projections$sourceName)]
sourceDensity <- projections$sourceName[order(projections$sourceName)]
>>>>>>> upstream/master
densityData <- data.frame(pointDensity, sourceDensity)

ggplot(densityData, aes(x=pointDensity, fill=sourceDensity)) + geom_density(alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of Projected Points") + theme(legend.title=element_blank())
ggsave(paste(getwd(),"/Figures/Calculate projections.jpg", sep=""), width=10, height=10)
dev.off()

#Save file
<<<<<<< HEAD
save(projections, file = paste(getwd(),"/Data/LeagueProjections_", league, ".RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/LeagueProjections_", league, ".csv", sep=""), row.names=FALSE)

save(projections, file = paste(getwd(),"/Data/Historical Projections/LeagueProjections_", league, "-2014.RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/Historical Projections/LeagueProjections_", league, "-2014.csv", sep=""), row.names=FALSE)

=======
save(projections, file = paste0(getwd(), "/Data/LeagueProjections.RData"))
write.csv(projections, file=paste0(getwd(), "/Data/LeagueProjections.csv"), row.names=FALSE)

save(projections, file = paste0(getwd(), "/Data/Historical Projections/LeagueProjections-", season, ".RData"))
write.csv(projections, file=paste0(getwd(), "/Data/Historical Projections/LeagueProjections-", season, ".csv"), row.names=FALSE)
>>>>>>> upstream/master
