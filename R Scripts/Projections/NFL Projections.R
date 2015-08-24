###########################
# File: NFL Projections.R
# Description: Downloads Fantasy Football Projections from NFL.com
# Date: 3/3/2013
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# To do:
###########################

#Load libraries
library("XML")
library("stringr")
library("ggplot2")
library("plyr")
library("data.table")

#Suffix
suffix <- "nfl"

#Functions
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings.R", sep=""))


nflPos <- list(QB="1", RB="2", WR="3", TE="4")
nflPages <- paste0("&offset=", seq(1, 150, by=25))
seasonVar <- paste0("&statSeason=", season)
nflBaseUrl <- paste0("http://fantasy.nfl.com/research/projections?sort=projectedPts&statCategory=projectedStats&statType=seasonProjectedStats"
                     ,seasonVar)
nflUrls <- paste0(nflBaseUrl, nflPages, "&position=", rep(nflPos, each=length(nflPages)))

nfl <- lapply(nflUrls, function(x) {data.table(readHTMLTable(x, stringsAsFactors = FALSE)$'NULL')})
nflList <- nfl

#Clean data
qbNames <- rbNames <- wrNames <- teNames <- c("player","opp","gp","passYds","passTds","passInt","rushYds","rushTds","recYds","recTds","fumbleTds","twoPts","fumbles","points")


#Clean data
for(i in 1:length(nflList)){
   if(nrow(nflList[[i]]) > 0){
      #Add position to projection
      nflList[[i]][,pos := rep(names(nflPos), each=length(nflPages))[i]]
      nflList[[i]][,pos := as.factor(pos)]
      
      #Add variable names
      if(unique(nflList[[i]][,pos]) == "QB"){
         setnames(nflList[[i]], c(qbNames, "pos"))
      } else if(unique(nflList[[i]][,pos]) == "RB"){
         setnames(nflList[[i]], c(rbNames, "pos"))
      } else if(unique(nflList[[i]][,pos]) == "WR"){
         setnames(nflList[[i]], c(wrNames, "pos"))
      } else if(unique(nflList[[i]][,pos]) == "TE"){
         setnames(nflList[[i]], c(teNames, "pos"))
      } 
   }
}

projections_nfl <- rbindlist(nflList, use.names=TRUE, fill=TRUE)


#Add missing variables
projections_nfl$passAtt <- NA
projections_nfl$passComp <- NA
projections_nfl$rushAtt <- NA
projections_nfl$rec <- NA
projections_nfl$returnTds <- NA

#Convert variables from character strings to numeric
numericVars <- names(projections_nfl)[names(projections_nfl) %in% c(scoreCategories, "dstDTd","dstRetTd")]
projections_nfl[, (numericVars) := lapply(.SD, function(x) as.numeric(gsub("-","",as.character(x)))), .SDcols = numericVars]

#Player names
qbnames <- str_sub(projections_nfl$player, end=str_locate(string=projections_nfl$player, c("QB"))[,1]-2) #"QB -"
rbnames <- str_sub(projections_nfl$player, end=str_locate(string=projections_nfl$player, c("RB"))[,1]-2) #"RB -"
wrnames <- str_sub(projections_nfl$player, end=str_locate(string=projections_nfl$player, c("WR"))[,1]-2) #"WR -"
tenames <- str_sub(projections_nfl$player, end=str_locate(string=projections_nfl$player, c("TE"))[,1]-2) #"TE -"

qbnames <- qbnames[1:length(projections_nfl[which(projections_nfl$pos == "QB"),pos])]
rbnames <- rbnames[(length(projections_nfl[which(projections_nfl$pos == "QB"),pos]) + 1):(length(projections_nfl[which(projections_nfl$pos == "QB"),pos]) + length(projections_nfl[which(projections_nfl$pos == "RB"),pos]))]
wrnames <- wrnames[(length(projections_nfl[which(projections_nfl$pos == "QB"),pos]) + length(projections_nfl[which(projections_nfl$pos == "RB"),pos]) + 1):(length(projections_nfl[which(projections_nfl$pos == "QB"),pos]) + length(projections_nfl[which(projections_nfl$pos == "RB"),pos]) + length(projections_nfl[which(projections_nfl$pos == "WR"),pos]))]
tenames <- tenames[(length(projections_nfl[which(projections_nfl$pos == "QB"),pos]) + length(projections_nfl[which(projections_nfl$pos == "RB"),pos]) + length(projections_nfl[which(projections_nfl$pos == "WR"),pos]) + 1):(length(projections_nfl[which(projections_nfl$pos == "QB"),pos]) + length(projections_nfl[which(projections_nfl$pos == "RB"),pos]) + length(projections_nfl[which(projections_nfl$pos == "WR"),pos]) + length(projections_nfl[which(projections_nfl$pos == "TE"),pos]))]
  
projections_nfl$name_nfl <- c(na.omit(qbnames),na.omit(rbnames),na.omit(wrnames),na.omit(tenames))
projections_nfl$name <- nameMerge(projections_nfl$name_nfl)

#Player teams
projections_nfl$team_nfl <- str_trim(str_sub(projections_nfl$player, start=str_locate(string=projections_nfl$player, c(" - "))[,1]+3, end=str_locate(string=projections_nfl$player, c(" - "))[,1]+6))

#Remove duplicate cases
projections_nfl[projections_nfl$name %in% projections_nfl[duplicated(projections_nfl$name),"name"],]

#Rename players
projections_nfl[projections_nfl$name=="TIMOTHYWRIGHT", "name"] <- "TIMWRIGHT"

#Calculate Overall Rank
projections_nfl <- projections_nfl[order(-points)][,overallRank := 1:.N]

#Calculate Position Rank
projections_nfl <- projections_nfl[order(-points)][,positionRank := 1:.N, by=list(pos)]

#Add source
projections_nfl$sourceName <- suffix

#Remove duplicate cases
duplicateCases <- duplicated(projections_nfl$name)
projections_nfl <- projections_nfl[!duplicateCases,]

#Order variables in data set
allVars <- c(prefix, paste(sourceSpecific, suffix, sep="_"), varNames)
keepVars <- allVars[allVars %in% names(projections_nfl)]
projections_nfl <- projections_nfl[,keepVars, with=FALSE]

#Order players by overall rank
projections_nfl <- projections_nfl[order(projections_nfl$overallRank),]

#Density Plot
ggplot(projections_nfl, aes(x=points), fill=pos) + geom_density(fill="green", alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of NFL.com Projected Points")
ggsave(paste(getwd(),"/Figures/NFL projections.jpg", sep=""), width=10, height=10)
dev.off()

#Save file
save(projections_nfl, file = paste(getwd(), "/Data/NFL-Projections.RData", sep=""))
write.csv(projections_nfl, file=paste(getwd(), "/Data/NFL-Projections.csv", sep=""), row.names=FALSE)

save(projections_nfl, file = paste(getwd(), "/Data/Historical Projections/NFL-Projections-", season, ".RData", sep=""))
write.csv(projections_nfl, file=paste(getwd(), "/Data/Historical Projections/NFL-Projections-", season, ".csv", sep=""), row.names=FALSE)
