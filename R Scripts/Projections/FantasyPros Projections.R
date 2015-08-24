###########################
# File: FantasyPros Projections.R
# Description: Downloads Fantasy Football Projections from FantasyPros.com
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

#Functions
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings.R", sep=""))

#Suffix
suffix <- "fp"

#Download fantasy football projections from FantasyPros.com
qb_fp <- readHTMLTable("http://www.fantasypros.com/nfl/projections/qb.php", stringsAsFactors = FALSE)$data
rb_fp <- readHTMLTable("http://www.fantasypros.com/nfl/projections/rb.php", stringsAsFactors = FALSE)$data
wr_fp <- readHTMLTable("http://www.fantasypros.com/nfl/projections/wr.php", stringsAsFactors = FALSE)$data
te_fp <- readHTMLTable("http://www.fantasypros.com/nfl/projections/te.php", stringsAsFactors = FALSE)$data
k_fp <- readHTMLTable("http://www.fantasypros.com/nfl/projections/k.php", stringsAsFactors = FALSE)$data

#Add variable names for each object
names(qb_fp) <- c("player","passAtt","passComp","passYds","passTds","passInt","rushAtt","rushYds","rushTds","fumbles","points")
names(rb_fp) <- c("player","rushAtt","rushYds","rushTds","rec","recYds","recTds","fumbles","points")
names(wr_fp) <- c("player","rushAtt","rushYds","rushTds","rec","recYds","recTds","fumbles","points")
names(te_fp) <- c("player","rec","recYds","recTds","fumbles","points")
names(k_fp) <- c("player","fg","fga","xp","points")

#Add variable for player position
qb_fp$pos <- as.factor("QB")
rb_fp$pos <- as.factor("RB")
wr_fp$pos <- as.factor("WR")
te_fp$pos <- as.factor("TE")
k_fp$pos <- as.factor("K")

#Merge players across positions
projections_fp <- data.table(rbind.fill(qb_fp, rb_fp, wr_fp, te_fp, k_fp))

#Add variables from other projection sources
projections_fp$returnTds <- NA
projections_fp$twoPts <- NA

#Remove special characters(commas)
projections_fp <- projections_fp[,lapply(.SD, function(x) gsub("\\,", "", x))]


#Convert variables from character strings to numeric
numericVars <- names(projections_fp)[names(projections_fp) %in% c(scoreCategories, "dstDTd","dstRetTd")]
projections_fp[, (numericVars) := lapply(.SD, function(x) as.numeric(as.character(x))), .SDcols = numericVars]

#Player names
projections_fp[, name_fp := str_trim(str_sub(player
                                             ,start=1
                                             ,end=str_length(player)-3))]


#Name for merging
projections_fp$name <- nameMerge(projections_fp$name_fp)

#Player teams
projections_fp[, team_fp := str_trim(str_sub(player
                                             ,start=str_length(player)-2
                                             ,end=str_length(player)))]

#Remove rows with all NAs
projections_fp <- projections_fp[apply(projections_fp, 1, function(x) any(!is.na(x))),]

#Remove rows with missing player name
if(length(which(projections_fp$name_fp == "")) > 0){
  projections_fp <- projections_fp[-which(projections_fp$name_fp == ""),]
}

#Remove duplicate cases
projections_fp[projections_fp$name %in% projections_fp[duplicated(projections_fp$name),"name"],]

#Same name, different player
projections_fp <- projections_fp[!(projections_fp$name=="ALEXSMITH" & projections_fp$team_fp=="CIN"),]
projections_fp <- projections_fp[!(projections_fp$name=="RYANGRIFFIN" & projections_fp$team_fp=="HOU"),]

#Add source
projections_fp$sourceName <- suffix

#Remove duplicate cases
duplicateCases <- duplicated(projections_fp$name)
projections_fp <- projections_fp[!duplicateCases,]

#Rename Players
if(length(projections_fp[projections_fp$name == "CHRISTOPHERIVORY", "name"]) > 0){projections_fp[projections_fp$name == "CHRISTOPHERIVORY", "name"] <- "CHRISIVORY"}
if(length(projections_fp[projections_fp$name == "DOMANIQUEDAVIS", "name"]) > 0){projections_fp[projections_fp$name == "DOMANIQUEDAVIS", "name"] <- "DOMINIQUEDAVIS"}

#Calculate Overall Rank
projections_fp <- projections_fp[order(-points)][,overallRank := 1:.N]

#Calculate Position Rank
projections_fp <- projections_fp[order(-points)][,positionRank := 1:.N, by=list(pos)]

#Order variables in data set
allVars <- c(prefix, paste(sourceSpecific, suffix, sep="_"), varNames)
keepVars <- allVars[allVars %in% names(projections_fp)]
projections_fp <- projections_fp[,keepVars, with=FALSE]


#Order players by overall rank
projections_fp <- projections_fp[order(projections_fp$overallRank),]

#Density Plot
ggplot(projections_fp, aes(x=points)) + geom_density(fill="orange", alpha=.3) + xlab("Player's Projected Points") + ggtitle("Density Plot of FantasyPros Projected Points")
ggsave(paste(getwd(),"/Figures/FantasyPros projections.jpg", sep=""), width=10, height=10)
dev.off()

#Save file
save(projections_fp, file = paste(getwd(), "/Data/FantasyPros-Projections.RData", sep=""))
write.csv(projections_fp, file=paste(getwd(), "/Data/FantasyPros-Projections.csv", sep=""), row.names=FALSE)

save(projections_fp, file = paste(getwd(), "/Data/Historical Projections/FantasyPros-Projections-", season, ".RData", sep=""))
write.csv(projections_fp, file=paste(getwd(), "/Data/Historical Projections/FantasyPros-Projections-", season, ".csv", sep=""), row.names=FALSE)