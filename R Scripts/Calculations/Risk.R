###########################
# File: Risk.R
# Description: Calculates players' risk level
# Date: 3/3/2013
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# -ESPN projections do not include fumbles!
# To do:
# -Evaluate accuracy of projections while taking into account risk
# -Add FantasyPros to sdPts calculation
###########################

#Load libraries
library("XML")
library("stringr")
library("data.table")
library("stringr")

#Functions
source(paste(getwd(), "/R Scripts/Functions/Global Settings.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings", ".R", sep=""))

#Load data
load(paste(getwd(),"/Data/LeagueProjections_", league, ".RData", sep=""))
load(paste(getwd(),"/Data/wisdomOfTheCrowd_", league, ".RData", sep=""))

#Risk - "Experts"
experts <- data.table(readHTMLTable("http://www.fantasypros.com/nfl/rankings/consensus-cheatsheets.php", stringsAsFactors = FALSE)$data)
experts[,sdPick_experts := as.numeric(get("Std Dev"))]
experts[,pick_experts := as.numeric(Avg)]
experts[,player := getFpPlayerString(get("Player (team, bye)"))]
experts$name <- nameMerge(experts$player)

#Rename Players
experts <- experts[,c("name","pick_experts","sdPick_experts"), with=FALSE]

#Risk - Wisdom of the Crowd
wisdomOfTheCrowd[,pick_crowd := mean]
wisdomOfTheCrowd[,sdPick_crowd := mad]
wisdomOfTheCrowd <- wisdomOfTheCrowd[freq >= .06,c("name","pick_crowd","sdPick_crowd", "freq"), with=FALSE]

#Merge files
risk <- merge(experts, wisdomOfTheCrowd, by="name", all=TRUE)
projections <- merge(projections, risk, by="name", all.x=TRUE)

#Calculate risk
projections[,pick := rowMeans(projections[,c("pick_experts", "pick_crowd"), with=FALSE], na.rm=TRUE)]

projections[-which(sourceName %in% c("average","averageRobust","averageWeighted")), sdPts := mad(points, na.rm=TRUE), by=c("name","player","pos","team","playerID")]
projections[,sdPts := mean(sdPts, na.rm=TRUE), by=c("name","player","pos","team","playerID")]

projections[,sdPick := rowMeans(projections[,c("sdPick_experts","sdPick_crowd"), with=FALSE], na.rm=TRUE)]
projections$sdPts[which(projections$sdPts == 0)] <- NA

#Standardize risk by position
projections[,sdPickZ := scale(sdPick), by="pos"]
projections[,sdPtsZ := scale(sdPts), by="pos"]
projections[,risk := rowMeans(projections[,c("sdPickZ","sdPtsZ"), with=FALSE], na.rm=TRUE)]

#Rescale risk with mean~5 and sd~2
projections[,risk := ((risk * 2/(sd(risk, na.rm=TRUE))) + (5-(mean(risk, na.rm=TRUE))))]

#Select and order variables
newVars <- c("pick","risk","sdPts","sdPick")
allVars <- c(finalVarNames, newVars)
keepVars <- finalVarNames[finalVarNames %in% names(projections)]
projections <- projections[,keepVars, with=FALSE]

#Density plot
ggplot(projections, aes(x=risk)) + geom_density(fill="red", alpha=.7) + xlab("Player's Risk Level") + ggtitle("Density Plot of Players' Risk Levels")
ggsave(paste(getwd(),"/Figures/Risk_", league, ".jpg", sep=""), width=10, height=10)
dev.off()

#Save file
save(projections, file = paste(getwd(),"/Data/Risk_", league, ".RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/Risk_", league, ".csv", sep=""), row.names=FALSE)

save(projections, file = paste(getwd(),"/Data/Historical Files/Risk_", league, "-", season, ".RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/Historical Files/Risk_", league, "-", season, ".csv", sep=""), row.names=FALSE)