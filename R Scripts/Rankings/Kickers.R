###########################
# File: Kickers.R
# Description: Kicker rankings
# Date: 6/9/2013
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# To do:
###########################

#Load libraries
library("XML")
library("stringr")

#Functions
source(paste(getwd(), "/R Scripts/Functions/Global Settings.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings_", league, ".R", sep=""))

#Risk - "Experts"
kickers <- readHTMLTable("http://www.fantasypros.com/nfl/rankings/k-cheatsheets.php", stringsAsFactors = FALSE)$data

name1 <- str_sub(kickers[,c("Player (team/bye)")], end=str_locate(kickers[,c("Player (team/bye)")], '\\(')[,1]-2)
name2 <- str_sub(kickers[,c("Player (team/bye)")], end=str_locate(kickers[,c("Player (team/bye)")], '\\)')[,1]-1)

name1[is.na(name1)] <- name2[is.na(name1)]

kickers$player <- name1
kickers$name <- nameMerge(kickers$player)
kickers$team <- str_sub(kickers[,c("Player (team/bye)")], start=str_locate(kickers[,c("Player (team/bye)")], '\\(')[,1]+1, end=str_locate(kickers[,c("Player (team/bye)")], '\\/')[,1]-1)

kickers$rank <- as.numeric(kickers[,"Ave"])
kickers$risk <- as.numeric(kickers[,"Std Dev"])

#Subset columns
kickers <- kickers[,c("name","player","team","rank","risk")]

#Remove rows with all NAs
kickers <- kickers[rowSums(is.na(kickers)) != ncol(kickers),]

#Sort by rank
kickers <- kickers[order(kickers$rank),]

#View Rankings
kickers

#Save file
<<<<<<< HEAD
save(kickers, file = paste(getwd(),"/Data/kickers_", league, ".RData", sep=""))
write.csv(kickers, file=paste(getwd(),"/Data/kickers_", league, ".csv", sep=""), row.names=FALSE)

save(kickers, file = paste(getwd(),"/Data/Historical Rankings/kickers_", league, "-2014.RData", sep=""))
write.csv(kickers, file=paste(getwd(),"/Data/Historical Rankings/kickers_", league, "-2014.csv", sep=""), row.names=FALSE)
=======
save(kickers, file = paste(getwd(), "/Data/kickers.RData", sep=""))
write.csv(kickers, file=paste(getwd(), "/Data/kickers.csv", sep=""), row.names=FALSE)

save(kickers, file = paste(getwd(), "/Data/Historical Rankings/kickers-", season, ".RData", sep=""))
write.csv(kickers, file=paste(getwd(), "/Data/Historical Rankings/kickers-", season, ".csv", sep=""), row.names=FALSE)
>>>>>>> upstream/master
