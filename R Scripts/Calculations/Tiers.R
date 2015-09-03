###########################
# File: Tiers.R
# Description: Calculates tiers for position using k-means
# Date: 8/29/2015
# Author: Barret Miller
# Notes:
# To do:
#  1. select players by pos and sourceName, calculate tiers
###########################

# Libraries

#Functions
source(paste(getwd(), "/R Scripts/Functions/Global Settings.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings",".R", sep=""))

#Load data
load(paste(getwd(),"/Data/VOR_", league, ".RData", sep=""))

###### Wide Receivers #######
#load(paste(getwd(),"/Data/VOR_", league, ".RData", sep=""))
wr <- projections[sourceName == "averageRobust" & pos == "WR", 
                             .(playerID, points, pointsLo, pointsHi, pick 
                             #,rushAtt, rushYds, rushTds, 
                             #,rec, recTgt, recYds, recTds
                             #,returnTds, twoPts, fumbles
                             )]
#silScore(wr) ## 5 is optimal.
WRclust <- clusterPlayers(wr, 8)
#projections <- merge(projections, WRclust, by = c("playerID"), all=T)
#wrView <- projections[pos == "WR" & sourceName == "averageRobust"
#                      ,.(name, team, positionRank, points, tier)][order(positionRank)]


###### Running Backs #######
#load(paste(getwd(),"/Data/VOR_", league, ".RData", sep=""))
rb <- projections[sourceName == "averageRobust" & pos == "RB", 
                             .(playerID, points, pointsLo, pointsHi, pick
                             #,rushAtt, rushYds, rushTds
                             #,rec, recTgt, recYds, recTds
                             #,returnTds, twoPts, fumbles
                             )]
#silScore(rb)## 2 is optimal, 4 isn't too bad
RBclust <- clusterPlayers(rb, 8)
#projections <- merge(projections, RBclust, by = c("playerID"), all=T)
#rbView <- projections[pos == "RB" & sourceName == "averageRobust"
#                      ,.(name, team, positionRank, points, tier)][order(positionRank)]

###### Tight Ends #######
#load(paste(getwd(),"/Data/VOR_", league, ".RData", sep=""))
te <- projections[sourceName == "averageRobust" & pos == "TE", 
                  .(playerID, points, pointsLo, pointsHi, pick
                  #,rec, recTgt, recYds, recTds, twoPts, fumbles
                  )]
#silScore(te)
TEclust <- clusterPlayers(te, 8)
#projections <- merge(projections, TEclust, by = c("playerID"), all=T)
#teView <- projections[pos == "TE" & sourceName == "averageRobust"
#                      ,.(name, team, positionRank, points, tier)][order(positionRank)]


###### Quarterbacks #######
#load(paste(getwd(),"/Data/VOR_", league, ".RData", sep=""))
qb <- projections[sourceName == "averageRobust" & pos == "QB",
                  .(playerID, points, pointsLo, pointsHi, pick
                  #,passAtt, passComp, passIncomp
                  #,passYds, passTds, passInt
                  #,twoPts, fumbles
                             )]
#silScore(qb)
QBclust <- clusterPlayers(qb, 8)
#projections <- merge(projections, QBclust, by = c("playerID"), all=T)
#qbView <- projections[pos == "QB" & sourceName == "averageRobust"
#                      ,.(name, team, positionRank, points, tier)][order(positionRank)]

## Combine positions
clust <- rbind(QBclust, RBclust, WRclust, TEclust)

## Merge back into projectsions
projections <- merge(projections, clust, by = c("playerID"), all=T)


#Save file
save(projections, file = paste(getwd(),"/Data/Tier_", league, ".RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/Tier_", league, ".csv", sep=""), row.names=FALSE)

save(projections, file = paste(getwd(),"/Data/Historical Files/Tier_", league, "-", season, ".RData", sep=""))
write.csv(projections, file=paste(getwd(),"/Data/Historical Files/Tier_", league, "-", season, ".csv", sep=""), row.names=FALSE)

