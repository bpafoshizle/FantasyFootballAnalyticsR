###########################
# File: League Settings.R
# Description: User sets league settings
# Date: 6/1/2013
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# To do:
###########################

numTeams = 12
numRounds = 15
leagueFormat = "standard" # ppr or standard - plays into Wisdom of the Crowd

#Roster
numQBstarters <- 1
numRBstarters <- 2
numWRstarters <- 3
numTEstarters <- 1
numTotalStarters <- 8
numTotalPlayers <- 15

#League settings
defaultCap <- 200           #what the typical cap is for your service (ESPN, Yahoo, etc.) -- used for placing "avg cost" in context
leagueCap <- 225            #your league's cap
maxCost <- leagueCap - (numTotalPlayers - numTotalStarters)

#Variable names
prefix <- c("name","pos")
varNames <- c("name","team","positionRank","overallRank","pts",
              "passAtt","passComp","passYds","passTds","passInt",
              "rushAtt","rushYds","rushTds","rec","recYds","recTds",
              "returnTds","twoPts","fumbles") 
#"solo","ast","idpSack","idpFumlRec","idpFumlForce","idpInt","idpPD",
#"ptsAllowed","dstSack","dstInt","dstFumlRec","blk","to","intTd","kRetTd","pRetTd",
#"fg","fg3039","fg4049","fg50","xp"

#Scoring
passAttMultiplier <- 0      #0 pts per passing attempt
passCompMultiplier <- 0     #0 pts per passing completion
passIncompMultiplier <- 0     #0 pts per passing incompletion
passYdsMultiplier <- (1/25) #1 pt per 25 passing yds
passTdsMultiplier <- 4      #4 pts per passing td
passIntMultiplier <- -2     #-3 pts per passing interception
rushAttMultiplier <- 0      #0 pts per rushing attempt
rushYdsMultiplier <- (1/10) #1 pt per 10 rushing yds
rushTdsMultiplier <- 6      #6 pts per rushing touchdown
recMultiplier <- 0          #0 pts per reception
recYdsMultiplier <- (1/10)   #1 pt per 10 receiving yds
recTdsMultiplier <- 6       #6 pts per receiving touchdown
returnTdsMultiplier <- 6    #6 pts per return touchdown
twoPtsMultiplier <- 2       #2 pts per 2-point conversion
fumlMultiplier <- -2        #-3 pts per fumble lost

#Projections
sourcesOfProjections <- c("Accuscore", "CBS1", "CBS2", "ESPN", "FantasyFootballNerd", "FantasyPros", "FantasySharks", "FFtoday", "Footballguys1", "Footballguys2", "Footballguys3", "Footballguys4", "FOX", "NFL", "WalterFootball", "Yahoo")
sourcesOfProjectionsAbbreviation <- c("accu", "cbs1", "cbs2", "espn", "ffn", "fp", "fs", "fftoday", "fbg1", "fbg2", "fbg3", "fbg4", "fox", "nfl", "wf", "yahoo")

#Number of players at each position drafted in Top 100 (adjust for your league)
# qbReplacements <- 15
# rbReplacements <- 37
# wrReplacements <- 36
# teReplacements <- 11

# Calculate the replacement position based on method from "Fantasy Football for Smart People"
# Uses S(N) where is is number of starters at the position and N is number of managers/teams
qbReplacements <- numQBstarters*numTeams
rbReplacements <- numRBstarters*numTeams
wrReplacements <- numWRstarters*numTeams
teReplacements <- numTEstarters*numTeams


#Alternative way of calculating the number of players at each position drafted in Top 100 based on league settings
#numTeams <- 10  #number of teams in league
#numQB <- 1      #number of avg QBs in starting lineup
#numRB <- 2.5    #number of avg RBs in starting lineup
#numWR <- 2.5    #number of avg WRs in starting lineup
#numTE <- 1      #number of avg TEs in starting lineup

#qbReplacements <- print(ceiling(numQB*numTeams*1.7))
#rbReplacements <- print(ceiling(numRB*numTeams*1.4))
#wrReplacements <- print(ceiling(numWR*numTeams*1.4))
#teReplacements <- print(ceiling(numTE*numTeams*1.3))