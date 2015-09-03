###########################
# File: League Settings.R
# Description: User sets league settings
# Date: 6/1/2013
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# To do:
###########################

source("~/github/local/FantasyFootballAnalyticsR/R Scripts/Functions/Global Settings.R")

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
prefix <- c("name","pos","sourceName")
sourceSpecific <- c("name","team")
scoreCategories <- c("passAtt","passComp","passIncomp","passYds","passTds","passInt",
                     "rushAtt","rushYds","rushTds",
                     "rec","recTgt","recYds","recTds",
                     "returnTds","twoPts","fumbles",
                     "idpSolo","idpAst","idpSack","idpFumlRec","idpFumlForce","idpInt","idpPD",
                     "dstPtsAllow","dstYdsAllowed","dstSack","dstSafety","dstInt","dstFumlRec","dstFumlForce","dstBlk","dstTd",
                     "fg","fgAtt","fg0019","fg2029","fg3039","fg4049","fg50","xp", "points")
calculatedVars <- c("positionRank","overallRank","calcPoints","pointsLo","pointsHi","vor","pick","risk","sdPts","sdPick", "dropOffNorm")
varNames <- c(calculatedVars, scoreCategories)
finalVarNames <- c("name","pos","team","sourceName","player","playerID","season", "playerId", "analystId", varNames)

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

scoringRules <- list(
    QB = data.frame(dataCol = c("passYds", "passTds", "passInt", "rushYds", "rushTds", "twoPts", "fumbles"),
                    multiplier = c(passYdsMultiplier, passTdsMultiplier, passIntMultiplier, rushYdsMultiplier, 
                                   rushTdsMultiplier, twoPtsMultiplier,  fumlMultiplier)),
    RB = data.frame(dataCol = c("rushYds", "rushTds", "rec", "recYds", "recTds", "returnTds", "twoPts", "fumbles"),
                    multiplier = c(rushYdsMultiplier, rushTdsMultiplier, recMultiplier, recYdsMultiplier, 
                                   recTdsMultiplier, returnTdsMultiplier, twoPtsMultiplier, -fumlMultiplier)), 
    WR = data.frame(dataCol = c("rushYds", "rushTds", "rec", "recYds", "recTds", "returnTds", "twoPts", "fumbles"),
                    multiplier = c(rushYdsMultiplier, rushTdsMultiplier, recMultiplier, recYdsMultiplier, 
                                   recTdsMultiplier, returnTdsMultiplier, twoPtsMultiplier, -fumlMultiplier)),
    TE = data.frame(dataCol = c("rushYds", "rushTds", "rec", "recYds", "recTds", "returnTds", "twoPts", "fumbles"),
                    multiplier = c(rushYdsMultiplier, rushTdsMultiplier, recMultiplier, recYdsMultiplier, 
                                   recTdsMultiplier, returnTdsMultiplier, twoPtsMultiplier, -fumlMultiplier)),
    K = data.frame(dataCol = c("xp", "fg", "fg0019", "fg2029", "fg3039", "fg4049", "fg50"),
                   multiplier = c(1, 3, 3, 3, 3, 4, 5)),
    DST = data.frame(dataCol = c("dstFumlRec", "dstInt", "dstSafety", "dstSack", "dstTd", "dstBlk"),
                     multiplier = c(2, 2, 2, 1, 6, 1.5)),
    ptsBracket = data.frame(threshold = c(0, 6, 20, 34, 99),
                             points = c(10, 7, 4, 0, -4))
  )


#Projections
sourcesOfProjections <- c("CBS", "ESPN", "FantasyPros",  "FOX", "NFL", "Yahoo")
sourcesOfProjectionsAbbreviation <- c("cbs", "espn", "fp", "fox", "nfl", "yahoo")

#Weights applied to each source in calculation of weighted average of projections
weight_cbs <- 1    #Jamey Eisenberg
weight_espn <- 1    #ESPN
weight_fox <- 1    #FOX
weight_fp <- 1      #FantasyPros
weight_nfl <- 1     #NFL.com
weight_yahoo <- 1   #Yahoo 

sourceWeights <- c(
  "Jamey Eisenberg"   = 1, 
  "Dave Richard"      = 1, 
  "Yahoo Sports"      = 1, 
  "ESPN"              = 1, 
  "NFL"               = 1, 
  "FOX Sports"        = 1, 
  "FFtoday"           = 1,
  "NumberFire"        = 1, 
  "FantasyPros"       = 1,
  "Dodds-Norton"      = 1, 
  "Dodds"             = 1, 
  "Tremblay"          = 1, 
  "Herman"            = 1, 
  "Henry"             = 1, 
  "Wood"              = 1, 
  "Bloom"             = 1
  ) 

# Calculate the replacement position based on method from "Fantasy Football for Smart People"
# Uses S(N) where is is number of starters at the position and N is number of managers/teams
qbReplacements <- numQBstarters*numTeams
rbReplacements <- numRBstarters*numTeams
wrReplacements <- numWRstarters*numTeams
teReplacements <- numTEstarters*numTeams