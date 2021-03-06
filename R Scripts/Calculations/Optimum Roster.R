###########################
# File: Optimum Roster.R
# Description: Determines Optimum Roster to Maximize Projected Points and Minimize Risk
# Date: 3/3/2013
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# -Cost is based on Yahoo Avg Cost
# To do:
###########################

#Specify Maximum Risk
maxRisk <- 5

#Library
library("Rglpk")
library("data.table")

#Functions
source(paste(getwd(), "/R Scripts/Functions/Global Settings.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings_", league, ".R", sep=""))

#Load data
load(paste(getwd(),"/Data/AvgCost_", league, ".RData", sep=""))

#Roster Optimization
optimizeData <- na.omit(projections[sourceName == "averageRobust", c("name","player","pos","points","risk","inflatedCost"), with=FALSE]) #name,projectedPtsLatent,projectedPtsMedian

#Calculate Optimum Roster
optimizeTeam(maxRisk=maxRisk)
optimizeTeam(maxRisk=100)
