#Run fantasy football analysis files in order

###DON'T FORGET TO MANUALLY DOWNLOAD AVG COST FROM YAHOO
setwd("~/github/local/FantasyFootballAnalyticsR")
###############
# Functions
###############
source(paste(getwd(), "/R Scripts/Functions/Global Settings.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/Functions.R", sep=""))
source(paste(getwd(),"/R Scripts/Functions/League Settings_", league, ".R", sep=""))

###############
# Rankings
###############

#source(paste(getwd(),"/R Scripts/Rankings/IDP.R", sep=""), echo=TRUE)
#source(paste(getwd(),"/R Scripts/Rankings/Kickers.R", sep=""), echo=TRUE)

###############
# Projections
###############


source(paste(getwd(),"/R Scripts/Projections/CBS Projections.R", sep=""), echo=TRUE)
source(paste(getwd(),"/R Scripts/Projections/ESPN Projections.R", sep=""), echo=TRUE)
source(paste(getwd(),"/R Scripts/Projections/Yahoo Projections.R", sep=""), echo=TRUE)
source(paste(getwd(),"/R Scripts/Projections/FOX Projections.R", sep=""), echo=TRUE)
source(paste(getwd(),"/R Scripts/Projections/NFL Projections.R", sep=""), echo=TRUE)
source(paste(getwd(),"/R Scripts/Projections/FantasyPros Projections.R", sep=""), echo=TRUE)
#source(paste(getwd(),"/R Scripts/Projections/Accuscore Projections.R", sep=""), echo=TRUE) #Paywalled
#source(paste(getwd(),"/R Scripts/Projections/FantasyFootballNerd Projections.R", sep=""), echo=TRUE) #Paywalled
#source(paste(getwd(),"/R Scripts/Projections/Footballguys1 Projections.R", sep=""), echo=TRUE)#Paywalled
#source(paste(getwd(),"/R Scripts/Projections/Footballguys2 Projections.R", sep=""), echo=TRUE)#Paywalled
#source(paste(getwd(),"/R Scripts/Projections/Footballguys3 Projections.R", sep=""), echo=TRUE)#Paywalled
#source(paste(getwd(),"/R Scripts/Projections/Footballguys4 Projections.R", sep=""), echo=TRUE)#Paywalled
#source(paste(getwd(),"/R Scripts/Projections/WalterFootball Projections.R", sep=""), echo=TRUE) # later, he doesn't use anyway
#source(paste(getwd(),"/R Scripts/Projections/FantasySharks Projections.R", sep=""), echo=TRUE)
#source(paste(getwd(),"/R Scripts/Projections/FFtoday Projections.R", sep=""), echo=TRUE)

###############
# Calculations
###############

source(paste(getwd(),"/R Scripts/Calculations/Calculate League Projections.R", sep=""), echo=TRUE)
source(paste(getwd(),"/R Scripts/Calculations/Wisdom of the Crowd.R", sep=""), echo=TRUE)
source(paste(getwd(),"/R Scripts/Calculations/Risk.R", sep=""), echo=TRUE)
source(paste(getwd(),"/R Scripts/Calculations/Value Over Replacement.R", sep=""), echo=TRUE)
source(paste0(getwd(), "/R Scripts/Calculations/Tiers.R"), echo=TRUE)