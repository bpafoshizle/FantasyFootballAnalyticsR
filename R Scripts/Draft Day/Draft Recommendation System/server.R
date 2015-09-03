###########################
# Shiny App: 
# File: server.R
# Description: Dominate your draft with VORP
# Date: 2014-08-24
# Author: Barret Miller
# Notes:
# To do:
#  -Possibly optimize for maximum points based on risk once I understand it. See Draft Day calc
#  -Show Drop to next best player (input tiers and show tiers graphically)
#  -Intercept browser back and prevent or save state. Almost cost me the draft last year. 
###########################

library(data.table)
library(shiny)
library(dplyr)

# Options
options(digits=2)

#Functions
source("~/github/local/FantasyFootballAnalyticsR/R Scripts/Functions/Global Settings.R")
source("~/github/local/FantasyFootballAnalyticsR/R Scripts/Functions/Functions.R")
source(paste("~/github/local/FantasyFootballAnalyticsR/R Scripts/Functions/League Settings_", league, ".R", sep=""))

#Load data
load(paste("~/github/local/FantasyFootballAnalyticsR/Data/Tier_", league, ".RData", sep=""))

# Define server logic
shinyServer(function(input, output) {
  
  # Set up availablePlayerControl
  output$draftedPlayerControl <- renderUI({
    availablePlayers <- projections$name
    names(availablePlayers) <- projections$player
    selectInput("drafted", "Drafted Players", availablePlayers, multiple=TRUE, selectize=TRUE)
  })
  
  output$roundText  <- renderText({
    lastPick <- length(input$drafted)
    pickList <- calcPickNumbers(16, numTeams, as.numeric(input$firstPick))
    pick <- length(input$drafted)
    thisRound <- ceiling(pick/numTeams)
    picksBeforeMe <- picksBetweenNext(lastPick, pickList)
    paste("Round: ",  as.character(thisRound)," - Pick: ", as.character(pick+1), " (In front: ", picksBeforeMe, ")", sep="")
  })
  
  # Set up position selector
  output$positionSelectControl <- renderUI({
    selectInput("pos", "Show Positions", c("QB","RB","WR","TE"), c("QB","RB","WR","TE"),multiple=TRUE, selectize=FALSE)
  })
  
  output$positionTextControl <- renderUI({
    textInput("firstPick", "Draft Position", "1")
  })
  
  # Render table according to selections
  output$rankings <- renderDataTable({
    lastPick <- length(input$drafted)
    pickList <- calcPickNumbers(16, numTeams, as.numeric(input$firstPick))
    nextPick <- nextDraftPick(lastPick, pickList)
    
    data <- projections %>% 
              filter(!(name %in% input$drafted) 
                     & pos %in% input$pos
                     & sourceName == "averageRobust") %>% 
              mutate(NxtRnd = pick>nextPick,
                     pts = points,
                     plyr = player,
                     tm = team) %>%
              arrange(desc(vor)) %>%
              select(plyr, pos, tm, vor, pts, pick, risk, tier, NxtRnd)
    data
  }, options = list(iDisplayLength = 5, bFilter = FALSE))
  
  
  # Render table according to selections
  output$wrtiers <- renderDataTable({
     tmp <- projections %>%
        filter(!(name %in% input$drafted)
               & pos == "WR"
               & sourceName == "averageRobust") %>%
        summarise(tier = as.integer(tier))
     minTierLeft <- min(tmp[,tier], na.rm=T)
     
     data <- projections %>% 
        filter(!(name %in% input$drafted) 
               & pos == "WR"
               & sourceName == "averageRobust"
               & tier == minTierLeft) %>% 
        mutate(pts = points,
               plyr = player,
               tm = team) %>%
        arrange(desc(vor)) %>%
        select(plyr, pos, tm, tier)
     data
  }, options = list(iDisplayLength = 5, bFilter = FALSE))
  
  
  # Render table according to selections
  output$rbtiers <- renderDataTable({
     tmp <- projections %>%
        filter(!(name %in% input$drafted)
               & pos == "RB"
               & sourceName == "averageRobust") %>%
        summarise(tier = as.integer(tier))
     minTierLeft <- min(tmp[,tier], na.rm=T)
     
     data <- projections %>% 
        filter(!(name %in% input$drafted) 
               & pos == "RB"
               & sourceName == "averageRobust"
               & tier == minTierLeft) %>% 
        mutate(pts = points,
               plyr = player,
               tm = team) %>%
        arrange(desc(vor)) %>%
        select(plyr, pos, tm, tier)
     data
  }, options = list(iDisplayLength = 5, bFilter = FALSE))
  
  # Render table according to selections
  output$qbtiers <- renderDataTable({
     tmp <- projections %>%
        filter(!(name %in% input$drafted)
               & pos == "QB"
               & sourceName == "averageRobust") %>%
        summarise(tier = as.integer(tier))
     minTierLeft <- min(tmp[,tier], na.rm=T)
     
     data <- projections %>% 
        filter(!(name %in% input$drafted) 
               & pos == "QB"
               & sourceName == "averageRobust"
               & tier == minTierLeft) %>% 
        mutate(pts = points,
               plyr = player,
               tm = team) %>%
        arrange(desc(vor)) %>%
        select(plyr, pos, tm, tier)
     data
  }, options = list(iDisplayLength = 5, bFilter = FALSE))
  
  # Render table according to selections
  output$tetiers <- renderDataTable({
     tmp <- projections %>%
        filter(!(name %in% input$drafted)
               & pos == "TE"
               & sourceName == "averageRobust") %>%
        summarise(tier = as.integer(tier))
     minTierLeft <- min(tmp[,tier], na.rm=T)
     
     data <- projections %>% 
        filter(!(name %in% input$drafted) 
               & pos == "TE"
               & sourceName == "averageRobust"
               & tier == minTierLeft) %>% 
        mutate(pts = points,
               plyr = player,
               tm = team) %>%
        arrange(desc(vor)) %>%
        select(plyr, pos, tm, tier)
     data
  }, options = list(iDisplayLength = 5, bFilter = FALSE))

})