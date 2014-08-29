###########################
# Shiny App: 
# File: server.R
# Description: Dominate your draft with VORP, sleepers, and starters
# Date: 2014-08-24
# Author: Barret Miller
# Notes:
# To do:
# -Possibly optimize for maximum points based on risk once I understand it. See Draft Day calc
# -Show Drop to next best player
###########################

library(shiny)
library(dplyr)

# Options
options(digits=2)

#Functions
source("../../../R Scripts/Functions/Global Settings.R")
source("../../../R Scripts/Functions/Functions.R")
source(paste("../../../R Scripts/Functions/League Settings_", league, ".R", sep=""))

#Load data
load(paste("../../../Data/VOR_", league, ".RData", sep=""))

# Define server logic
shinyServer(function(input, output) {  
  
  # Set up availablePlayerControl
  output$draftedPlayerControl <- renderUI({
    availablePlayers <- projections$name
    names(availablePlayers) <- projections$player
    selectInput("drafted", "Drafted Players", availablePlayers, multiple=TRUE, selectize=TRUE)
  })
  
  output$roundText  <- renderText({
    pick <- length(input$drafted)
    thisRound <- ceiling(pick/numTeams)
    paste("Round: ",  as.character(thisRound)," - Pick: ", as.character(pick))
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
    picksBeforeMe <- picksBetweenNext(lastPick, pickList)
    
    data <- projections %>% 
              filter(!(name %in% input$drafted) & pos %in% input$pos) %>% 
              mutate(pcksB4Nxt = picksBeforeMe,
                     gnB4Nxt = pick<nextPick,
                     Pts = projections,
                     plyr = player,
                     tm = team) %>%
              select(plyr, pos, tm, vor, Pts, risk, gnB4Nxt, pcksB4Nxt, pick)
    data
  }, options = list(iDisplayLength = 8,bFilter = FALSE))

  output$starters <- renderDataTable({
    lastPick <- length(input$drafted)
    pickList <- calcPickNumbers(16, numTeams, as.numeric(input$firstPick))
    nextPick <- nextDraftPick(lastPick, pickList)
    picksBeforeMe <- picksBetweenNext(lastPick, pickList)
    
    data <- projections %>%
              filter(!(name %in% input$drafted) & pos %in% input$pos & risk < 5) %>% 
              mutate(pcksB4Nxt = picksBeforeMe,
                     gnB4Nxt = pick<nextPick,
                     Pts = projections,
                     plyr = player,
                     tm = team) %>%
              select(plyr, pos, tm, vor, Pts, risk, gnB4Nxt, pcksB4Nxt, pick)
    data
  }, options = list(iDisplayLength = 8,bFilter = FALSE))

  output$sleepers <- renderDataTable({
    lastPick <- length(input$drafted)
    pickList <- calcPickNumbers(16, numTeams, as.numeric(input$firstPick))
    nextPick <- nextDraftPick(lastPick, pickList)
    picksBeforeMe <- picksBetweenNext(lastPick, pickList)
    
    data <- projections %>%
              filter(!(name %in% input$drafted) & pos %in% input$pos & risk >= 5) %>% 
              mutate(pcksB4Nxt = picksBeforeMe,
                     gnB4Nxt = pick<nextPick,
                     Pts = projections,
                     plyr = player,
                     tm = team,
                     UpsdPts = Pts + sdPts) %>%
              select(plyr, pos, tm, UpsdPts, vor, Pts, risk, gnB4Nxt, pcksB4Nxt, pick) %>%
              arrang(desc(UpsdPts))
    data
  }, options = list(iDisplayLength = 8,bFilter = FALSE))

})