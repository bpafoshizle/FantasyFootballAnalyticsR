###########################
# Shiny App: 
# File: server.R
# Description: Dominate your draft with VORP, sleepers, and starters
# Date: 2014-08-24
# Author: Barret Miller
# Notes:
# To do:
# -Count draft position and round
# -Show sleepers and starters
# -Possible optimize for maximum points based on risk once I understand it. See Draft Day calc
###########################

library(shiny)
library(dplyr)

#Functions
source("../../../R Scripts/Functions/Global Settings.R")
source("../../../R Scripts/Functions/Functions.R")
source(paste("../../../R Scripts/Functions/League Settings_", league, ".R", sep=""))

#Load data
load(paste("../../../Data/Historical Cost/AvgCost_", league, "-2014.RData", sep=""))

# Define server logic
shinyServer(function(input, output) {  
  
  # Set up availablePlayerControl
  output$draftedPlayerControl <- renderUI({
    availablePlayers <- projections$name
    names(availablePlayers) <- projections$player
    selectInput("drafted", "Drafted Players", availablePlayers, multiple=TRUE, selectize=TRUE)
  })
  
  # Set up position selector
  output$positionSelectControl <- renderUI({
    selectInput("pos", "Show Positions", c("QB","RB","WR","TE"), c("QB","RB","WR","TE"),multiple=TRUE, selectize=FALSE)
  })
  
  # Render table according to selections
  output$rankings <- renderDataTable({
    data <- projections %>% 
              filter(!(name %in% input$drafted) & pos %in% input$pos) %>% 
              select(player, pos, team, overallRank, vor, projections, risk, pick, overallRank, pos)
    data
  })

  output$staters <- renderDataTable({
    data <- projections %>%
              filter(!(name %in% input$drafted) & pos %in% input$pos & risk < 5) %>% 
              select(player, pos, team, overallRank, vor, projections, risk, pick, overallRank, pos)
    data
  })

  output$sleepers <- renderDataTable({
    data <- projections %>%
              filter(!(name %in% input$drafted) & pos %in% input$pos & risk >= 5) %>% 
              select(player, pos, team, overallRank, vor, projections, risk, pick, overallRank, pos)
    data
  })

})