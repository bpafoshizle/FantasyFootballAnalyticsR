library(shiny)

source("~/github/local/FantasyFootballAnalyticsR/R Scripts/Functions/Global Settings.R")

# Define UI for application that recommends custom league draft candidates
shinyUI(fluidPage(
  # Application title
  titlePanel(paste0("Draft Recommendation System - ", league, " - ", season)),
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      verbatimTextOutput("roundText"),
      uiOutput("positionTextControl"),
      uiOutput("positionSelectControl"),
      uiOutput("draftedPlayerControl")
    ),  
    # Show a plot of the generated distribution
    mainPanel(
        tabsetPanel(
           tabPanel("All Player Rankings", dataTableOutput(outputId="rankings"))
        ),
        tabsetPanel(
           tabPanel("RB Tiers", dataTableOutput(outputId="rbtiers")),
           tabPanel("WR Tiers", dataTableOutput(outputId="wrtiers")),
           tabPanel("QB Tiers", dataTableOutput(outputId="qbtiers")),
           tabPanel("TE Tiers", dataTableOutput(outputId="tetiers"))
        )
    )
  )
))