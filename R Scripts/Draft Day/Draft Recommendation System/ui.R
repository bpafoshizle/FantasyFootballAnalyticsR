library(shiny)

# Define UI for application that recommends custom league draft candidates
shinyUI(fluidPage(
  # Application title
  titlePanel("Draft Recommendation System"),
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      uiOutput("draftedPlayerControl"),
      uiOutput("positionSelectControl")
    ),  
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("All Player Rankings", dataTableOutput(outputId="rankings")),
        tabPanel("Starters", dataTableOutput(outputId="starters")),
        tabPanel("Sleepers", dataTableOutput(outputId="sleepers"))
      )
    )
  )
))