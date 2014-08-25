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
      dataTableOutput(outputId="rankings")#,
      #uiOutput("starters"),
      #uiOutput("starters")
    )
  )
))