library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Google analytics tracking
  tags$head(includeScript("google-analytics.js")),
  # Application title
  titlePanel("theswishcheese NBA fantasy player evaluation"),
  br(),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("budget","Select budget:", min=100, max=500, value = 200, step=50),
      sliderInput("n_teams","No. of teams in league:", min=4, max=20, value = 14),
      sliderInput("squad_size","No. of core players:", min=4, max=13, value = 10),
      uiOutput("sel_playerlist"),
      textOutput("team_value1"),textOutput("team_value"),textOutput("team_value2")
    ),

    mainPanel(
      div(plotOutput("valueplot")),
      div(tableOutput("sel_player_table"))
      #plotOutput("iterplot")
    )
  )
))