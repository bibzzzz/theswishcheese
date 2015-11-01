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
      h3("League settings"),
      sliderInput("budget","Select budget:", min=100, max=500, value = 200, step=50),
      sliderInput("n_teams","No. of teams in league:", min=4, max=20, value = 14),
      sliderInput("squad_size","No. of core players:", min=4, max=13, value = 10),
      uiOutput("sel_playerlist"),
      textOutput("team_value"),
      br(),
      h3("Optimization settings"),
      sliderInput("n_iters","No. of iterations:", min=10, max=100, value = 50, step = 10),
      sliderInput("n_samples","No. of samples per iteration:", min=50, max=1000, value = 200, step = 50)
    ),

    mainPanel(
      plotOutput("iterplot"),
      br(),
      plotOutput("valueplot")
      # div(tableOutput("sel_player_table")),
      
    )
  )
))