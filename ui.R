library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Google analytics tracking
  tags$head(includeScript("google-analytics.js")),
  
  # Application title
  titlePanel(HTML('<a target="_blank" href="http://cargocollective.com/theswishcheese">the swish cheese</a> - NBA fantasy draft tool'), 
             windowTitle = 'the swish cheese - NBA fantasy draft tool'),
  br(),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      h3("League settings"),
      # checkboxGroupInput("cats", "Categories:",
      #                    c("TO" = "TO",
      #                      "PTS" = "Points",
      #                      "FG%" = "FGPCT")),
      sliderInput("budget","Select budget:", min=100, max=500, value = 200, step=50),
      sliderInput("n_teams","No. of teams in league:", min=2, max=20, value = 14),
      sliderInput("squad_size","No. of core players:", min=2, max=13, value = 10),
      uiOutput("sel_playerlist"),
      textOutput("team_value"),
      br(),
      actionButton("go", "Run optimization", width = '100%'),
      br(),
      h3("Optimization settings"),
      sliderInput("n_iters","No. of iterations:", min=10, max=500, value = 50, step = 10),
      sliderInput("n_samples","No. of samples per iteration:", min=20, max=100, value = 50, step = 10),
      h3("Final valuation table"),
      p("(Only available once optimization is complete)"),
      downloadButton('downloadData', 'Download')
    ),

    mainPanel(
      h4("Optimization performance (over selected players)"),
      plotOutput("iterplot"),
      br(),
      h4("Final player values"),
      plotOutput("valueplot")
      # div(tableOutput("sel_player_table")),
      
    )
  )
))