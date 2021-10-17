library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Google analytics tracking
  tags$head(includeScript("google-analytics.js")),
  
  # Application title
  titlePanel(HTML('<a target="_blank" href="http://cargocollective.com/theswishcheese">the swish cheese</a> - NBA fantasy draft wizard'), 
             windowTitle = 'the swish cheese - NBA fantasy draft wizard'),
  br(),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      h3("League settings"),
      checkboxGroupInput("cats", "Categories (select at least 1):",
                         c("Field goal percentage" = "FGPCT",
                           "Free throw percentage" = "FTPCT",
                           "Field goals made" = "FGM",
                           "Field goals attempted" = "FGA",
                           "Free throws made" = "FTM",
                           "Free throws attempted" = "FTA",
                           "Three pointers made" = "TPM",
                           "Assists" = "AST",
                           "Steals" = "STL",
                           "Rebounds" = "REB",
                           "Blocks" = "BLK",
                           "Points" = "PTS",
                           "Turnovers" = "TO"
                           ), 
                         selected = c("AST", "REB", "STL", "BLK", "TPM", "PTS", "FGPCT", "FTPCT"),
                         inline = TRUE),
      tags$style(type="text/css", HTML("#cats {margin-bottom: 0px;}")),
      actionLink("all_cats", "Tick 'em all ^"),
      br(),
      br(),
      sliderInput("budget","Select budget:", min=100, max=500, value = 200, step=5),
      sliderInput("n_teams","No. of teams in league:", min=2, max=20, value = 14),
      sliderInput("squad_size","No. of core players:", min=2, max=13, value = 10),
      uiOutput("sel_playerlist"),
      textOutput("team_value"),
      br(),
      actionButton("go", "Run optimization", width = '100%', icon("cog"),
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      br(),
      h3("Optimization settings"),
      sliderInput("n_iters","No. of iterations:", min=10, max=500, value = 350, step = 10),
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