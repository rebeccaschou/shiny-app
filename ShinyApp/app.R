#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinythemes)
library(DT)

# Import League of Legends Data
# setwd("/Users/rebecca/Desktop/Dropbox/HCBPS_Data_Project")
leagueDataDiamond <- read.csv(file = 'lol_ranked_diamond_data.csv')

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("sandstone"),

    # Application title
    titlePanel("League of Legends Diamond/Master Ranked Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            # Select variable for y-axis
            selectInput(
                inputId = "y",
                label = "Y-axis (Red Team):",
                choices = c("CS Per Minute" = "redCSPerMin", 
                            "Gold Per Minute" = "redGoldPerMin", 
                            "Total Gold" = "redTotalGold", 
                            "Gold Difference" = "redGoldDiff", 
                            "Total Experience" = "redTotalExperience", 
                            "Experience Difference" = "redExperienceDiff", 
                            "Kills" = "redKills", 
                            "Assists" = "redAssists", 
                            "Wards Placed" = "redWardsPlaced"),
                selected = "redGoldPerMin"
            ),
            
            # Select variable for x-axis
            selectInput(
                inputId = "x",
                label = "X-axis (Blue Team):",
                choices = c("CS Per Minute" = "blueCSPerMin", 
                            "Gold Per Minute" = "blueGoldPerMin", 
                            "Total Gold" = "blueTotalGold", 
                            "Gold Difference" = "blueGoldDiff", 
                            "Total Experience" = "blueTotalExperience", 
                            "Experience Difference" = "blueExperienceDiff", 
                            "Kills" = "blueKills", 
                            "Assists" = "blueAssists", 
                            "Wards Placed" = "blueWardsPlaced"),
                selected = "blueExperienceDiff"
            ),
            
            # Select variable for x-axis
            radioButtons(
                inputId = "method",
                label = "Line of Best Fit:",
                choiceNames = c("Linear", "Automatic (Non-Linear)"),
                choiceValues = c('lm', "auto")
            ),
            
            selectInput(
                inputId = "color",
                label = "Color by:",
                choices = c(
                    "First Blood" = "blueFirstBlood",
                    "Winner" = "blueWins"
                ),
                selected = "blueWins"
            ),
            
            sliderInput(
                inputId = "alpha",
                label = "Point Opacity:",
                step = 0.1,
                min = 0,
                max = 1,
                value = 0.5
            ),
            
            numericInput(
                inputId = "n",
                label = "Number of Data Points:",
                step = 50,
                min = 100,
                max = 9879,
                value = 500
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                type = "tabs",
                tabPanel("Scatterplot", plotOutput(outputId = "scatterplot")),
                tabPanel("Data", dataTableOutput(outputId = "datatable")),
                tabPanel(
                    "Reference",
                    tags$p(
                        "This data was obtained from",
                        tags$a("kaggle.com", href = "https://www.kaggle.com/bobbyscience/league-of-legends-diamond-ranked-games-10-min"), "."
                    ),
                    tags$p(
                        "This data represents", nrow(leagueDataDiamond),
                        "randomly sampled League of Legends games in Diamond/Master ELO. The dataset gives summary data obtained ten minutes into each game and includes nineteen features per team (thirty-eight features in total, for two teams) that describe the status of the game at the ten minute point, as well as the outcome of the game. These variables are all quantitative save for two, which are categorical: whether each team drew first blood, and whether each team won. Each row of the dataset is one unique game that is separate and independent from each other row. The dataset is not missing any data values. "
                    )
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    dataInput <- reactive({
        leagueDataDiamond %>% 
            sample_n(size = input$n) # Randomly selects n of the 10,000 data points 
    })
    
    output$scatterplot <- renderPlot({
        dataInput() %>% 
            ggplot(aes_string(x = input$x, y = input$y, color = input$color)) +
            geom_point(size = 0.1, alpha = input$alpha) +
            geom_smooth(method = input$method) # Uses a linear model
            # labs(x = 'Blue Team Experience Difference', y = 'Red Team Gold Earned Per Minute')
    })
    
    output$datatable <- renderDataTable({
        DT::datatable(data = leagueDataDiamond,
                      options = list(pageLength = 10),
                      rownames = FALSE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
