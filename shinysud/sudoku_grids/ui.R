#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#

library(shiny)
library(bslib)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
    #Theme
    theme = bs_theme(
    bootswatch = "sketchy"),
    
    # Application title
    titlePanel("Sudoku"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          actionButton("do", "Resolve"),
          fluidRow(column(1,tableOutput("Solution")))
        ),

        # Displays the Sudoku grid and the solution once the "Resolve" 
        # button is clicked
        mainPanel(
            tableOutput("Grid")
        )
    )
))
