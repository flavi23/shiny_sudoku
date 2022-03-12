#
#This is the user-interface definition of The Sudoku Shiny web application.
#

#library(shiny)
library(bslib)

#Define UI for application that generates a Sudoku grid and its solution
shinyUI(fluidPage(
  #Theme
  theme = bs_theme(
    bootswatch = "sketchy"),

  #Application title
  titlePanel("Sudoku"),

  #Sidebar with a select input for choosing the level of the Sudoku
  sidebarLayout(sidebarPanel(selectInput("level", "Level",
                                         c("Easy","Medium","Difficult")),
                             sliderInput("i", "Line", 1,
                                         min = 1, max = 9),
                             sliderInput("j", "Column", 1,
                                         min = 1, max = 9),
                             numericInput("number", "Number", 1,
                                          min = 1, max = 9)
  ),
  #New panels for generating and displaying the Sudoku grid
  # + displaying the solution
  mainPanel(navlistPanel(
    tabPanel(actionButton("generate","Generate"),"Grid",
             tableOutput("Grid")),
    tabPanel(actionButton("confirm","Confirm"),"Your try",
             tableOutput("RespGrid"), actionButton("verify","Verify")),
    tabPanel(actionButton("solve", "Resolve"),"Solution",
             tableOutput("Solution"))),
    textOutput("Check")
  )
  )
))
