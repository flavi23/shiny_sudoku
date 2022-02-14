#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#

library(shiny)
library(bslib)
library(DT)
library(formattable)

# Define server logic required to display the Sudoku grid 
shinyServer(function(input, output) {
  
    Matrix_sol <- matrix_grid()

    output$Grid <- renderTable({
     Sudoku_grid <- matrix_remover(Matrix_sol)
    },  
     bordered = TRUE,
     colnames = FALSE,
     spacing = 'l',
     na = "",
     digits = 0)
    
    resolve <- eventReactive(input$do,{
      Matrix_sol
    })
    
    output$Solution <- renderTable({
      resolve()
    },  
    bordered = TRUE,
    colnames = FALSE,
    spacing = 'l',
    digits = 0)

})
