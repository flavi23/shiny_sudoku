#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distMatrix <- renderTable({
      matrix_grid()
    },  
    bordered = TRUE,
    colnames = FALSE,
    digits = 0)    

})
