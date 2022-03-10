#
# This is the server logic of the Sudoku Shiny web application.
#

# Define server logic required to display the Sudoku grid
shinyServer(function(input, output) {

    #       if(is.na(Sudoku_grid[i,j])) {
    #         Sudoku_grid[i,j] <- input$case
    #       }

## Generating a playable Sudoku grid of a certain level

  #Create a new global variable with the selected input level
  difficulty <- observeEvent(input$level,{
    assign("inputLevel", input$level, envir = .GlobalEnv)
  })

  #Generate the Sudoku grid from a complete 9x9 matrix (the solution)
  #when the Generate button is clicked
  generate <- eventReactive(input$generate,{

    #Generate the solution matrix and assign it to a global variable
    assign("Matrix_sol", matrix_grid(), envir = .GlobalEnv)

    #Generate a Sudoku grid from the solution matrix
    assign("Sudoku_grid", matrix_remover(Matrix_sol, inputLevel),
           envir = .GlobalEnv)
  })

  #Display the Sudoku grid
  output$Grid <- renderTable({
    generate()
  },
  bordered = TRUE,
  colnames = FALSE,
  spacing = 'l',
  na = "",
  digits = 0)

##Filling the Sudoku grid

  #Assign inputs (line, col, number) to global variables
  line <- observeEvent(input$i,{
    assign("i", input$i, envir = .GlobalEnv)
  })

  col <- observeEvent(input$j,{
    assign("j", input$j, envir = .GlobalEnv)
  })

  num <- observeEvent(input$number,{
    assign("num", input$number, envir = .GlobalEnv)
  })

  #Assign the new Sudoku_grid (filled to a global variable)
  try <- eventReactive(input$confirm,{
    assign("Sudoku_grid", fill(Sudoku_grid), envir = .GlobalEnv)
  })

  #Display the filled Sudoku grid
  output$RespGrid <- renderTable({
    try()
  },
  bordered = TRUE,
  colnames = FALSE,
  spacing = 'l',
  na = "",
  digits = 0)

##Resolving the Sudoku grid

  #Show the solution matrix when the Resolve button is clicked
  resolve <- eventReactive(input$solve,{
    Matrix_sol
  })

  #Display the solution to the Sudoku
  output$Solution <- renderTable({
    resolve()
  },
  bordered = TRUE,
  colnames = FALSE,
  spacing = 'l',
  digits = 0)

})

###############################################################################

#Function to fill in the Sudoku grid
fill <- function(Try_grid) {

  if(is.na(Try_grid[i,j])) {
    Try_grid[i,j] <- num
  }
  return(Try_grid)
}
