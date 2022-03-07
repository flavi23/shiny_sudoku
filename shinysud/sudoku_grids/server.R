#
# This is the server logic of the Sudoku Shiny web application.
#

fill <- function(Try_grid) {

  if(is.na(Try_grid[i,j])) {
    Try_grid[i,j] <- num
  }
  else {
    Try_grid[i,j] <- Try_grid[i,j]
  }
  return(Try_grid)
}

# Define server logic required to display the Sudoku grid
shinyServer(function(input, output) {

    #       if(is.na(Sudoku_grid[i,j])) {
    #         Sudoku_grid[i,j] <- input$case
    #       }

## Generating a playable Sudoku grid of a certain level

  #Creates a new global variable with the selected input level
  difficulty <- observeEvent(input$level,{
    assign("inputLevel", input$level, envir = .GlobalEnv)
  })

  #Generates the Sudoku grid from a complete 9x9 matrix (the solution)
  #when the Generate button is clicked
  generate <- eventReactive(input$generate,{

    #Generates the solution matrix and assign it to a global variable
    assign("Matrix_sol", matrix_grid(), envir = .GlobalEnv)

    #Generates a Sudoku grid from the solution matrix
    assign("Sudoku_grid", matrix_remover(Matrix_sol, inputLevel),
           envir = .GlobalEnv)
  })

  #Displays the Sudoku grid
  output$Grid <- renderTable({
    generate()
  },
  bordered = TRUE,
  colnames = FALSE,
  spacing = 'l',
  na = "",
  digits = 0)

##Filling the Sudoku grid

  #Assign inputs to global variables
  line <- observeEvent(input$i,{
    assign("i", input$i, envir = .GlobalEnv)
  })

  col <- observeEvent(input$j,{
    assign("j", input$j, envir = .GlobalEnv)
  })

  num <- observeEvent(input$number,{
    assign("num", input$number, envir = .GlobalEnv)
  })

  output$RespGrid <- renderTable({
    fill(Sudoku_grid)
  },
  bordered = TRUE,
  colnames = FALSE,
  spacing = 'l',
  na = "",
  digits = 0)

##Resolving the Sudoku grid

  #Shows the solution matrix when the Resolve button is clicked
  resolve <- eventReactive(input$solve,{
    Matrix_sol
  })

  #Displays the solution to the Sudoku
  output$Solution <- renderTable({
    resolve()
  },
  bordered = TRUE,
  colnames = FALSE,
  spacing = 'l',
  digits = 0)

})
