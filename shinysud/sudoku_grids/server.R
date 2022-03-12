#
# This is the server logic of the Sudoku Shiny web application.
#

# Define server logic required to display the Sudoku grid
shinyServer(function(input, output) {

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

    #Store the Sudoku grid in another variable
    assign("Start_grid", Sudoku_grid, envir = .GlobalEnv)
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

  #Assign the new filled Sudoku_grid to the global variable
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

  #Check if the filled Sudoku grid is the same as the Solution
  check_solved <- eventReactive(input$verify,{
    if(all(!is.na(Sudoku_grid))) {
      if(all(Sudoku_grid == Matrix_sol)) {
        return("Congrats, you solved the Sudoku!")
      }
      else {
        return("Oh, that's not quite right... Try again?")
      }
    }
  })

  #Display a message
  output$Check <- renderText({
    check_solved()
  })

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

  if(is.na(Start_grid[i,j])) {
    Try_grid[i,j] <- num
  }
  return(Try_grid)
}
