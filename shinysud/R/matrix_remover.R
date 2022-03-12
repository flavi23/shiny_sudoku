#' Matrix remover function
#' @description A Sudoku grid ready to be played based on the solution matrix generated.
#' The function clears some of the cases (replacing the numbers with NA), tests
#' the obtained grid with matrix_solver to make sure it has a unique solution
#' and is of the expected difficulty.
#' @param Matrix_m The solution matrix generated
#' @param Level The expected difficulty of the Sudoku
#' @return a 9x9 Matrix corresponding to a playable Sudoku grid
#' @author Flavie B.
#' @importFrom stats runif
#' @export matrix_remover
matrix_remover <- function(Matrix_m,Level) {

  #to see how long it takes to generate the Sudoku grid
  startTime <- Sys.time()

  #make sure the level selected exists
  rank <- c("Impossible","TooEasy","Easy","Medium","Difficult")
  if(!Level %in% rank){
    stop("Level of matrix remover unknown")
    }

  #re initialise Difficulty variable and keep a copy of Matrix_m to go back to
  #if needed
  Matrix_begin <- Matrix_m
  assign("Dif","TooEasy",envir = .GlobalEnv)

  #while a grid of the expected level has not been generated
  while(Dif != Level){

    #initialisation
    tryNA <- matrix(TRUE,9,9)
    Matrix_m <- Matrix_begin
    Sudoku <- Matrix_m
    assign("Dif","TooEasy",envir = .GlobalEnv)

    while((Dif != Level && !all(tryNA==FALSE)) && which(rank == Level) > which(rank == Dif)) {
      Matrix_m <- Sudoku

      #at position j, in the vector containing the numbers
      #for which we did not try to put NA yet
      #try to put NA now
      j <- floor(runif(1,1,length(Matrix_m[tryNA == TRUE])))
      Matrix_m[tryNA == TRUE][j] <- NA

      #and in this same position j, change TRUE in FALSE, which means
      #we tried to put NA instead of the number in this position.
      #this is to make sure we don't always try the same positions
      tryNA[tryNA == TRUE][j] <- FALSE

      #use matrix_solver to make sure the Sudoku has a unique solution
      #and to get the difficulty
      solving <- matrix_solver(Matrix_m)

      #to check the difficulty
      print(Dif)

      #if solving is indeed a matrix, it means it's a valid Sudoku grid
      if(is.matrix(solving)) {
        Sudoku <- Matrix_m
      }
    }
  }
  #when the difficulty corresponds to the expected level, we can store the
  #matrix obtained, get out of the while loop, and return the grid

  #print how long it took to find a grid of the expected difficulty
  #note:
  print(paste0("Difficulty found in : ",round(Sys.time() - startTime,2)," sec"))
  return(Sudoku)
}
