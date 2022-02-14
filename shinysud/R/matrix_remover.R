#' Matrix remover function
#' A sudoku grid ready to be played based on the solution matrix generated.
#' The function clears some of the cases (replacing the numbers with NA)
#' @param Matrix_m The solution matrix generated
#' @return a 9x9 Matrix
#' @author Flavie B.
#' @export matrix_play
matrix_remover <- function(Matrix_m)
{
  while(sum(is.na(Matrix_m)) < 5)
    {
  i <- floor(runif(1,1,9))
  j <- floor(runif(1,1,9))
  Matrix_m[i,j] <- NA
    }
  
  return(Matrix_m)
}