#' Matrix remover function
#' A sudoku grid ready to be played based on the solution matrix generated.
#' The function clears some of the cases (replacing the numbers with NA)
#' @param Matrix_m The solution matrix generated
#' @return a 9x9 Matrix
#' @author Flavie B.
#' @importFrom stats runif
#' @export matrix_remover
matrix_remover <- function(Matrix_sol) {
  while(sum(is.na(Matrix_sol)) < floor(runif(1,15,40)))
    {
  i <- floor(runif(1,1,9))
  j <- floor(runif(1,1,9))
  Matrix_sol[i,j] <- NA
    }

  return(Matrix_sol)
}

#partir de matrix(NA,9,9) et remplir certaines cases avec Matrix_sol
