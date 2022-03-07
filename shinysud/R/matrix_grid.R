#' Matrix grid function
#' Generates a filled 9x9 matrix corresponding to the solution
#' for the Sudoku grid
#' @return a 9x9 Matrix
#' @author Flavie B.
#' @export matrix_grid
matrix_grid <- function() {
  #matrix initialisation
  Matrix_sol <- matrix(0, nrow = 9, ncol = 9)

  #first three lines
  Matrix_sol[1,] <- sample(1:9, replace = FALSE)
  Matrix_sol[2,] <- offset3(Matrix_sol[1,])
  Matrix_sol[3,] <- offset3(Matrix_sol[2,])

  #columns
  Matrix_sol[,1:3] <- generate3(Matrix_sol[,1:3])
  Matrix_sol[,4:6] <- generate3(Matrix_sol[,4:6])
  Matrix_sol[,7:9] <- generate3(Matrix_sol[,7:9])

  #shuffles the lines and columns of the matrix generated
  Matrix_sol <- mix3(Matrix_sol)

  return(Matrix_sol)
}

###############################################################################

#' Offset 3 function
#'
#' @param Vect A vector of 9 corresponding to the line or column of interest
#' @param Rev A Boolean to indicate the direction of the offset
#' @return The same vector with the first three numbers now being the fourth
#' fifth and sixth and so on (shifting in circle)
#' @author Flavie B.
#' @examples
#' Matrix_m <- matrix(sample(1:9, replace = FALSE), nrow = 9, ncol = 9)
#' Matrix_m[2,] <- offset3(Matrix_m[1,])
#' @export offset3
offset3 <- function(Vect, Rev = FALSE) {
  if(!Rev) Vect <- c(Vect[7:9],Vect[1:3],Vect[4:6])
  else Vect <- c(Vect[4:6],Vect[7:9],Vect[1:3])

  return(Vect)
}

###############################################################################

#' Generate 3 function
#'
#' @param Temp_m A temporary matrix storing the three columns of one block
#' from the main matrix
#' @return The same temporary matrix with the rest of the columns filled
#' (doing another circle shift)
#' @author Flavie B.
#' @examples
#' Matrix_m <- matrix(sample(1:9, replace = FALSE), nrow = 9, ncol = 9)
#' Matrix_m[,1:3] <- generate3(Matrix_m[,1:3])
#' @export generate3
generate3 <- function(Temp_m) {
  Temp_m[,1] <- c(Temp_m[1:3,1],Temp_m[1:3,2],Temp_m[1:3,3])
  Temp_m[,2] <- offset3(Temp_m[,1], Rev = TRUE)
  Temp_m[,3] <- offset3(Temp_m[,2], Rev = TRUE)

  return(Temp_m)
}

###############################################################################

#' Mix 3 function
#'
#' @param Matrix_m A complete matrix generated
#' @return The same matrix with the order of the lines and columns shuffled
#' inside a group of 3
#' @author Flavie B.
#' @export mix3
mix3 <- function(Matrix_m) {
  #loop on all the lines by groups of 3
  for(i in 0:2) {
    temp_x1 <- Matrix_m[1+i*3,]
    temp_x2 <- Matrix_m[2+i*3,]
    temp_x3 <- Matrix_m[3+i*3,]

    #randomize the position of the lines
    ran <- sample(1:3, replace = FALSE)

    Matrix_m[ran[1]+i*3,] <- temp_x1
    Matrix_m[ran[2]+i*3,] <- temp_x2
    Matrix_m[ran[3]+i*3,] <- temp_x3
  }
  #loop on all the columns by groups of 3
  for(i in 0:2) {
    temp_y1 <- Matrix_m[,1+i*3]
    temp_y2 <- Matrix_m[,2+i*3]
    temp_y3 <- Matrix_m[,3+i*3]

    #randomize the position of the columns
    ran <- sample(1:3, replace = FALSE)

    Matrix_m[,ran[1]+i*3] <- temp_y1
    Matrix_m[,ran[2]+i*3] <- temp_y2
    Matrix_m[,ran[3]+i*3] <- temp_y3
  }
    return(Matrix_m)
}
