# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' Matrix grid function
#' Generates a filled 9x9 matrix we will be able to use as a sudoku grid
#' @return a 9x9 Matrix
#' @author Flavie B.
#' @export matrix_grid
matrix_grid <- function()
{
  #matrix initialisation
  Matrix_m <- matrix(0, nrow = 9, ncol = 9)
  
  #first three lines
  Matrix_m[1,] <- sample(1:9, replace = F)
  Matrix_m[2,] <- offset3(Matrix_m[1,])
  Matrix_m[3,] <- offset3(Matrix_m[2,])
  
  #columns
  Matrix_m[,1:3] <- generate3(Matrix_m[,1:3])
  Matrix_m[,4:6] <- generate3(Matrix_m[,4:6])
  Matrix_m[,7:9] <- generate3(Matrix_m[,7:9])
  
  return(Matrix_m)
}


#' Offset 3 function
#'
#' @param Vect A vector of 9 corresponding to the line or column of interest
#' @param Rev A Boolean to indicate the direction of the offset
#' @return The same vector with the first three numbers now being the fourth fifth and sixth and so on (shifting in circle)
#' @author Flavie B.
#' @examples
#' Matrix_m <- matrix(sample(1:9), nrow = 9, ncol = 9)
#' Matrix_m[2,] <- offset3(Matrix_m[1,])
#' @export offset3
offset3 <- function(Vect, Rev = FALSE)
{
  if(!Rev) Vect <- c(Vect[7:9],Vect[1:3],Vect[4:6])
  else Vect <- c(Vect[4:6],Vect[7:9],Vect[1:3])
  
  return(Vect)
}

#' Generate 3 function
#'
#' @param Temp_m A temporary matrix storing the three columns of one block from the main matrix
#' @return The same temporary matrix with the rest of the columns filled (doing another circle shift) 
#' @author Flavie B.
#' @examples
#' Matrix_m <- matrix(sample(1:9), nrow = 9, ncol = 9)
#' Matrix_m[,1:3] <- generate3(Matrix_m[,1:3])
#' @export generate3
generate3 <- function(Temp_m)
{
  Temp_m[,1] <- c(Temp_m[1:3,1],Temp_m[1:3,2],Temp_m[1:3,3])
  Temp_m[,2] <- offset3(Temp_m[,1], Rev = TRUE)
  Temp_m[,3] <- offset3(Temp_m[,2], Rev = TRUE)
  
  return(Temp_m)
} 

#mix3