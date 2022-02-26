#' Matrix solver function
#' This function solves a sudoku grid.
#' @param Matrix_m The playable matrix generated.
#' @return a 9x9 Matrix
#' @author Flavie B.
#' @export matrix_solver
matrix_solver <- function(Matrix_solved)
{
  empty_cases <- empty_cells(Matrix_solved)

  while(is.data.frame(empty_cases)) {
    for(i in 1:nrow(empty_cases)) {
      #in case of issue check here first
      if(is.data.frame(empty_cases) && empty_cases$nb_sol[i] == 1) {
        Matrix_solved[empty_cases$row[i],empty_cases$col[i]] <- as.numeric(substr(empty_cases$sol[i],2,2))
      }
    }
    empty_cases <- empty_cells(Matrix_solved)
  }
  return(Matrix_solved)
}

#' Empty cells function
#' This function finds empty cells in the sudoku grid and searches for
#' possible numbers to fill them.
#' @param Matrix_m The playable matrix generated.
#' @return a dataframe with information on the empty cells of the matrix
#' @author Flavie B.
#' @export empty_cells
empty_cells <- function(Matrix_m) {

  #creates a table with the row and col of each cell with NA and initialises sol
  empty_cells <- which(is.na(Matrix_m), arr.ind = TRUE)
  sol <- c()
  nb_sol <- c()

  if(nrow(empty_cells) == 0) return(TRUE)

  #initialises a dataframe with the previous table and 9 new columns to know which number is valid
  empty_cells <- data.frame(empty_cells, box = c(1:nrow(empty_cells)), matrix(NA, nrow = nrow(empty_cells), ncol = 9))

  #for each empty cell checks which numbers could be placed in this cell
  for(i in 1:nrow(empty_cells)) {
    empty_cells$box[i] <- num_block(empty_cells$row[i],empty_cells$col[i])
    for(j in 1:9) {
      empty_cells[i,j+3] <- is_valid(Matrix_m, j, empty_cells[i,1],empty_cells[i,2])
    }
    nb_sol <- c(nb_sol,sum(empty_cells[i,4:12], na.rm = FALSE))
    if(nb_sol[i] == 1) {
      sol <- c(sol,colnames(empty_cells[i,4:12])[empty_cells[i,4:12] == TRUE])
    }
    else {
      sol <- c(sol,NA)
    }
  }
  empty_cells <- cbind(empty_cells,nb_sol,sol)
    #recup resultat de empty_cells (nom col où TRUE) et stocker ds empty_cells
    #ou alors utilise direct?
  return(empty_cells)
}

#' Is valid function
#' This function checks if a certain number can be placed in a certain empty
#' case. The number is valid if it does not already appear in the row, column,
#' or block of the case.
#' @param matrix The playable matrix generated.
#' @param num the number to try (an integer between 1 and 9)
#' @param row the row index
#' @param col the column index
#' @return False if the number already appears in the row/column/block,
#' True otherwise.
#' @author Flavie B.
#' @export is_valid
is_valid <- function(matrix, num, row, col) {
  #Checks if the number is already present in the row or column
  if(any(matrix[row,] == num, na.rm = TRUE) ||
     any(matrix[,col] == num, na.rm = TRUE)) {
    return(FALSE)
  }

  #Checks if the number is present in other parts of the 3x3 block
  #pr voir row/col/block affectés qd resout une possibilite
  block <- box(row,col)
  block <- matrix[(3*block$y - 2):(3*block$y),(3*block$x - 2):(3*block$x)]
  if(any(block == num, na.rm = TRUE)) return(FALSE)

  return(TRUE)
}

#Function to define the box around a case with its row and col
box <- function(row,col) {
  box_x <- floor((col-1)/3 + 1)
  box_y <- floor((row-1)/3 + 1)
  return (data.frame(x = box_x, y = box_y))
}

#Function to identify which box a case with its row and col belongs to
num_block <- function(row,col) {
  block <- box(row,col)
  num_block <- block$x + (block$y - 1)*3
  return (num_block)
}

#empty_cells <- function(matrix){
  #which(is.na(matrix), arr.ind = TRUE)}
