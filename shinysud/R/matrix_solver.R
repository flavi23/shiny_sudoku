#' Matrix solver function
#' This function solves a Sudoku grid.
#' @param Matrix The playable matrix generated.
#' @param layer Integer showing depth of the called functions. Default = 0.
#' @return a 9x9 Matrix corresponding to the solution found by the solver,
#' or TRUE/FALSE
#' @author Flavie B.
#' @export matrix_solver
matrix_solver <- function(Matrix,layer = 0) {
  print(paste0("Depth: ",layer))
  #temp is a list containing the dataframe emptycells and a Boolean
  temp <- empty_cells(Matrix)
  empty_cases <- as.data.frame(temp[1])
  if(temp[2] == FALSE && !any(is.na(Matrix))) {
    print("Sudoku solved")
    return(Matrix)
  }

  #while all the functions return TRUE, and the matrix is not solved,
  #matrix_solver tries to solve it
  while(temp[2] == TRUE) {
    for(i in 1:nrow(empty_cases)) {
      #if there is a solution for each row of empty_cases,
      #add the corresponding number to Matrix
      if(!is.na(empty_cases$sol[i])) {
        Matrix[empty_cases$row[i],empty_cases$col[i]] <- as.numeric(
          substr(empty_cases$sol[i],2,2))
      }
      #if one case does not have a solution, stop everything
      if(empty_cases$nb_sol[i] == 0) {
        return(FALSE)
      }
    }
    #recalculate empty_cells for the updated Matrix,
    #checking if the matrix is solved
    temp <- empty_cells(Matrix)
    empty_cases <- as.data.frame(temp[1])
  }
  #if the by deduction method is not enough to solve the matrix
  #try the trials and errors method
  if(nrow(empty_cases) != 0) {
    Matrix <- trials_errors(Matrix,empty_cases,layer+1)
    return(Matrix)
  }
  print('Success! Sudoku solved')
  return(Matrix)
}

#' Empty cells function
#' This function finds empty cells in the sudoku grid and searches for
#' possible numbers to fill them.
#' @param Matrix_m The playable matrix generated.
#' @return a list containing a dataframe with information on the empty cells
#' of the matrix and a Boolean
#' @author Flavie B.
#' @export empty_cells
empty_cells <- function(Matrix_m) {

  #creates a table with the row and col of each cell with NA and initialises sol
  emptycells <- which(is.na(Matrix_m), arr.ind = TRUE)
  sol <- c()
  nb_sol <- c()

  #if no NA in matrix, no need to do the rest (the matrix is already solved)
  if(nrow(emptycells) == 0) return(list(emptycells,FALSE))

  #initialises a dataframe with the previous table and 9 new columns to know which number is valid
  emptycells <- data.frame(emptycells, box = c(1:nrow(emptycells)),
                           matrix(NA, nrow = nrow(emptycells), ncol = 9))

  #for each empty cell checks which numbers could be placed in this cell
  for(i in 1:nrow(emptycells)) {
    emptycells$box[i] <- num_block(emptycells$row[i],emptycells$col[i])
    for(j in 1:9) {
      emptycells[i,j+3] <- is_valid(Matrix_m, j,
                                    emptycells[i,1],emptycells[i,2])
    }
    nb_sol <- c(nb_sol,sum(emptycells[i,4:12], na.rm = FALSE))
    if(nb_sol[i] == 1) {
      sol <- c(sol,colnames(emptycells[i,4:12])[emptycells[i,4:12] == TRUE])
    }
    else {
      sol <- c(sol,NA)
    }
  }
  emptycells <- cbind(emptycells,nb_sol,sol)

  if(all(emptycells[,4:12] == FALSE)) {
    print('alors')
    return(list(emptycells,FALSE))
  }

  if(all(emptycells$nb_sol > 1)) {
    temp <- by_deduction(emptycells)
    if(temp[2] == FALSE) {
      return(list(emptycells,FALSE))
    }
    else {
      emptycells <- as.data.frame(temp[1])
    }
  }

  return(list(emptycells,TRUE))
}

by_deduction <- function(emptycells) {
  for(i in 1:nrow(emptycells)) {
    x <- c(colnames(emptycells[i,4:12])[emptycells[i,4:12] == TRUE])
    for(j in 1:length(x)) {
      y <- unlist(emptycells[x[j]])
      y[i] <- FALSE
      if(all(y[emptycells$col == emptycells$col[i]] == FALSE)
         || all(y[emptycells$row == emptycells$row[i]] == FALSE)
         || all(y[emptycells$box == emptycells$box[i]] == FALSE)) {
        emptycells$sol[i] <- x[j]
        return(list(emptycells,TRUE))
      }
    }
  }
  print("Deduction not enough")
  return(list(emptycells,FALSE))
}

trials_errors <- function(matrix_m, emptycells,layer) {

  solved <- matrix(NA,9,9)
  solved_state <- FALSE

  emptycells <- emptycells[order(emptycells$nb_sol),]
  num_trial <- colnames(emptycells[1,4:12])[emptycells[1,4:12] == TRUE]
  for(j in 1:length(num_trial)) {
    print(paste0(emptycells$row[1],"x",emptycells$col[1]," Number: ",
                 num_trial[j]))
    matrix_m[emptycells$row[1],emptycells$col[1]] <- as.numeric(
      substr(num_trial[j],2,2))
    solve <- matrix_solver(matrix_m,layer)

    if(!is.matrix(solve) && solve == TRUE){return(TRUE)}
    #checks if the matrix is solved
    if(is.matrix(solve) && !any(is.na(solve))) {
      #stores the first solved matrix
      if(solved_state == FALSE) {
        solved_state <- TRUE
        solved <- solve
      }

      if(solved_state == TRUE && !all(solved == solve)) {
        #if already stored it means that there are multiple solutions
        print("Multiple solutions")
        return(TRUE)
      }
    }
  }
  #check if the matrix is really solved before returning
  if(is.matrix(solved) && !any(is.na(solved))) {
    if(layer == 1){print("Unique solution!")}
    return(solved)
  }
  #else in case of malfunction
  return(FALSE)
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
