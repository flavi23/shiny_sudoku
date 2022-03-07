#' Matrix remover function
#' A Sudoku grid ready to be played based on the solution matrix generated.
#' The function clears some of the cases (replacing the numbers with NA).
#' @param Matrix_m The solution matrix generated
#' @param Level The expected difficulty of the Sudoku
#' @return a 9x9 Matrix corresponding to a playable Sudoku grid
#' @author Flavie B.
#' @importFrom stats runif
#' @export matrix_remover
matrix_remover <- function(Matrix_m,Level) {

  startTime <- Sys.time()

  rank <- c("Impossible","TooEasy","Easy","Medium","Difficult")
  if(!Level %in% rank){stop("Level of matrix remover unknown")}

  Matrix_begin <- Matrix_m
  assign("Dif","TooEasy",envir = .GlobalEnv)


  while(Dif != Level){

    tryNA <- matrix(TRUE,9,9)
    Matrix_m <- Matrix_begin
    Sudoku <- Matrix_m
    assign("Dif","TooEasy",envir = .GlobalEnv)

    while((Dif != Level && !all(tryNA==FALSE)) && which(rank == Level) > which(rank == Dif)) {
      Matrix_m <- Sudoku

      #i <- floor(runif(1,1,10))
      j <- floor(runif(1,1,length(Matrix_m[tryNA == TRUE])))

      #A la position J dans notre vecteur qui contient les chiffres où on a pas essayé de mettre des NA
      #On met un NA
      #A cette meme position on met un false dans la matrice qui liste les cases essayés
      Matrix_m[tryNA == TRUE][j] <- NA
      tryNA[tryNA == TRUE][j] <- FALSE

      solving <- matrix_solver(Matrix_m)
      print(Dif)
      print(tryNA)

      if(is.matrix(solving)) {
        Sudoku <- Matrix_m
      }
    }
  }
  print(paste0("Difficulty found in : ",round(Sys.time() - startTime,2)," sec"))
  return(Sudoku)
}

#une fois qu'on a fait difficulté, l'utiliser pour condition de while:
#while(stop == FALSE)
#if(diff == 1) prendre une grille avec ~20 NA qui utilise pas by deduction
#if(diff == 2) grille avec plus de NA qui utilise peu de fois by deduction
#if(diff == 3) grille avec encore plus de NA qui utilise plus by deduction
#if(diff == 4) grille qui utilise trials and errors

# #initialise a matrix of NA
# Sudoku <- matrix(NA,9,9)
#
# #fill one case per line with the corresponding number from Matrix_m
# for(i in 1:9) {
#   b <- floor(runif(1,1,9))
#   Sudoku[i,b] <- Matrix_m[i,b]
# }
#
# #fill one case per column with the corresponding number from Matrix_m
# for(j in 1:9) {
#   a <- floor(runif(1,1,9))
#   Sudoku[a,j] <- Matrix_m[a,j]
# }
#
# x <- 0
# rand <- floor(runif(1,5,20))
# while(x < rand) {
#   i <- floor(runif(1,1,9))
#   j <- floor(runif(1,1,9))
#
#   if(is.na(Sudoku[i,j])) {
#     Sudoku[i,j] <- Matrix_m[i,j]
#     x <- x + 1
#   }
# }
# #while(sum(is.na(Matrix_m)) < floor(runif(1,30,50)))
# return(Sudoku)
