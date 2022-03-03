#Test Data matrix_solved (a 'diabolic' Sudoku found on a website)

l1 <- c(NA,NA,NA,NA,NA,7,NA,NA,2)
l2 <- c(NA,NA,1,5,NA,NA,7,9,NA)
l3 <- c(NA,9,NA,NA,NA,NA,NA,NA,4)
l4 <- c(NA,NA,NA,NA,NA,NA,NA,NA,9)
l5 <- c(NA,1,NA,NA,NA,4,3,6,NA)
l6 <- c(NA,NA,5,NA,8,NA,NA,NA,NA)
l7 <- c(3,NA,NA,4,NA,NA,NA,NA,NA)
l8 <- c(NA,NA,NA,NA,NA,NA,2,NA,NA)
l9 <- c(NA,6,NA,NA,NA,3,1,7,NA)

matrixdur <- data.frame(l1,l2,l3,l4,l5,l6,l7,l8,l9)
matrixdur <- t(as.matrix(matrixdur))
