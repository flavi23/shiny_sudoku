# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

matrix_grid <- function()
  {
  Matrix1 <- matrix(data = sample(1:9, replace = F), nrow = 1, ncol = 9)
  Matrix2 <- rbind(Matrix1,c(Matrix1[7:9],Matrix1[1:3],Matrix1[4:6]))
  Matrix3 <- rbind(Matrix2,c(Matrix2[2,7:9],Matrix2[2,1:3],Matrix2[2,4:6]))
  }
