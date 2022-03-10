#' Launch App function
#' @description Launch the Shiny app "sudoku_grids"
#' @return shiny app
#' @author Flavie B.
#' @export launch_app
launch_app <- function() {
  library(shiny)
  runApp('sudoku_grids')
}
