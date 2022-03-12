#' Launch App function
#' @description Launch the Shiny app "sudoku_grids". The directory "sudoku_grids" needs to be set as the working directory.
#' @return shiny app
#' @author Flavie B.
#' @export launch_app
launch_app <- function() {
  runApp(
    appDir = getwd(),
    port = getOption("shiny.port"),
    launch.browser = getOption("shiny.launch.browser", interactive()),
    host = getOption("shiny.host", "127.0.0.1"),
    workerId = "",
    quiet = FALSE,
    display.mode = c("auto", "normal", "showcase"),
    test.mode = getOption("shiny.testmode", FALSE)
  )
}
