#'StockSearch
#'
#'@description
#'Launch the shiny app. It will show the graph plot (candlestick) of the stock symbol in the period range selected by the
#'user. It is also possibile to select the periodicity which can be daily, weekly or monthly. The default settings on first boot is 'GOOG'
#'from 14/09/2019 to 27/01/2020. Data are taken from Yahoo finance via 'quantmode' package.
#'@return shiny app
#'
#'@examples
#'stockSearch()
#'
#'
#'@export

stockSearch <- function(){
  dir <- system.file("shinyApp", package = "stockSearch")
  shiny::runApp(dir)

}
