#' Shiny App Server

shiny::shinyServer(function(input, output){

  #Grabs data from yahoo finance every time symbol or date range or granularity change#

  data <- shiny::reactive({quantmod::getSymbols(input$symbol, src = "yahoo", from = input$dateRange[1], to = input$dateRange[2], periodicity = input$granularity, auto.assign = FALSE)})

  # Render the current symbol in main panel#

  output$selected_symbol <- shiny::renderText(paste("Stock Prices for:", input$symbol))

  #Render graphs on main panel#
  output$plot <- plotly::renderPlotly({

    #Transforms Data into a dataframe#
    historical <- data.frame(Date = zoo::index(data()), zoo::coredata(data()))

    colnames(historical) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

    #Plots candlestick graph#
    plotly::plot_ly( x = historical$Date, type="candlestick",
                     open = historical$Open, close = historical$Close,
                     high = historical$High, low = historical$Low) %>%

      plotly::add_lines(x = historical$Date, y= historical$Open,
                line = list(width = 0.75), inherit = F) %>%

      plotly::layout(xaxis = list (title = "Dates", zeroline = FALSE),
             yaxis = list(title = "Price"))
  })
})
