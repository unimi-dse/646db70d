#Project Stock
library(jsonlite)
library(plotly)
library(shiny)

######################UTILITIES#######################, 
#get unix timestamp
toUnixTimestamp <-  function(date){
  return(as.numeric(as.POSIXct(date, format = "%Y-%m-%d")))
}

toDateTimestamp <- function(unixDate){
  return(as.Date(as.POSIXct(unixDate, origin ="1970-01-01")))  
}

toGranularity <- function(x){
  if(x == "Weekly")
    return("1wk")
  else if(x == "Monthly")
    return("1mo")
  else return("1d")
}
##################FUNCTIONS####################
##################get yahoo.finance data##################
getYahooData <- function(period1, period2, interval, index){
  unix1 <- toUnixTimestamp(period1)
  unix2 <- toUnixTimestamp(period2)
  granularity <- toGranularity(interval)
  baseUrl <- "https://query1.finance.yahoo.com/v8/finance/chart/"
  endUrl <- "&events=history&crumb=AxoyVAgmoo6"
  period <- paste("period1=", unix1, "&", "period2=", unix2, "&interval=", granularity, sep = "")
  url <- paste(baseUrl, index, "?symbol=", index, "&", period, sep="")
  print(url)
  page <- fromJSON(url)
  historicalClose <- data.frame(page$chart$result$indicators$quote[[1]]$close)
  histroicalHigh <- data.frame(page$chart$result$indicators$quote[[1]]$high)
  historicalLow <- data.frame(page$chart$result$indicators$quote[[1]]$low)
  historicalOpen <- data.frame(page$chart$result$indicators$quote[[1]]$open)
  date <- page$chart$result$timestamp
  for (i in date) {
    date <- toDateTimestamp(i)
  }
  historical <- data.frame(date, historicalClose, historicalOpen, histroicalHigh, historicalLow)
  colnames(historical) <- c('Date', 'Close', 'Open', 'High', 'Low')
  return(historical)
}
############################PLOTLY###########
# period1 <- "2019-09-14 01:00:00"
# period2 <- "2020-01-25 01:00:00"
# interval <- "daily"
# index <- "GOOG"
# historical = getYahooData(period1, period2, interval, index)
# min <- format(round(min(historical$Low) - 2*min(historical$Low)/100 , 2), nsmall = 2) 
# max <- format(round(max(historical$High) + 2*max(historical$High/100), 2), nsmall = 2) 
# plot <- plot_ly( x = index(historical), y = historical$Close, type="scatter", mode = "lines", fill = "tozeroy", fillcolor = "blue", 
#                  hoverinfo = "text",text=paste(index(historical), 
#                                                "<br> Close: ",  format(round(historical$Close, 2), nsmall = 2),
#                                                "<br> Open: ",  format(round(historical$Open, 2), nsmall = 2),
#                                                "<br> High: ",  format(round(historical$High, 2), nsmall = 2),
#                                                "<br> Low: ",  format(round(historical$Low, 2), nsmall = 2))
#                  ) %>% 
#   layout(xaxis = list (title = 'Date',
#                        zeroline = FALSE), 
#          yaxis = list(title = 'Price',
#                       range = c(min, max)))

#################### SHINY####################
##client
ui<-fluidPage(
      titlePanel("Stock price searcher"), 
      sidebarLayout(
        sidebarPanel( 
            helpText("Please type a Stock Symbol and select the Interval"),
            textInput("symbol", label = "Stock Symbol", value = "GOOG"),
            selectInput("granularity", label = "Select Granularity", 
                        choice = list("Daily", "Weekly", "Monthly")),
            dateRangeInput("dateRange", label ="Date Range: ", start = "2019/09/14", end = "2020/01/27", min = "2015/12/12")),
        mainPanel(
          textOutput("selected_symbol"),
          plotlyOutput("plot"))))

##server
server <- function (input, output){
  data <- reactive({getYahooData(input$dateRange[1], input$dateRange[2], input$granularity, input$symbol)})
  output$selected_symbol <- renderText(paste("Stock Prices for:", input$symbol))
  output$plot <- renderPlotly({
                     historical <- data()
                     min <- format(round(min(historical$Low)- 2*min(historical$Low)/100 , 2), nsmall = 2)
                     max <- format(round(max(historical$High) + 2*max(historical$High/100), 2), nsmall = 2)
                     plot <- plot_ly( x = historical$Date, type="candlestick", open = historical$Open, close = historical$Close, high = historical$High, low = historical$Low,
                                  hoverinfo = "text",text=paste(historical$Date, 
                                 "<br> Close: ",  format(round(historical$Close, 2), nsmall = 2),
                                 "<br> Open: ",  format(round(historical$Open, 2), nsmall = 2),
                                 "<br> High: ",  format(round(historical$High, 2), nsmall = 2),
                                 "<br> Low: ",  format(round(historical$Low, 2), nsmall = 2))
                               ) %>%
                             add_lines(x = historical$Date, y= historical$Open, line = list(width = 0.75), inherit = F
                                       ) %>%
                             layout(xaxis = list (title = "Dates", zeroline = FALSE), 
                                    yaxis = list(title = "Price",
                                    range = c(min, max)))
    })
  }
shinyApp(ui, server)