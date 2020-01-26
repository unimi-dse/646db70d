#Project Stock
library(jsonlite)
library(zoo)
library(ggplot2)
library(plotly)

######################UTILITIES#######################
#get unix timestamp
toUnixTimestamp <-  function(date){
  return(as.numeric(as.POSIXct(date)))
}

toDateTimestamp <- function(unixDate){
  return(as.Date(as.POSIXct(unixDate, origin="1970-01-01")))  
}

toInterval <- function(x){
  if(x=="weekly")
    return("1wk")
  else if(x=="monthly")
    return("1mo")
  else return("1d")
}
##################FUNCTIONS####################
##################get yahoo.finance data##################
getYahooData <- function(period1, period2, granularity, index){
  baseUrl <- "https://query1.finance.yahoo.com/v8/finance/chart/"
  endUrl <- "&events=history&crumb=AxoyVAgmoo6"
  period <- paste("period1=", period1, "&", "period2=", period2, "&interval=", granularity, sep = "")
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
  historical <- data.frame(historicalClose, historicalOpen, histroicalHigh, historicalLow)
  colnames(historical) <- c('Close', 'Open', 'High', 'Low')
  return(zoo(historical, order.by = date))
}
############################PLOTLY###########
plot <- plot_ly( x=~index(historical), y=~historical$Close, type="scatter", mode = "lines", fill = "tonextx",
                 hoverinfo = "text",text=paste(index(historical), 
                                               "<br> Close: ",  format(round(historical$Close, 2), nsmall = 2),
                                               "<br> Open: ",  format(round(historical$Open, 2), nsmall = 2),
                                               "<br> High: ",  format(round(historical$High, 2), nsmall = 2),
                                               "<br> Low: ",  format(round(historical$Low, 2), nsmall = 2))
                 ) %>% 
  layout(xaxis = list (title = 'Date'), 
         yaxis = list(title = 'Price'),
         hovertemplate = )

