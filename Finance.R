#Project Stock
library(jsonlite)
library(zoo)
##################get yahoo.finance data
#get unix timestamp
toUnixTimestamp <-  function(date){
  return(as.numeric(as.POSIXct(date)))
}

toDateTimestamp <- function(unixDate){
  return(as.Date(as.POSIXct(unixDate, origin="1970-01-01")))  
}


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
  date <- page$chart$result$timestamp
  for (i in date) {
    date <- toDateTimestamp(i)
  }
  historical <- data.frame(historicalClose, histroicalHigh, historicalLow)
  colnames(historical) <- c('Close', 'High', 'Low')
  return(zoo(historical, order.by = date))
}

toInterval <- function(x){
  if(x=="weekly")
    return("1wk")
  else if(x=="monthly")
      return("1mo")
  else return("1d")
  }