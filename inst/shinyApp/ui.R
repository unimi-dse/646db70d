##Client
#'
#'UI for application
#'
shiny::shinyUI (
  fluidPage(
  titlePanel("Stock price searcher"),
  sidebarLayout(
    #Sidebar Panel#
    sidebarPanel(

      #Help text to use the program#
      helpText("Please type a Stock Symbol and select the Interval"),

      #Text area for typing the stock symbol#
      textInput("symbol", label = "Stock Symbol", value = "AAPL"),

      #Selection Box for choosing granularity#
      selectInput("granularity", label = "Select Granularity",
                  choice = list("daily", "weekly", "monthly")),

      #Date interval for selecting data range#
      dateRangeInput("dateRange", label ="Date Range: ", start = "2019/09/14",
                     end = "2020/01/27", min = "2015/12/12")),

    #Main Panel#
    mainPanel(

      #Text output that displays the current symbol#
      textOutput("selected_symbol"),

      #Graph output graph based on symbol, data range, granularity inserted#
      plotly::plotlyOutput("plot")
      )
    )
  )
)


