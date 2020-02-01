library(plotly)
library(quantmod)
library(shiny)

############FUNCTION##############
############SHINY####################

##Client
ui<-fluidPage(
  titlePanel("Stock price searcher"), 
  sidebarLayout(
    sidebarPanel( 
      helpText("Please type a Stock Symbol and select the Interval"),
      textInput("symbol", label = "Stock Symbol", value = "AAPL"),
      selectInput("granularity", label = "Select Granularity", 
                  choice = list("daily", "weekly", "monthly")), 
      dateRangeInput("dateRange", label ="Date Range: ", start = "2019/09/14",
                     end = "2020/01/27", min = "2015/12/12")), 
    mainPanel(
      textOutput("selected_symbol"),
      plotlyOutput("plot")
      )
    )
  )

##Server
server <- function (input, output){
  data <- reactive({getSymbols(input$symbol, src = "yahoo", from = input$dateRange[1], to = input$dateRange[2], periodicity = input$granularity, auto.assign = FALSE)})
  output$selected_symbol <- renderText(paste("Stock Prices for:", input$symbol))
  output$head <- renderText(
    historical <- data.frame())
  output$plot <- renderPlotly({
    historical <- data.frame(Date = index(data()), coredata(data()))
    colnames(historical) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
    plot <- plot_ly( x = historical$Date, type="candlestick", 
                     open = historical$Open, close = historical$Close, 
                     high = historical$High, low = historical$Low)
            %>% 
            add_lines(x = historical$Date, y= historical$Open, 
                      line = list(width = 0.75), inherit = F)
            %>%
            layout(xaxis = list (title = "Dates", zeroline = FALSE), 
                   yaxis = list(title = "Price"))
    })
}

shinyApp(ui, server)
