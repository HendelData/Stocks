library(plotly)
library(shiny)
library(ggplot2)
library(scales)
library(lubridate)
library(quantmod)

#FUNCTION TO PULL STOCK DATA FROM YAHOO
stockData <- function(symbol, start_dt, caret) {
  if (caret==1) {symbol_use <- paste0("^",symbol)}
  getSymbols(symbol_use, src="yahoo", from=start_dt, to=Sys.Date())
  df <- as.data.frame(eval(parse(text=symbol)))
  df$Date <- as.Date(rownames(df))
  rownames(df) <- seq(1:nrow(df))
  df <- df[ ,c(7,1:4)]
  colnames(df) <- c("Date","Open","High","Low","Close")
  return(df)
}

#GET DOW JONES DATA
dji <- stockData("DJI", "1992-01-03", 1)

#GET S&P DATA
sp <- stockData("GSPC", "1992-01-03", 1)

#GET NASDAQ DATA
nasdaq <- stockData("IXIC", "1992-01-03", 1)

#COLORS FOR OHLC CHARTS
i <- list(line=list(color='green3'))
d <- list(line=list(color='red3'))

shinyApp(
  #UI
  ui <- shinyUI(fluidPage(
    sidebarLayout(sidebarPanel(dateRangeInput(inputId="DateRange",
                                              label="Select start and end dates:",
                                              start=as.Date('1992-01-02'),
                                              end=as.Date('2023-10-30'))),
                  
                  #TABS
                  mainPanel(tabsetPanel(type="tabs", id="tabpanel",
                                        tabPanel("Closing Price",
                                                 value="closing",
                                                 br(),
                                                 plotlyOutput("dowjones", height=200),
                                                 br(),
                                                 plotlyOutput("sandp", height=200),
                                                 br(),
                                                 plotlyOutput("nasdaq", height=200)),
                                        
                                        tabPanel("Open-High-Low-Close",
                                                 value="ohlc",
                                                 br(),
                                                 plotlyOutput("dowjonesOHLC", height=200),
                                                 br(),
                                                 plotlyOutput("sandpOHLC", height=200),
                                                 br(),
                                                 plotlyOutput("nasdaqOHLC", height=200)),
                                        
                                        tabPanel("Yesterday",
                                                 value="prevday",
                                                 fluidRow(column(4, 
                                                                 br(),
                                                                 htmlOutput("dj_text1"),
                                                                 br(),
                                                                 htmlOutput(("dj_text2")),
                                                                 br(),
                                                                 uiOutput("dj_text3")),
                                                          
                                                          column(4, 
                                                                 br(),
                                                                 htmlOutput("sp_text1"),
                                                                 br(),
                                                                 htmlOutput(("sp_text2")),
                                                                 br(),
                                                                 uiOutput("sp_text3")),
                                                          
                                                          column(4, 
                                                                 br(),
                                                                 htmlOutput("nd_text1"),
                                                                 br(),
                                                                 htmlOutput("nd_text2"),
                                                                 br(),
                                                                 uiOutput("nd_text3"))
                                                           ) #END FLUID ROW
                                                 ) #END TAB PANEL
                  ))
    )
  )),
  
  #SERVER
  server <- shinyServer(
    function(input, output, session) {
      
      #IF OHLC TAB IS SELECTED, CHANGE DATE RANGE TO CURRENT YEAR
      observe({
        if (input$tabpanel=='ohlc') 
        {updateDateRangeInput(session, "DateRange", 
                              start=paste0(year(Sys.Date()), "-01-01"), 
                              end=paste0(year(Sys.Date()), "-12-31"))}
      })
      
      
      #STOP SESSION ON EXIT
      session$onSessionEnded(function() {
        stopApp()
      })
      
      #TEXT FOR YESTERDAY'S INFORMATION TAB
      output$dj_text1 <- renderText({
        paste0("Dow Jones ", "<font color=\"darkgoldenrod\"><b>Open </font></b>", "on ", dji$Date[nrow(dji)], ": <b>",
               paste0("$", formatC(dji$Open[nrow(dji)], big.mark=",", format="f", digits=2)), "</b>")
      })
      
      output$dj_text2 <- renderText({
        paste0("Dow Jones ", "<font color=\"darkgoldenrod\"><b>Close </font></b>", "on ", dji$Date[nrow(dji)], ": <b>",
               paste0("$", formatC(dji$Close[nrow(dji)], big.mark=",", format="f", digits=2)), "</b>")
      })
      
      output$dj_text3 <- renderUI({
        change <- dji$Close[nrow(dji)] - dji$Open[nrow(dji)]
        if(change > 0){
          HTML(paste0("Change: <font color=\"green\">\u25B2</font><b>", 
                      paste0("$", formatC(change, big.mark=",", format="f", digits=2))), "</b>")
        } else {HTML(paste0("Change: <font color=\"red\">\u25BC</font><b>", 
                            paste0("$", formatC(-change, big.mark=",", format="f", digits=2))), "</b>")}
      })
      
      
      output$sp_text1 <- renderText({
        paste0("S & P 500 ", "<font color=\"darkgoldenrod\"><b>Open </font></b>", "on ", dji$Date[nrow(sp)], ": <b>",
               paste0("$", formatC(sp$Open[nrow(sp)], big.mark=",", format="f", digits=2)), "</b>")
      })
      
      output$sp_text2 <- renderText({
        paste0("S & P 500 ", "<font color=\"darkgoldenrod\"><b>Close </font></b>", "on ", dji$Date[nrow(sp)], ": <b>",
               paste0("$", formatC(sp$Close[nrow(sp)], big.mark=",", format="f", digits=2)), "</b>")
      })
      
      output$sp_text3 <- renderUI({
        change <- sp$Close[nrow(sp)] - sp$Open[nrow(sp)]
        if(change > 0){
          HTML(paste0("Change: <font color=\"green\">\u25B2</font><b>", 
                      paste0("$", formatC(change, big.mark=",", format="f", digits=2))), "</b>")
        } else {HTML(paste0("Change: <font color=\"red\">\u25BC</font><b>", 
                            paste0("$", formatC(-change, big.mark=",", format="f", digits=2))), "</b>")}
      })
      
      
      output$nd_text1 <- renderText({
        paste0("NASDAQ ", "<font color=\"darkgoldenrod\"><b>Open </font></b>", "on ", nasdaq$Date[nrow(nasdaq)], ": <b>",
               paste0("$", formatC(nasdaq$Open[nrow(nasdaq)], big.mark=",", format="f", digits=2)), "</b>")
      })
      
      output$nd_text2 <- renderText({
        paste0("NASDAQ ", "<font color=\"darkgoldenrod\"><b>Close </font></b>", "on ", nasdaq$Date[nrow(nasdaq)], ": <b>",
               paste0("$", formatC(nasdaq$Close[nrow(nasdaq)], big.mark=",", format="f", digits=2)), "</b>")
      })
      
      output$nd_text3 <- renderUI({
        change <- nasdaq$Close[nrow(nasdaq)] - nasdaq$Open[nrow(nasdaq)]
        if(change > 0){
          HTML(paste0("Change: <font color=\"green\">\u25B2</font><b>", 
                      paste0("$", formatC(change, big.mark=",", format="f", digits=2))), "</b>")
        } else {HTML(paste0("Change: <font color=\"red\">\u25BC</font><b>", 
                            paste0("$", formatC(-change, big.mark=",", format="f", digits=2))), "</b>")}
      })
      
      
      #LINE GRAPHS OF CLOSING PRICE
      output$dowjones <- renderPlotly({
        djiplot <- reactive({dji[dji$Date>=input$DateRange[1] & dji$Date<=input$DateRange[2],]})
        padding <- 0.02*(input$DateRange[2] - input$DateRange[1])
        
        p <- plot_ly(data=djiplot(), x=~Date, y=~Close,
                     type='scatter', mode='lines', 
                     fill="tozeroy", fillcolor=rgb(0.25,0.41,0.88,0.25),
                     line=list(color="navy", width=1))
        
        p <- p %>% layout(title=list(text="<b>Dow Jones Industrial Average</b>", x=0.09, 
                                     font=list(size=16)),
                          xaxis=list(title="", showgrid=FALSE, range=c(input$DateRange[1]-padding, input$DateRange[2]+padding)),
                          yaxis=list(title="", tickformat=",.0f"))
        
        ggplotly(p)
      })
      
      output$sandp <- renderPlotly({
        spplot <- reactive({sp[sp$Date>=input$DateRange[1] & sp$Date<=input$DateRange[2],]})
        padding <- 0.02*(input$DateRange[2] - input$DateRange[1])
        p <- plot_ly(data=spplot(), x=~Date, y=~Close,
                     type='scatter', mode='lines', 
                     fill="tozeroy", fillcolor=rgb(0.25,0.41,0.88,0.25),
                     line=list(color="navy", width=1))
        
        p <- p %>% layout(title=list(text="<b>S & P 500</b>", x=0.09, 
                                     font=list(size=16)),
                          xaxis=list(title="", showgrid=FALSE, range=c(input$DateRange[1]-padding, input$DateRange[2]+padding)),
                          yaxis=list(title="", tickformat=",.0f"))
        
        ggplotly(p)
      })
      
      output$nasdaq <- renderPlotly({
        nplot <- reactive({nasdaq[nasdaq$Date>=input$DateRange[1] & nasdaq$Date<=input$DateRange[2],]})
        padding <- 0.02*(input$DateRange[2] - input$DateRange[1])
        p <- plot_ly(data=nplot(), x=~Date, y=~Close,
                     type='scatter', mode='lines', 
                     fill="tozeroy", fillcolor=rgb(0.25,0.41,0.88,0.25),
                     line=list(color="navy", width=1))
        
        p <- p %>% layout(title=list(text="<b>NASDAQ</b>", x=0.09, 
                                     font=list(size=16)),
                          xaxis=list(title="", showgrid=FALSE, range=c(input$DateRange[1]-padding, input$DateRange[2]+padding)),
                          yaxis=list(title="", tickformat=",.0f"))
        
        ggplotly(p)
      })
      
      #OPEN-HIGH-LOW-CLOSE CHARTS
      output$dowjonesOHLC <- renderPlotly(({
        djohlcplot <- reactive({dji[dji$Date>=input$DateRange[1] & dji$Date<=input$DateRange[2],]})
        padding <- 0.02*(input$DateRange[2] - input$DateRange[1])
        fig <- plot_ly(data= djohlcplot(), x=~Date, type="ohlc",
                       open=~djohlcplot()$Open, close=~djohlcplot()$Close,
                       high=~djohlcplot()$High, low=~djohlcplot()$Low,
                       increasing=i, decreasing=d)
        
        fig <- fig %>% layout(title=list(text="<b>Dow Jones Industrial Average OHCL</b>", x=0.05, font=list(size=16)),
                              xaxis=list(rangeslider=list(visible = F), title="", showgrid=FALSE),
                              yaxis=list(title="", tickformat=",.0f"))
        
        fig
      }))
      
      output$sandpOHLC <- renderPlotly(({
        spohlcplot <- reactive({sp[sp$Date>=input$DateRange[1] & sp$Date<=input$DateRange[2],]})
        padding <- 0.02*(input$DateRange[2] - input$DateRange[1])
        fig <- plot_ly(data= spohlcplot(), x=~Date, type="ohlc",
                       open=~spohlcplot()$Open, close=~spohlcplot()$Close,
                       high=~spohlcplot()$High, low=~spohlcplot()$Low,
                       increasing=i, decreasing=d)
        
        fig <- fig %>% layout(title=list(text="<b>S&P 500 OHCL</b>", x=0.05, font=list(size=16)),
                              xaxis=list(rangeslider=list(visible = F), title="", showgrid=FALSE),
                              yaxis=list(title="", tickformat=",.0f"))
        
        fig
      }))
      
      output$nasdaqOHLC <- renderPlotly(({
        nohlcplot <- reactive({nasdaq[nasdaq$Date>=input$DateRange[1] & nasdaq$Date<=input$DateRange[2],]})
        padding <- 0.02*(input$DateRange[2] - input$DateRange[1])
        fig <- plot_ly(data= nohlcplot(), x=~Date, type="ohlc",
                       open=~nohlcplot()$Open, close=~nohlcplot()$Close,
                       high=~nohlcplot()$High, low=~nohlcplot()$Low,
                       increasing=i, decreasing=d)
        
        fig <- fig %>% layout(title=list(text="<b>NASDAQ OHCL</b>", x=0.05, font=list(size=16)),
                              xaxis=list(rangeslider=list(visible = F), title="", showgrid=FALSE),
                              yaxis=list(title="", tickformat=",.0f"))
        
        fig
      }))
      
    })
)

shinyApp(ui, server)