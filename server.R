
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(quantmod)

source("~/Hello//helpers.R")

shinyServer(function(input, output) {

  dataInput<-reactive({
    print("Hi")
    tryCatch({
      return(finalData(input$getSymbol))
    },error = function(e){
      getData()
      return(finalData(input$getSymbol))
    })
  })
  output$plot<- renderPlot({
    data<-dataInput()
    tryCatch({
      chartSeries(data,name=input$getSymbol,theme="white",TA = input$tis)
    },error = function(e){
      chartSeries(data, name = input$getSymbol, theme = "white")
      switch(input$tis,
        CMF = addTA(CMF(HLC(data),Vo(data)),on =1),
        ADX = addTA(ADX(HLC(data)), on =1),
        ATR = addTA(ATR(HLC(data)), on =1),
        CCI = addTA(CCI(HLC(data)), on =1),
        #MACD = addTA(MACD(Cl(data),12,26,9), on =1),
        ROC = addTA(ROC(Cl(data)), on =1),
        Momentum = addTA(momentum(Cl(data)), on =1),
        RSI = addTA(RSI(Cl(data)), on =1),
        Stoch = addTA(stoch(HLC(Data)), on =1),
        TRIX = addTA(TRIX(Cl(data)), on =1),
        WPR = addTA(WPR(HLC(data)), on =1)
      )
    })
  })
  
  canBuy<-reactive({
    if(buy(input$buySymbol)){
      return("Buy")
      message("Buy")
    }
    else{
      return("Hold")
      message("Hold")
    }
    
  })
  output$buyit<-renderText({
    canBuy()
  })
  
  
})
