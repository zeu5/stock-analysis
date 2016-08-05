
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
source("helpers.R")
library(shiny)
library(shinythemes)
library(shinydashboard)


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Invest", tabName = "invest", icon = icon("credit-card")),
    menuItem("Statistics", icon = icon("line-chart"), tabName = "statistics")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "invest",
            fluidRow(
              box(title = "Company", solidHeader = TRUE,
                selectInput("buySymbol",label = "Select the company to invest in :",company,selected = company[[1]],size = 10,width = "400px",selectize = FALSE)
              ),
              box(title = "Output", status = "warning", solidHeader = TRUE,
                  textOutput("buyit")
              )
            )
            img("tass.png",height="30%",width="100%)
            
    ),
    
    tabItem(tabName = "statistics",
            fluidRow(
              box(title = "Company", solidHeader = TRUE,status = "primary",
                selectInput("getSymbol",label ="",choices = company,selected = company[[1]],size = 7,width = "400px",selectize = FALSE)
              ),
              box(title = "Technical indicators", solidHeader = TRUE, status = "primary",
                selectInput("tis",choices = tiList,width = '400px',label = "",selected = tiList[1],size = 7,selectize = FALSE)
              )
            ),
            fluidRow(
              tabBox(
                height = "80%", selected = "Plot", width = "100px",
                tabPanel("Plot",plotOutput("plot"))
              )
            )
    )
  )
)

# Put them together into a dashboardPage
dashboardPage(skin = "black",
  dashboardHeader(title = "TASS"),
  sidebar,
  body
)


























# shinyUI(fluidPage(
# 
#   # Application title
#   titlePanel("Show stock data"),
# 
#   # Sidebar with a slider input for number of bins
#   sidebarLayout(
#     sidebarPanel(
#       h3("Select the Stock Symbol"),
#       br(),    
#       selectInput("symbol","Symbol",company)
#     ),
#     
#     # Show a plot of the generated distribution
#     mainPanel(
#       if(textOutput("err")!='') plotOutput("plot") else p("Error Occured fetching data")
#       )
#   )
# ))
