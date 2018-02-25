library(shiny)
source("main.R")
library(shinyjs)
library(plotly)


ui <- fluidPage(

  titlePanel("Global CO2 Emission Data"),
  
  sidebarLayout(
    
    sidebarPanel(
      useShinyjs(),
      fluidRow(
        
        h3("Control Panel"),
        sliderInput("year", 
                         label = "Year",
                         min = 1998, max = 2014, value = 1998,
                         animate=TRUE),
        
        selectInput(
          inputId = "country",
          label = "Enter one Country :",
          choices = getCountrynames()
        ),
        selectInput(
          inputId = "series", 
          label = "Enter one Serie of Data :",
          choices = emission.def[seq(1,11),"Indicator.Name"]
        ),
        
        numericInput(
          inputId = "top",
          label="# of Top Country(s) to show",
          min = 1,
          max = 2821,
          step=1, value =10
        )
             
      )

    ),
    
    mainPanel(
      tabsetPanel(id = "mainPanel",
                  type = "tabs",
                  tabPanel("Introduction", htmlOutput("intro")),
                  tabPanel("Trend",plotlyOutput("trend")),
                  tabPanel("Individual Country CO2 Emission Source Breakdown",
                           plotOutput("country"),
                           tableOutput("country.table")
                           ),
                  tabPanel("Map", 
                           plotOutput("world.map",click = "map.click",hover = "map.hover"),
                           verbatimTextOutput('info')),
                  tabPanel("Top Emission Country",
                           dataTableOutput("rank.table"))
      )
      
    )
  )
)
