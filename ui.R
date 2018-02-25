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
        helpText("Summary: The summary of the global CO2 Emission Data"),
        helpText("Country Source Breakdown: The sources of CO2 from one country"),
        helpText("Map: The global Co2 Emission Data"),
        h3("CO2 Emission in Year "),
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
          label="# of Top COuntry(s) to show",
          min = 1,
          max = 2821,
          step=1, value =10
        )
             
      )

    ),
    
    mainPanel(
      tabsetPanel(id = "mainPanel",
                  type = "tabs",
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
