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
          choices = emission.def[,"Indicator.Name"]
        )
        
             
      )

    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(id = "mainPanel",
                  type = "tabs",
                  tabPanel("Trend",plotlyOutput("trend")),
                  tabPanel("Country Source Breakdown Plot",
                           plotOutput("country", click = "country.hover"),
                           verbatimTextOutput('country.info')
                           ),
                  tabPanel("Country Source Breakdown Table", tableOutput("country.table")),
                  tabPanel("Map", 
                           plotOutput("world.map",click = "map.click"),
                           verbatimTextOutput('info'))
      )
      
    )
  )
)
