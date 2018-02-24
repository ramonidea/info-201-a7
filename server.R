library(shiny)
source("main.R")
library(shinyjs)
library(knitr)
library(plotly)
# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$trend <- renderPlotly({
    enable("country")
    disable("year")
    show("series")
    getTrend(input$country, input$series)
    
  })

  
  output$country <- renderPlot({
    enable("country")
    enable("year")
    hide("series")
    validate(
      need(!is.null(input$country), "Please select a Country")
    )
    getYearPlot(input$year ,input$country)
  })
  
  output$country.table <-renderTable({
    enable("country")
    enable("year")
    hide("series")
    validate(
      need(!is.null(input$country), "Please select a Country")
    )
    getYearTable(input$year,input$country)
  })
  
  output$world.map <- renderPlot({
    disable('country')
    enable("year")
    hide("series")
    global_emission_map(input$year)
  })
  
  output$info <- renderPrint({
        return(input$map.click)
    })
  
  output$country.info <- renderPrint({
    return(input$country.hover)
  })
  
  
  
}
