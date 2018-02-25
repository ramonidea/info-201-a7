library(shiny)
source("main.R")
library(shinyjs)
library(knitr)
library(plotly)
source("spatial_utils.R")



server <- function(input, output, session) {
  
  tabSwitch <- function(tab){
    if(tab == "Trend"){
      shinyjs::show("country")
      updateSelectInput(session,"country", choices = c("World",getCountrynames()), selected = input$country)
      shinyjs::hide("year")
      shinyjs::show("series")
      shinyjs::hide("top")
    }else if(tab == "Individual Country CO2 Emission Source Breakdown"){
      shinyjs::show("country")
      shinyjs::show("year")
      shinyjs::hide("series")
      shinyjs::hide("top")
      updateSelectInput(session,"country", choices = getCountrynames(), selected = input$country)
    }else if(tab =="Map" ){
      shinyjs::hide('country')
      shinyjs::show("year")
      shinyjs::show("series")
      shinyjs::hide("top")
    }else {
      #Top Emission Country
      shinyjs::show("year")
      shinyjs::hide('country')
      shinyjs::show("series")
      shinyjs::show("top")
    }
  }
  
  output$trend <- renderPlotly({
    tabSwitch(input$mainPanel)
    getTrend(input$country, input$series)
    
  })

  
  output$country <- renderPlot({
    tabSwitch(input$mainPanel)
    validate(
      need(input$country != "","Please select one country.")
    )
    getYearPlot(input$year ,input$country)
  })
  
  output$country.table <-renderTable({
    validate(
      need(input$country != "","Please select one country.")
    )
    getYearTable(input$year,input$country)
  })
  
  output$world.map <- renderPlot({
    tabSwitch(input$mainPanel)
    global_emission_map(input$year, input$series)
  })
  
  observeEvent(input$map.click,{
    country <- GetCountryAtPoint(input$map.hover$x,input$map.hover$y)
    if(!is.na(country)){
      updateSelectInput(session,"country", selected = country)
      updateTabsetPanel(session, "mainPanel", selected = "Trend")
    }
  })
  
  output$info <- renderText({
    country <- GetCountryAtPoint(input$map.hover$x,input$map.hover$y)
    if(!is.na(country)){
      return(paste(paste0("Country Name: ", country),
            paste0('Data Name: ', input$series),
            paste0('Value: ',getSpecificValue(country, input$year, input$series))
      ,sep="\n")
      )
    }else{
      return("")
    }
    })
  
  output$rank.table<- renderDataTable({
    tabSwitch(input$mainPanel)
    validate({
      need(input$top >0 && input$top <= 2821, "Please input a valid number")
    })
    filterCountryTable(input$year, input$series, input$top)
  })
}
