library(shiny)
source("main.R")
library(shinyjs)
library(knitr)
library(plotly)
source("spatial_utils.R")


server <- function(input, output, session) {
  #when switch the tab and reset the control panel.
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
  
  #When Click the map, it will direct to the trend for that country and that serie.
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
  
  output$intro <- renderUI({
    div(
    div(
      h2("CO2 Emission Data Visualization"),
      br(),
      h4(intro),
      br()
    ),
    div(
      h3("Tab list:"),
      h4("Trend : Show one Country/World data in certain year for on Serie of Data."),
      p("You may choose 'World' in the Country list to see the global trend. 
        You may also hover on the data points to see the complete number"),
      h4("Individual Country CO2 Emission Source Breakdown : Source of CO2 breakdown plot and table for one country in one year"),
      p("You may choose the year and the country to show. The output would be the pie chart which indicates the percentage of the CO2 source
        Also, the table shows the detail number and explaination."),
      h4("Map : Global CO2 Emission Map *May take a while to load one map"),
      p("You may choose the year and the serie of data to show on the map.
        You can also hover on the country to preview the data from that country.
        Also, you may click the country to direct you to view the trend data for that country at that year for that serie of data."),
      h4("Top Emission Country : show the top n countries for emission"),
      p("You may choose the serie of data and the number country to show.")
    )
    )
    
    
  })
  
}
