library(dplyr)
library(tidyr)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
#install.packages("plotly")
library(plotly)
library(reshape2)

#get data
emission.data <-
  read.csv("./data/WDI_emissions_Data.csv", fileEncoding = "UTF-8-BOM")
emission.def <-
  read.csv("./data/WDI_emissions_Definition and Source.csv",
           fileEncoding = "UTF-8-BOM")
emission.data <- emission.data[seq(1,2821),]

#Data clean on definition
emission.def <- emission.def %>%
  select(Code, Indicator.Name) 

intro <-
  "This data set is from The World Bank data base. CO2 emissions (kt) Carbon dioxide emissions
are those stemming from the burning of fossil fuels and the manufacture of cement. They include carbon
dioxide produced during consumption of solid, liquid, and gas fuels and gas flaring."

getSpecificValue <- function(country, year, series) {
  result <- emission.data %>%
    filter(
      Country.Code == iso.alpha(country, 3),
      as.character(Series.Code) == as.character(emission.def[emission.def$"Indicator.Name" == series,]$Code)
    ) %>%
    select(paste0("YR", year))
  return(result)
}

getFullName <- function(names) {
  result = c()
  for (i in 1:nrow(names)) {
    name <-
      as.character((
        filter(emission.def, as.character(Code) == as.character(names[[1]][i]))$Indicator.Name
      ))
    result <-
      c(result, name)
  }
  return(result)
}

#Add Global Total Data row
global.data <- emission.data %>%
  na.omit() %>%
  select(-Country.Code, -Most_Recent) %>%
  group_by(Series.Code) %>%
  summarise_all(sum)
source.labels <- global.data["Series.Code"]
source.labels <- getFullName(source.labels)
global.data$Series.Code <- source.labels
rownames(global.data) <- global.data$Series.Code
global.data <- select(global.data,-Series.Code)

getTrend <- function(country, series) {
  if (country == "World") {
    data.result <- global.data[series, ]
  } else{
    data.result <-
      filter(emission.data, Country.Code == iso.alpha(country, 3)) %>% 
      na.omit()
    source.labels <- data.result["Series.Code"]
    source.labels <- getFullName(source.labels)
    data.result$Series.Code <- source.labels
    data.result <- filter(data.result, Series.Code == series)
  }
  if(nrow(data.result)>0){
    data.result <- melt(data.result[, 2:ncol(data.result)])
    gg <- ggplot(data.result, aes(x = variable, y = value)) +
      geom_line(group = 1,
                size = 2,
                color = 'Dark Blue') +
      ggtitle(paste0(series, ' Trend in ', country)) +
      theme(axis.title.y = element_blank(),
            axis.text.y = element_blank())
    return(gg)
  }else{
    return(ggplot()+ggtitle("No Enough Data to Show"))
  }
}



getCountrynames <- function() {
  country.names <-
    emission.data %>%
    select(Country.Code, Series.Code, YR1998) %>%
    filter(Series.Code == "EN.ATM.CO2E.KT") %>%
    na.omit() %>%
    select(Country.Code)
  country.names$code <-
    iso.expand(as.character(country.names$Country.Code))
  #Fix special country name
  country.names$code[country.names$code == "(^Barbuda)|(^Antigua)"] <-
    'Barbuda'
  country.names$code[country.names$code == "(^China(?!:Hong Kong|:Macao))|(^Paracel Islands)"] <-
    "China"
  country.names$code[country.names$code == "Finland(?!:Aland)"] <-
    "Finland"
  country.names$code[country.names$code == "(^France)|(^Clipperton Island)"] <-
    "France"
  country.names$code[country.names$code == "Norway(?!:Bouvet|:Svalbard|:Jan Mayen)"] <-
    "Norway"
  country.names$code[country.names$code == "(^Portugal)|(^Azores)|(^Madeira Islands)"] <-
    "Portugal"
  country.names$code[country.names$code == "(^Spain)|(^Canary Islands)"] <-
    "Spain"
  country.names$code[country.names$code == "(^Saint Kitts)|(^Nevis)"] <-
    "Saint Kitts"
  country.names$code[country.names$code == "(^Saint Vincent)|(^Grenadines)"] <-
    "Saint Vincent"
  country.names$code[country.names$code == "(^Trinidad)|(^Tobago)"] <-
    "Trinidad"
  return(country.names$code)
}


get_year_emission <- function(year, country) {
  emission.data <-
    emission.data %>%
    na.omit() %>%
    filter(Country.Code == iso.alpha(country, n = 3)) %>%
    select(Country.Code, year, Series.Code) %>%
    spread(Country.Code, year) %>%
    filter(
      Series.Code %in% c(
        "EN.CO2.TRAN.ZS",
        "EN.ATM.CO2E.SF.ZS",
        "EN.CO2.BLDG.ZS",
        "EN.CO2.OTHX.ZS",
        "EN.CO2.MANF.ZS",
        "EN.ATM.CO2E.LF.ZS",
        "EN.ATM.CO2E.GF.ZS",
        "EN.CO2.ETOT.ZS"
      )
    ) %>%
    arrange_(iso.alpha(country, n = 3))
  emission.data$Series.Code <-
    factor(emission.data$Series.Code, levels = emission.data$Series.Code)
  return(emission.data)
}

getName <- function(names) {
  result = c()
  for (i in 1:nrow(names)) {
    name <-
      as.character((
        filter(emission.def, as.character(Code) == as.character(names[[1]][i]))$Indicator.Name
      ))
    name <-
      strsplit(strsplit(name, "CO2 emissions ")[[1]][2], "\\(")[[1]][1]
    result <-
      c(result, name)
  }
  return(result)
}


getYearPlot <- function(year, country) {
  num.year <- paste0("YR", year)
  country.data <- get_year_emission(num.year, country)
  code <- iso.alpha(country, n = 3)
  source.labels <- country.data["Series.Code"]
  source.labels <- getName(source.labels)
  
  plot <-
    ggplot(data = country.data, aes_string(x = factor(1), y = code, fill = "Series.Code")) +
    theme_light(base_size = 15, base_family = "") +
    geom_bar(
      ,
      width = 1,
      position = "stack",
      stat = "identity",
      color = "black"
    ) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    scale_fill_brewer(palette = "Blues",
                      name = "Source",
                      labels = source.labels) +
    theme(
      legend.position = "bottom",
      legend.direction = "vertical",
      legend.text = element_text(size = 12)
    ) +
    ggtitle(paste0(country, " CO2 Emission Source Distribution")) +
    ylab("% of total Emission") +
    xlab("Sources") +
    coord_polar("y", start = 0)
  return(plot)
}

getYearTable <- function(year, country) {
  num.year <- paste0("YR", year)
  country.data <- get_year_emission(num.year, country)
  code <- iso.alpha(country, n = 3)
  source.labels <- country.data["Series.Code"]
  source.labels <- getName(source.labels)
  country.data$Series.Code <- source.labels
  colnames(country.data) <- c('Source', "% of the total emission")
  return(country.data)
}

#Global CO2 Emission Data
global_emission_map <- function(year, series) {
  year <- paste0("YR", year)
  
  emission.data$Series.Code <- getFullName(emission.data["Series.Code"])
  
  global.emission <-
    emission.data %>%
    na.omit() %>% 
    filter(Series.Code == series) %>%
    select(Country.Code, year)
  
  world <- map_data("world")
  world$Country.Code <- iso.alpha(world$region, n = 3)
  global.data <- inner_join(world, global.emission, by = "Country.Code")
  global.data <- mutate(global.data, year = cut(global.data[, year], 5))
  
  gg <- ggplot(data = global.data) +
    geom_map(
      map = global.data,
      aes(x = long, y = lat, map_id = region),
      color = "dark gray",
      fill = "gray",
      size = 0.05
    ) +
    geom_map(map = global.data,
             aes(map_id = region, fill = year)) +
    theme(
      legend.position = "right",
      legend.direction = "vertical",
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    scale_fill_brewer(palette = "Blues", name = "CO2 Emission kt (Millions)") +
    theme_dark() +
    ggtitle(paste0("World ", series, "in year", year))
  gg
  return(gg)
  
}


filterCountryTable <- function(year, series, top){
  year <- paste0("YR", year)
  year.sym <- rlang::sym(year)
  result.data <- 
    emission.data %>% 
    filter(Series.Code == as.character(emission.def[emission.def$Indicator.Name == series,]$Code)) %>% 
    select(Country.Code, year) %>% 
    arrange(desc(!!year.sym)) %>% 
    top_n(top) %>% 
    mutate(Country.Code = iso.expand(as.character(Country.Code)))
  return (result.data)
}
