# server.R
library(zoo)

library(shiny)
library(data.table)
library(stringr)
library(lubridate)
library(googleVis)
library(leaflet)
library(htmltools)
# library(rCharts, lib.loc = "/srv/R/x86_64-pc-linux-gnu-library/3.2")
library(dygraphs)
library(yaml)
library(xts)

source('./supportingScript.R')

mapping <- fread("app-data/Mapping.csv")
mapping[,STANAME:=gsub(" ","", STANAME)]
countries <- sort(unique(mapping$Country))

shinyServer(function(input, output, session) {
  
  output$country <- renderUI({selectInput("country", h4("Select Country:"), as.list(countries),selected = "Greece")})
  
  output$station <- renderUI({selectInput("station", h4("Select Station:"), as.list(mapping[Country==input$country,STANAME]))})
  
  output$allcountries <- renderLeaflet({plotAllAvailableStations()})
  
  output$timePeriod <- renderUI({
    
    timeValue <- findMinimumMaximumYear(country = input$country, station = input$station)
    
    sliderInput("timeperiod", label = h4("Select Time Period (in years) of selected station:"), min = timeValue[[1]], 
                max = timeValue[[2]], value = c(timeValue[[1]], timeValue[[2]]))
    
  })
  
  output$monthlyPlot <- renderDygraph({
    
    timeValue <- findMinimumMaximumYear(country = input$country, station = input$station)
    timeValue[[3]]
    
  })
  
  output$countryPlot <- renderLeaflet({
    
    if (!is.null(input$country)){
      mapOfCountry <- appFunc(country = input$country)
      mapOfCountry
    } else {
      
      mapOfCountry<-NULL
      
    }
  })
  
  
  output$changePoint <- renderLeaflet({
    
    #     sliderValues <- reactive({
    #       
    #       # Compose data frame
    #       data.frame(
    #         Name = c("timePeriod"),
    #         Value = as.character(c(paste(input$range, collapse=' '))), 
    #         stringsAsFactors=FALSE)
    #     }) 
    #     
    
    
    cpTest <- changePointTest(station=input$station, minYear= input$timeperiod[1], maxYear=input$timeperiod[2], alpha=input$checkGroup)
    cpTest
    
    
  })
  
  
})  

