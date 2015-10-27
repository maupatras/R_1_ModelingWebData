# ui.R
library(zoo)
library(shiny)
library(data.table)
library(stringr)
library(lubridate)
library(googleVis)
library(leaflet)
library(htmltools)
library(dygraphs)
library(yaml)
library(xts)

source('./supportingScript.R')

shinyUI(fluidPage(
  
  tags$head(
    tags$style(HTML("@import url('http://bootswatch.com/simplex/bootstrap.css');")),
    tags$title("Rainfal Tracker Application")),
  headerPanel(
    list(img(src="LogoASES.JPG", "Rainfal Tracker Application",height =50)
    )),
  
  sidebarPanel(width=3,
               
               uiOutput("country"),
               uiOutput("station"),
               #                numericInput("integer", 
               #                             label = h4("Input an extreme daily rainfall event in (mm):"), 
               #                             value = 300),
               uiOutput("timePeriod"),
               
               radioButtons("checkGroup", 
                            label = h3("Sig. Level Change-Point Test"), 
                            choices = list("0.01" = 0.01, 
                                           "0.05" = 0.05, "0.10" = 0.1))),  
  
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Country Map", strong("Locations of meteorological stations per selected country."),leafletOutput("countryPlot", height = 700, width = 1100)),
                tabPanel("Monthly Rainfall",strong("Monthly rainfall series"), p(""),p(""),p(""),dygraphOutput("monthlyPlot", height = 600)),
                tabPanel("Change Point Detection", strong("Pettitt change point detection test on annual rainfall series"),p(""),p(""),p(""),dygraphOutput("changePoint", height = 600)),
                tabPanel("Stations Location - Entire Database", strong("Locations of all the available meteorological stations. Tap to discover country and city respectively."),leafletOutput("allcountries", height = 700, width = 1100)),
                tabPanel("User Information", p(""), "The present Shiny Application works as a reporting analytics tool that aims to provide information of a rainfall gauges network of 23 Nations provided by ECAD.",p(""),
                         "1. On the first tab there is available information about the locations of the rain gauges linked to the selected country on the left tab", p(""),
                         "2. On the second tab, there is an interactive tab that presents the monthly rainfall amount (there is an interactive bar on the bottom of the chart that can be adjusted accordingly in order to focus on different time periods).",p(""), "3. In the third tab, the non-parametric Pettitt change point detection test is performed in annual rainfall series in order to identify possible significant change  in the median of the series.", p(""),
                         "4. In the fourth tab there is available information about the locations of the stations of the entire database.")
                
    ))
  
  
  
  
  
  
  
))
