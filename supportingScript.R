# Supoprting Script
library(RColorBrewer)
library(data.table)
library(stringr)
library(DT)
# require(propagate,lib.loc = "/srv/R/x86_64-pc-linux-gnu-library/3.2")
require(lubridate)
library(xts)
library(dygraphs)


downloadPath <- "./data/"

findLoc <- function(x) {
  tempLoc <- str_split(x, pattern = ":")
  x.out  <- as.numeric(tempLoc[[1]][1]) + as.numeric(tempLoc[[1]][2])/60 + as.numeric(tempLoc[[1]][3])/3600
  return(x.out)
}

appFunc <- function(country){

  mapping <- fread("./app-data/Mapping.csv")

  mapping <- mapping[Country==country]
  mapping[,STANAME:= gsub(" ","", STANAME)]
  
  
  
  tempData <- data.table(readRDS(paste0("app-data/",unique(mapping[Country==country,CN]),".RDS")))
  
  
  tempData[,DATE:=as.Date(as.character(DATE), format("%Y%m%d"))]  
  tempData[,Year:=year(DATE)]
  
  
  #   tempDataMap <- tempData[,list(`Total Records`=.N,`Missing Values`=length(which(RR==-9999))), by=c("STAID", "Year")]
  #   tempDataMap <- tempDataMap[,`% of Missing Values`:= 100*`Missing Values`/`Total Records`]
  tempAnnualRain <- tempData[RR!= -9999, list(`Annual Rainfall`= sum(RR)), by=c("STAID","Year")]
  countryMap <- tempAnnualRain[,list(`Starting Year`=min(Year), `Ending Year`=max(Year), `Mean Annual Rainfall`=mean(`Annual Rainfall`)), by=STAID]
  
  countryMap <-merge(countryMap,mapping[,c("STAID","Country","latUpd","longUpd","STANAME"), with=F], by=c("STAID"))
  
  
  countryMap[,Descr:=paste0("Station: ", STANAME," - Mean Annual Rainfall:", round(`Mean Annual Rainfall`,1)," mm")]
  
  
  outline <- countryMap[chull(countryMap$longUpd, countryMap$latUpd),]
  
  countrymappping  <- leaflet(countryMap) %>%
    
    addTiles(group = "OSM (default)") %>%
    addProviderTiles("Stamen.Toner", group = "Toner") %>%
    addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
    
    # Overlay groups
    addCircles(~longUpd, ~latUpd, ~`Mean Annual Rainfall`, stroke = F, group = "Mean Annual Rainfall", color="red") %>%
    addPolygons(data = outline, lng = ~longUpd, lat = ~latUpd,
                fill = F, weight = 3, color = "#FF7575", group = "Outline") %>%
    
    addMarkers(~longUpd, ~latUpd,  popup = ~htmlEscape(Descr)) %>%
    
    addLayersControl(
      baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
      overlayGroups = c("Mean Annual Rainfall", "Outline"),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  return(countrymappping)  
  
}


plotAllAvailableStations <- function(){
  
  mapping <- fread("./app-data/Mapping.csv")
  mapping[,Descr:=paste0("Country: ",Country,", Station: ", STANAME)]
  
  stationsMap <- leaflet(data = mapping) %>% addTiles() %>%
    addMarkers(~longUpd, ~latUpd,  popup = ~htmlEscape(Descr))
  
  return(stationsMap)
}

findMinimumMaximumYear <- function(country, station){
  options(warn=-2)
  mapping <- fread("./app-data/Mapping.csv")
  mapping[,STANAME:=gsub(" ","", STANAME)]
  
  
  if (length(unique(mapping[Country==country,CN]))>0){
    
    tempData <- data.table(readRDS(paste0("./app-data/",unique(mapping[Country==country,CN]),".RDS")))
    
    station_id <- mapping[STANAME==station,STAID]
    
    tempData <- tempData[STAID == station_id]
    tempData[,DATE:=as.Date(as.character(DATE), format("%Y%m%d"))]  
    tempData[,Year:=year(DATE)]
    tempData <- tempData[RR!=-9999]
    tempData[,Period:=as.yearmon(DATE)]
    
    MonthlyRainfall <- tempData[,list(`Monthly Rainfall`=sum(RR)), by=c("Period")]
    
    if (nrow(MonthlyRainfall)>0) {
      
      monthlyPlot <- dygraph(ts(MonthlyRainfall$`Monthly Rainfall`, frequency = 12, start= year(min(MonthlyRainfall$Period))), main = paste0("Station: ",station," - Monthly Rainfall Amount")) %>%  dyRangeSelector()
      
      
    } else {
      
      MonthlyRainfall<-data.table(Period=as.yearmon(1955:2010), `Monthly Rainfall`=0)
      monthlyPlot <- dygraph(ts(MonthlyRainfall$`Monthly Rainfall`, frequency = 12, start= year(min(MonthlyRainfall$Period))), main = paste0("Station: ",station," - Monthly Rainfall Amount")) %>%  dyRangeSelector()
      
    }
    
    
  } else {
    
    MonthlyRainfall<-data.table(Period=as.yearmon(1955:2010), `Monthly Rainfall`=0)
    monthlyPlot <- dygraph(ts(MonthlyRainfall$`Monthly Rainfall`, frequency = 12, start= year(min(MonthlyRainfall$Period))), main = paste0("Station: ",station," - Monthly Rainfall Amount")) %>%  dyRangeSelector()
    tempData <- data.table(Year=1955:2010)
  }
  
  return(list(min(tempData$Year), max(tempData$Year),monthlyPlot))
}


pettitt <- function(x, alpha) {
  # rank and rank_cumsum
  xtmp <- cbind(x, rank(x[,2]))
  xtmp <- cbind(xtmp, cumsum(xtmp[,3]))
  xtmp <- cbind(xtmp, 1:nrow(xtmp))
  # Xk (test statistic)
  xtmp <- cbind(xtmp, 2 * xtmp[,4] - xtmp[,5] * (nrow(xtmp) + 1))
  colnames(xtmp) <- c("YYYY", "VALUE", "rank", "rank_cumsum", "k", "Xk")
  # breakpoint
  XE  <- max(abs(xtmp[,'Xk']))
  XEa <- xtmp[which(abs(xtmp[,'Xk'])==XE), 'YYYY']
  # p-value
  n    <- nrow(xtmp)
  pval <- exp( (-6 * (XE^2)) / (n^2 + n^3) )
  # significance level (global for all series with same n)
  Xksign <- sqrt( -1/6 * ((log(0.05, base = exp(1))) * (n^2 + n^3)) )
  significant <- XE > Xksign
  
  res <- list(Xk=xtmp,XEa=XEa,XE=XE,Xksign=Xksign,sig=significant,pval=pval)
  return(res)
}




changePointTest <- function(station, minYear, maxYear, alpha){
  
  mapping <- fread("./app-data/Mapping.csv")
  mapping[,STANAME:=gsub(" ","", STANAME)]
  
  country_id <- unique(mapping[STANAME==station, CN]) 
  station_id <- min(mapping[STANAME==station, STAID])
  
  if (length(country_id)>0){
    
    tempData <- data.table(readRDS(paste0("app-data/",country_id,".RDS")))
    tempData[,DATE:=as.Date(as.character(DATE), format("%Y%m%d"))]  
    tempData <- tempData[STAID==station_id]
    tempData[,Year:=year(DATE)]    
    tempData <- tempData[Year >= minYear & Year <= maxYear]
    AnnualRainfall <- tempData[RR!=-9999, list(`Annual Rainfall`=sum(RR)), by=c("Year")]
    
    if (nrow(AnnualRainfall)>= 10) {
      res <- pettitt(data.frame(AnnualRainfall,stringsAsFactors = F), alpha)
      
      if (res$pval<=alpha) { status <- "Significant"} else { status <- "Non-Significant"}
      
      changePoint <- as.integer(min(res$XEa))
      
      if (length(AnnualRainfall$`Annual Rainfall`)>0){
        
        rainfallPlot<- dygraph(ts(AnnualRainfall$`Annual Rainfall`, frequency = 1, start= min(AnnualRainfall$Year)), ylab="Annual Rainfall (mm)", xlab="Year",
                               main = paste0("Pettitt Change Point Detection Test in Annual Rainfall Amount")) %>%  dyRangeSelector() %>%
          dyEvent(date = paste0(changePoint,"-01-01"), paste0(status," Change Point"), labelLoc = "bottom") %>%
          dyOptions(colors = RColorBrewer::brewer.pal(5, "Set2"))
        
      } else {
        
        rainfallPlot <- NULL
        
      }
      
    } else {
      
      if (length(AnnualRainfall$`Annual Rainfall`)>0){
        
        
        rainfallPlot <- dygraph(ts(AnnualRainfall$`Annual Rainfall`, frequency = 1, start= min(AnnualRainfall$Year)), main = paste0("Station: ",station," - Monthly Rainfall Amount")) %>%  dyRangeSelector()
        
      } else {
        
        rainfallPlot <- NULL
        
      }
      
    }
    
    
  }
  
  return(rainfallPlot)
}







