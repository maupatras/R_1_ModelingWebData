# R_1_ModelingWebData

The present Shiny application works as a Rainfall tool that aims to present, analyze and visualize rainfall records from 23 meteorological stations. Initial data have been provided by www.ecad.eu and have been preprocessed accordingly; the final format can be found in folder app-data. The input information can be manipulated accordingly in order to analyze stations of interest. There is an available link of the application at: http://83.212.112.48/rainfall-app/.

The rainfall tracker application consists of 4 main tabs:

1) Country Map: A leaflet visualization of the selected country. Countries and stations can be selected in the relevant widgets at the left side. The map represents each available meteorological station and the mean annual rainfall amount of all the available rainfall records.

2) Monthly Rainfall: The monthly rainfall amount of each meteorological station represented in a time-series chart. The period of interest can be handled interactively by adjusting the range of the area under the x-axis.

3) Change Point Detection: The non paraetric Pettitt Change Point Detection test has been applied in Annual Rainfall Series in order to identify possible "changes" in the annual rainfall during the time. More information about the methodology can be found in https://cran.r-project.org/web/packages/trend/vignettes/trend.pdf (page 7). Two widgets that work for this tab are  "Select Time Period (in years) of selected station:" which actually adjust the  time period of each station and "Sig. Level Change-Point Test" that modifies the p-value of the test.

4) Stations Location - Entire Database: In this tab there is available information about all the available meteorological stations. 
