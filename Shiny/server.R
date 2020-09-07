library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)
library(rgdal)
library(raster)
library(plotly)
library(lubridate)
library(scales)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)
library(viridis)
library(shinycssloaders)
library(sf)

source('server_modules.R')
source('functions.R')

#df <- read.csv("Risk_Toilets_Final.csv")


CUA5 <-st_read("Shapefiles/CUA5.shp")
CUA5 <- readOGR("Shapefiles/CUA5.shp",GDAL1_integer64_policy = TRUE)
CUA5 <- spTransform(CUA5, CRS("+proj=longlat +datum=WGS84 +no_defs"))

# CUA5 Population
CUA5_Pop <- readOGR("Shapefiles/POP-CUA5.shp",GDAL1_integer64_policy = TRUE)
CUA5_Pop <- spTransform(CUA5_Pop, CRS("+proj=longlat +datum=WGS84 +no_defs"))

# CUA5 Roads
CUA5_Roads <- readOGR("Shapefiles/CUA5-Roads.shp",GDAL1_integer64_policy = FALSE)
CUA5_Roads <- spTransform(CUA5_Roads, CRS("+proj=longlat +datum=WGS84 +no_defs"))

# CUA5 Rivers
CUA5_Rivers <- readOGR("Shapefiles/CUA5-River.shp",GDAL1_integer64_policy = TRUE)
CUA5_Rivers <- spTransform(CUA5_Rivers, CRS("+proj=longlat +datum=WGS84 +no_defs"))

# CUA5 Risk
CUA5_Risk <- readOGR("Shapefiles/Final_SRI.shp",GDAL1_integer64_policy = TRUE)
CUA5_Risk<- spTransform(CUA5_Risk, CRS("+proj=longlat +datum=WGS84 +no_defs"))


# Server ----
server <- function(input, output, session) {
  # Bounding box filter (If the stats are being used)
  boundingBox <- reactive({
    if(!is.null(input$toilets_bounds)){
      Map <- in_bounding_box(df, df$latitude, df$longitude, input$toilets_bounds)
    } else {
      Map
    }
    
  })
  
  
  # This is from the older version of the App. This will just create 
  # output$toilets <- renderLeaflet({
  #   
  #   icons <- awesomeIcons(
  #     icon = 'bath',
  #     iconColor = '#FFFFFF',
  #     library = 'fa',
  #     markerColor = "darkred"
  #   )
  #   leaflet(data = df) %>%
  #     addProviderTiles(providers$Esri.WorldTopoMap, group = "Street Map (Default)") %>% 
  #     addProviderTiles(providers$Esri.WorldImagery, group = "Satellite Map") %>% 
  #     addAwesomeMarkers(lng = ~longitude,
  #                       lat = ~latitude,
  #                       popup = paste("Capacity:", "<b>", df$capacity, "m³</b><br>",
  #                                     "People using toilet:","<b>", round(df$people_usi, 0), "</b><br>",
  #                                     "Last cleaned (in days):","<bSRI>" ,round(df$last_clean,0), "</b>"),
  #                       popupOptions = popupOptions(closeOnClick = T),
  #                       
  #                       icon = icons) %>% 
  #     addPolylines(data = CUA5, color = "Black", weight = 1, smoothFactor = 0.5,
  #                  opacity = 1.0, fillOpacity = 1, dashArray ="4 2 3",
  #                  fillColor = "Blue")  %>% 
  #     addLayersControl(
  #       baseGroups = c("Street Map (Default)", "Satellite Map"),
  #       position = 'bottomleft',
  #       options = layersControlOptions(collapsed = TRUE)
  #     )
    
    
  #})
  
  #bins <- c(592, 3399, 6111, 10025, 27272, 113693)
  pal_risk <- colorBin("YlOrRd", domain = CUA5_Risk$SRI2, na.color = "#FFFFFF00", reverse = F)
  
  #bins <- c(592, 3399, 6111, 10025, 27272, 113693)
  pal_tpop <- colorBin("YlOrRd", domain = CUA5_Risk$tpop_mean, na.color = "#FFFFFF00", reverse = F)
  
  #bins <- c(592, 3399, 6111, 10025, 27272, 113693)
  pal_density <- colorBin("YlOrRd", domain = CUA5_Risk$HHdensity_, na.color = "#FFFFFF00", reverse = F)
  
  #bins <- c(592, 3399, 6111, 10025, 27272, 113693)
  pal_interfacet <- colorBin("YlOrRd", domain = CUA5_Risk$interfacet, na.color = "#FFFFFF00", reverse = F)
  
  #Labels for the symbology
  labels_1<- c("Low", "", "", "", "Medium", "", "", "", "High")
  
  # bins <- c(0, 30, 65, 104, 334, 2116)
  # pal_pop <- colorBin("PuRd", domain = CUA5_Pop$Pop_Densit, bins = bins)
  # bins <- c(2009, 5972, 7130, 8674, 9913, 12920)
  # pal_road <- colorBin("Oranges", domain = CUA5_Pop$DN, bins = bins)
  # bins <- c(0, 63, 1389, 4275)
  # pal_riv <- colorBin("Blues", domain = CUA5_Pop$DN, bins = bins)
   
  
  output$risk <- renderLeaflet({
    
    leaflet(data = CUA5_Risk) %>% 
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Street Map (Default)") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite Map") %>% 
      addPolylines(data = CUA5, color = "Black", weight = 3, smoothFactor = 0.5,
                   opacity = 1.0, fillOpacity = 1, dashArray ="4 6 2",
                   fillColor = "Blue") %>% 
      addPolygons(fillColor = ~pal_risk(SRI2), color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 0.1, fillOpacity = 0.6,
                  label = HTML_labels(CUA5_Risk$SRI2, text = ""),
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    opacity = 0.7,
                    bringToFront = TRUE)
       ) %>%
       addLegend(title = "Risk", position = "bottomleft", values = ~SRI2,
                pal = pal_risk,
                bins = 7,
                labFormat = function(type, cuts, p) {  # Here's the trick
                paste0(labels_1)}
      ) %>% 
      addLayersControl(
        baseGroups = c("Street Map (Default)", "Satellite Map"),
        position = 'bottomleft',
        options = layersControlOptions(collapsed = TRUE)
      )
  })
 

  #This shows another possible map from Luis' code that I am not currently using. 
  # output$risk_w_pop <- renderLeaflet({
  #   
  #   leaflet(data = CUA5_Pop) %>% 
  #     addProviderTiles(providers$Esri.WorldTopoMap, group = "Street Map (Default)") %>% 
  #     addProviderTiles(providers$Esri.WorldImagery, group = "Satellite Map") %>% 
  #     addPolylines(data = CUA5, color = "Black", weight = 3, smoothFactor = 0.5,
  #                  opacity = 1.0, fillOpacity = 1, dashArray ="4 6 2",
  #                  fillColor = "Blue") %>% 
  #     addPolygons(fillColor = ~pal_pop(Pop_Densit), color = "#444444", weight = 1, smoothFactor = 0.5,
  #                 opacity = 0.1, fillOpacity = 0.6,
  #                 label = HTML_labels(CUA5_Pop$Pop_Densit),
  #                 highlightOptions = highlightOptions(
  #                   weight = 5,
  #                   color = "#666",
  #                   opacity = 0.7,
  #                   bringToFront = TRUE)
  #     ) %>%
  #     addLegend(title = "Population Density",position = "bottomleft", values = ~Pop_Densit,
  #               pal = pal_pop,
  #               labels = c("Very low", "Low", "Medium", "High", "Very high")
  #     )  %>% 
  #     addLayersControl(
  #       baseGroups = c("Street Map (Default)", "Satellite Map"),
  #       position = 'bottomleft',
  #       options = layersControlOptions(collapsed = TRUE)
  #     )
  # })
  
  #This is currently showing the Tax mean. 
  output$environment <- renderLeaflet({
    
    leaflet(data = CUA5_Risk) %>% 
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Street Map (Default)") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite Map") %>% 
      addPolylines(data = CUA5, color = "Black", weight = 3, smoothFactor = 0.5,
                   opacity = 1.0, fillOpacity = 1, dashArray ="4 6 2",
                   fillColor = "Blue") %>% 
      addPolygons(fillColor = ~pal_tpop(tpop_mean), color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 0.1, fillOpacity = 0.6,
                  label = HTML_labels(CUA5_Risk$tpop_mean, text = ""),
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    opacity = 0.7,
                    bringToFront = TRUE)
      ) %>%
      addLegend(title = "Risk",position = "bottomleft", values = ~tpop_mean,
                pal = pal_tpop,
                labFormat = function(type, cuts, p) {  # Here's the trick
                  paste0(labels_1)}
      ) %>% 
      addLayersControl(
        baseGroups = c("Street Map (Default)", "Satellite Map"),
        position = 'bottomleft',
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  
  #This is currently showing the HHdensity_
  output$valuechain <- renderLeaflet({
    
    leaflet(data = CUA5_Risk) %>% 
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Street Map (Default)") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite Map") %>% 
      addPolylines(data = CUA5, color = "Black", weight = 3, smoothFactor = 0.5,
                   opacity = 1.0, fillOpacity = 1, dashArray ="4 6 2",
                   fillColor = "Blue") %>% 
      addPolygons(fillColor = ~pal_density(HHdensity_), color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 0.1, fillOpacity = 0.5,
                  label = HTML_labels(CUA5_Risk$HHdensity_, text = ""),
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    opacity = 0.7,
                    bringToFront = TRUE)
      ) %>%
      addLegend(title = "Risk",position = "bottomleft", values = ~HHdensity_,
                pal = pal_density,
                bins = 7,
                labFormat = function(type, cuts, p) {  # Here's the trick
                paste0(labels_1)}
      ) %>% 
      addLayersControl(
        baseGroups = c("Street Map (Default)", "Satellite Map"),
        position = 'bottomleft',
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  
  #This is currently showing interfacet
  
  output$social <- renderLeaflet({
    
    leaflet(data = CUA5_Risk) %>% 
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Street Map (Default)") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite Map") %>% 
      addPolylines(data = CUA5, color = "Black", weight = 3, smoothFactor = 0.5,
                   opacity = 1.0, fillOpacity = 1, dashArray ="4 6 2",
                   fillColor = "Blue") %>% 
      addPolygons(fillColor = ~pal_tpop(tpop_mean), color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 0.1, fillOpacity = 0.6,
                  label = HTML_labels(CUA5_Risk$tpop_mean, text = ""),
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    opacity = 0.7,
                    bringToFront = TRUE)
      ) %>%
      addLegend(title = "Risk",position = "bottomleft", values = ~tpop_mean,
                pal = pal_tpop,
                bins = 7,
                labFormat = function(type, cuts, p) {  # Here's the trick
                  paste0(labels_1)}
      ) %>% 
      addLayersControl(
        baseGroups = c("Street Map (Default)", "Satellite Map"),
        position = 'bottomleft',
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  
  output$updates <- renderUI(HTML("<ul>
  <li>04/10/2019 - Added the final texts and toilets individual informations</li>
  <li>02/10/2019 - Change the sample toilets for a best adaptation to the new borders</li>
  <li> 02/10/2019 - Added a new CUA5 shapefile based on an official source</li>
  <li>11/09/2019 - Created the first interactive version</li>
                                  </ul>"))
  
  output$home <- renderUI(HTML("<p> This microsite is a demonstration of our latest geospatial model focusing on the fifth arrondissement in the city of Antananarivo, the capital of Madagascar. You can see the visualisation of our latest risk formula and the source data that feeds into it.</p>
                               <p>This demonstration uses sample sanitation data: this means it is not yet ready to be used for decision making. Any questions? Contact us at <a href = mailto:hello@gatherhub.org>hello@gatherhub.org</a></p>"))
  
}

# When ready to deploy, it will then ask if you want to update the original app domain. Click yes. 
#it will upload here >> https://sanitationrisk.shinyapps.io/shiny
#library(rsconnect)
#rsconnect::deployApp('C:\\Users\\someg\\dev\\gather\\CUA5---Sanitation-Analysis\\Shiny')

