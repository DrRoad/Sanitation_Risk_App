library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)
library(plotly)
library(lubridate)
library(scales)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)
library(shinycssloaders)
source('ui_modules.R')

lorem_ipsum <- "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Vivamus arcu felis bibendum ut tristique et egestas quis. Et odio pellentesque diam volutpat commodo sed egestas egestas fringilla. Malesuada fames ac turpis egestas integer. Nulla posuere sollicitudin aliquam ultrices sagittis orci a. Libero id faucibus nisl tincidunt eget. Mi bibendum neque egestas congue. Proin sed libero enim sed. Erat imperdiet sed euismod nisi porta lorem. Velit ut tortor pretium viverra suspendisse potenti nullam ac. Mauris vitae ultricies leo integer malesuada nunc vel. Vivamus at augue eget arcu dictum varius duis at. Eleifend mi in nulla posuere sollicitudin aliquam ultrices sagittis orci. Aenean et tortor at risus viverra adipiscing."

# User interface ----
ui <- tagList(
  tags$head(tags$script(type="text/javascript", src = "code.js")),
  navbarPage(title = "Sanitation Risk for CUA5", id = "nav", theme = "style.css",
             tabPanel('Home', value = -1,
                      fluidRow( class = "updateTitle",
                                column(4, "Geospatial model for sanitation risk in CUA5, Antananarivo, Madagascar", div(style = "height:30px;"), offset = 4)
                      ),
                      fluidRow(class = "updateArea",
                               column(4, uiOutput(outputId = 'home'), offset = 4)
                      )),
             tabPanel("Risk", value = 0,
                      leafletOutput(outputId = "risk", height = 700) %>% withSpinner(type = 4),
                      absolutePanel(id = "waste", class = "panel panel-default", fixed = TRUE,
                                    draggable = F, top =140, left = "auto", right = 50, bottom = "auto",
                                    width = 400, height = 600,
                                    #Stats on the side
                                    h1("Risk"),
                                    br(),
                                    p(id = "mainText", "This map visualises the Urban Sanitation Risk Index for CUA5, Antananarivo, Madagascar. "),
                                    p(id = "mainText", "The increase of risk towards 'High Risk' indicates a higher chance that an individual in a community to come into contact with a sanitation related disease."),
                                    p(id = "mainText", "It is calculated as the mean of Environment, Social and Value Chain Datasets (see Data Sources Tab)."),
                                    p(id = "mainText", "Hovering above the grid cell will reveal the Sanitation Risk Value."),
                                    p(id = "mainText", "Change to satellite view with the layer button above the legend (bottom left)!"))
                      
             ),
             navbarMenu('Data Sources',
               tabPanel("Social Data", value = 1,
                        leafletOutput(outputId = "social", height = 700) %>% withSpinner(type = 4),
                        absolutePanel(id = "waste", class = "panel panel-default", fixed = TRUE,
                                      draggable = F, top =140, left = "auto", right = 50, bottom = "auto",
                                      width = 400, height = 600,
                                      h1("Social Data"),
                                      br(),
                                      p(id = "mainText","The sanitation data displayed here was created by Gather’s Centre for Sanitation Analytics. This shows the sum of all social datasets included in this analysis."),
                                      p(id = "mainText","This includes data from GPURL (World Bank) and Gather."),
                                      p(id = "mainText", "Hovering over each grid cell reveals the normalised value."))
                        
                          
               ),
               tabPanel("Environment Data", value = 2,
                        
                        leafletOutput(outputId = "environment", height = 700)%>% withSpinner(type = 4),
                        absolutePanel(id = "waste", class = "panel panel-default", fixed = TRUE,
                                      draggable = F, top =140, left = "auto", right = 50, bottom = "auto",
                                      width = 400, height = 600,
                                      h1("Environment"),
                                      br(),
                                      p(id = "mainText", "The sanitation data displayed here was created by Gather’s Centre for Sanitation Analytics. This shows the sum of all environment datasets included in this analysis."),
                                      p(id = "mainText", "This includes data from GPURL (World Bank) and Gather."),
                                      p(id = "mainText", "Hovering over each grid cell reveals the normalised value."))
                        
               ),
               tabPanel("Value Chain Data", value = 3,
                        leafletOutput(outputId = "valuechain", height = 700) %>% withSpinner(type = 4),
                        absolutePanel(id = "waste", class = "panel panel-default", fixed = TRUE,
                                      draggable = F, top =140, left = "auto", right = 50, bottom = "auto",
                                      width = 400, height = 600,
                                      h1("Value Chain"),
                                      br(),
                                      p(id = "mainText", "The sanitation data displayed here was created by Gather’s Centre for Sanitation Analytics. This shows the sum of all value chain datasets included in this analysis."),
                                      p(id = "mainText", "This includes data from GPURL (World Bank) and Gather."),
                                      p(id = "mainText", "Hovering over each grid cell reveals the normalised value."))
               )),
                                     
               
               # tabPanel("Road Density", value = 4,
               #          leafletOutput(outputId = "roads", height = 700) %>% withSpinner(type = 4),
               #          absolutePanel(id = "waste", class = "panel panel-default", fixed = TRUE,
               #                        draggable = F, top =140, left = "auto", right = 50, bottom = "auto",
               #                        width = 400, height = 600,
               #                        h1("Road Density"),
               #                        br(),
               #                        p(id = "mainText","This map visualises road density based on the location of roads and pathways from OpenStreetMap. The proximity of roads to sanitation infrastructure allows us to assess how easily a waste collection vehicle can access and empty a pit latrine of septic tank."),
               #                        p(id = "mainText","We plan to update the accuracy of the road data in partnership with OpenStreetMap so that we can differentiate roads from pedestrian pathways that are unsuitable for vehicles."),
               #                        HTML('<p id = "mainText">For more information on OpenStreetMap, please visit their <a href="https://www.openstreetmap.org/">website</a></p>'))
               # )),
             
             # tabPanel("Appendix", value = 5,
             #          tags$iframe(class = 'leaflet-container', style="height:400px; width:100%; scrolling=yes", src="Appendix - Moving toward predictive, geospatial analytics for urban sanitation.pdf")),
             # tabPanel("Site Updates", value = 6,
             #          fluidRow( class = "updateTitle",
             #            column(4, "Site Updates", div(style = "height:30px;"), offset = 4)
             #          ),
             #          fluidRow(class = "updateArea",
             #            column(4, uiOutput(outputId = 'updates'), offset = 4)
             #          )
             #           ,
                      
                      
                  
            useShinyjs()
             ))
