################################################################################
#           
#           Dashboard created by Rebecca Murphy and Sophia Bryson
#                    ENV 872: Environmental Data Analytics 
#                                Spring 2022
#
#                    Visualizes USGS water use data
################################################################################

################################# SET UP #######################################
### Load libraries #############################################################

library(shiny)
library(tidyverse)
library(shinythemes)
library(networkD3)
library(sf)
library(mapview)
library(tidycensus)

### Load data ##################################################################

   #Political boundaries (State and County)
      AllUseClasses <- c("character", "character", "character", "character", "character", 
                         "character", "numeric", "numeric", "numeric", "character", "character", "numeric")
      #define classes to retain leading zeroes on GEOID/ FIPS column
      alluse <- read.csv(file = "data/processed/water_use/All_Use.csv", colClasses = AllUseClasses)
   
   #huc6 
      huc6use.classes <- c("character", "character", "character", "character", "character", 
                           "numeric", "numeric", "character","character", "numeric")
      huc6use.data <- read.csv(file = "data/processed/water_use/HUC6Use.csv", colClasses = huc6use.classes)
   
   #huc8
      huc8use.classes <- c("character", "character", "character", "character", "character",
                           "numeric", "numeric", "character", "character", "numeric")
      huc8use.data <- read.csv(file = "data/processed/water_use/HUC8Use.csv", colClasses = huc8use.classes)
 
### Define domains #############################################################
      
      statelist <- unique(alluse$State)
      yearlist <- unique(alluse$year)
      
### Define functions ###########################################################


############################ DEFINE APP UI #####################################

ui <- fluidPage(
   # Set the theme
   theme = shinytheme("united"),

    # Application title
    titlePanel(h1("Water Usage by Source & Use")),
               
    # App credits
    fluidRow(
       column(width = 5, offset = 2.5, 
              em("Created by Rebecca Murphy and Sophia Bryson \n for ENV 872 - Environmental Data Analytics (Spring 2022)"))),
    hr(),

   mainPanel(
   # Create tabs to allow for geometry type selection
      tabsetPanel(type = "tabs",
                  tabPanel("By State", 
                           selectInput("state", label = "Select a state",
                                       choices = statelist),
                           sankeyNetworkOutput("stateSankey")),
                  tabPanel("By County", 
                           selectInput("state_county", label = "Select a state",
                                       choices = statelist),
                           selectInput("county", label = "Select a county",
                                       choices = c("FILL IN WITH FILTERED COUNTIES")), 
                           sankeyNetworkOutput("countySankey")),
                  # tabPanel("By Watershed", sankeyNetworkOutput("HUC4Sankey")),
                  tabPanel("By EPA region", 
                           selectInput("EPARegion", label = "Select an EPA region",
                                       choices = c("FILL IN WITH FILTERED EPA REGIONS (MATCH)")), 
                           sankeyNetworkOutput("EPASankey")))
   
    )
)

############################ DEFINE APP SERVER #################################

server <- function(input, output) {

   output$stateSankey <- renderSankeyNetwork({})
   
   output$countySankey <- renderSankeyNetwork({})
   
   # output$HUC4Sankey <- renderSankeyNetwork({})
   #    
   # output$HUC8Sankey <- renderSankeyNetwork({})
      
   output$EPASankey <- renderSankeyNetwork({})
   
}

################################## RUN APP #####################################

shinyApp(ui = ui, server = server)




########### NOTES ##############################################################
# Visualize by:
# + State
# + County
# + HUC
# + EPA region (state match)

# Select:
# + Region of interest (state, huc, EPA)
# + Year

# Animate?
      
#Possible to select by map rather than by list? Worth trying?       
   