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
      
   # spatial links
      #Load match file for USACE dist & div    
      USACEmatch <- read.csv(file = "data/processed/spatial/spatialLinks.csv")
      
    
### Define domains #############################################################
      
      statelist <- unique(alluse$State)
      yearlist <- unique(alluse$Year)
      countylist <- unique(alluse$Name)
      EPARegions <- paste("Region", as.character(seq(1, 10, 1)))
      USACEDivisions <- unique(USACEmatch$Division)
      USACEDistricts <- unique(USACEmatch$District)
      
      #Match EPA Regions to states: https://www.epa.gov/aboutepa/regional-and-geographic-offices
      RegionStateMatch <- list("Region 1" = c("CT", "ME", "MA", "NH", "RI", "VT"), 
                               "Region 2" = c("NJ", "NY"),
                               "Region 3" = c("DE", "DC", "MD", "PA", "VA", "WV"),
                               "Region 4" = c("AL", "FL", "GA", "KY", "MS", "NC", "SC", "TN"),
                               "Region 5" = c("IL", "IN", "MI", "MN", "OH", "WI"),
                               "Region 6" = c("AR", "LA", "NM", "OK", "TX"),
                               "Region 7" = c("IA", "KS", "MO", "NE"),
                               "Region 8" = c("CO", "MT", "ND", "SD", "UT", "WY"),
                               "Region 9" = c("AZ", "CA", "HI", "NV"),
                               "Region 10" = c("AK", "ID", "OR", "WA"))
      
      RegionStatePairs <- RegionStateMatch %>% list2DF() %>%
                                               pivot_longer(cols = EPARegions) %>%
                                               distinct() %>% 
                                               rename(Region = name, 
                                                      State = value)
      
### Create modified dataframes #################################################
   
   # EPA Regions
      epaUse <- alluse %>% merge(., RegionStatePairs, by = "State")
      
   #USACE
      #Match spatialLinks to alluse
      USACEuse <- merge(alluse, USACEmatch, by = c("State", "County", "Name"), all.y = TRUE) 
         #shorter than all use. Must be counties not in the spatialLink file. 
      
      
### Define functions ###########################################################

   # --- State -----------------------------------------------------------------
      StateSankey <- function(state, year = "2015") {
         
         state.sel <- state
         year.sel <- year
         
         sttyr <- alluse %>% filter(State == state.sel & Year == year.sel)
         
         statesum <- sttyr %>% group_by(State, Year, Type, Category) %>% summarize(Total_MGD = sum(MGD, na.rm=TRUE), .groups = "drop")
         links <- statesum[c("Type","Category", "Total_MGD")]
         links <- filter(links, links$Type != "Total", links$Category != "Total") #Removes "Total" values for accurate scaling
         as.data.frame(links)
         
         nodes <- data.frame(
            name=c(as.character(links$Type), 
                   as.character(links$Category)) %>% unique()
         )
         
         links$IDsource <- match(links$Type, nodes$name)-1 
         links$IDtarget <- match(links$Category, nodes$name)-1
         
         colorAssign <- 'd3.scaleOrdinal() .domain(["SW", "GW","Industrial", "Power", "Public", "Mining", "Irrigation", "Livestock", "Domestic"]) 
            .range(["lightsteelblue", "navy" , "darkgrey", "goldenrod", "maroon", "peru", "darkolivegreen", "indianred", "darkcyan"])'
         
         s <- sankeyNetwork(Links = links, Nodes = nodes, LinkGroup = "Type",
                            Source = "IDsource", Target = "IDtarget",
                            Value = "Total_MGD", NodeID = "name", 
                            sinksRight=FALSE, colourScale = colorAssign, 
                            fontSize = 10, units = "MGD")
         s
      }

   # --- County ----------------------------------------------------------------
      CountySankey <- function(state, county, year = "2015") {
         
         state.sel <- state
         county.sel <- county
         year.sel <- year
         
         countyyr <- alluse %>% filter(State == state.sel & Name == county.sel & Year == year.sel)
         
         countysum <- countyyr %>% group_by(State, Name, Year, Type, Category) %>% summarize(Total_MGD = sum(MGD, na.rm=TRUE), .groups = "drop")
         links <- countysum[c("Type","Category", "Total_MGD")]
         links <- filter(links, links$Type != "Total", links$Category != "Total") #Removes "Total" values for accurate scaling
         as.data.frame(links)
         
         nodes <- data.frame(
            name=c(as.character(links$Type), 
                   as.character(links$Category)) %>% unique()
         )
         
         links$IDsource <- match(links$Type, nodes$name)-1 
         links$IDtarget <- match(links$Category, nodes$name)-1
         
         colorAssign <- 'd3.scaleOrdinal() .domain(["SW", "GW","Industrial", "Power", "Public", "Mining", "Irrigation", "Livestock", "Domestic"]) 
               .range(["lightsteelblue", "navy" , "darkgrey", "goldenrod", "maroon", "peru", "darkolivegreen", "indianred", "darkcyan"])'
         
         s <- sankeyNetwork(Links = links, Nodes = nodes, LinkGroup = "Type",
                            Source = "IDsource", Target = "IDtarget",
                            Value = "Total_MGD", NodeID = "name", 
                            sinksRight=FALSE, colourScale = colorAssign, 
                            fontSize = 10, units = "MGD")
         s
      }
   # --- EPA Region ------------------------------------------------------------
      EPARegionSankey <- function(EPARegion, year = "2015") {
         
         reg.sel <- EPARegion
         year.sel <- year

         regyr <- epaUse %>% filter(Region == reg.sel & Year == year.sel)
         
         regsum <- regyr %>% group_by(Region, Year, Type, Category) %>% summarize(Total_MGD = sum(MGD, na.rm=TRUE), .groups = "drop")
         links <- regsum [c("Type","Category", "Total_MGD")]
         links <- filter(links, links$Type != "Total", links$Category != "Total") #Removes "Total" values for accurate scaling
         as.data.frame(links)
         
         nodes <- data.frame(
            name=c(as.character(links$Type), 
                   as.character(links$Category)) %>% unique()
         )
         
         links$IDsource <- match(links$Type, nodes$name)-1 
         links$IDtarget <- match(links$Category, nodes$name)-1
         
         colorAssign <- 'd3.scaleOrdinal() .domain(["SW", "GW","Industrial", "Power", "Public", "Mining", "Irrigation", "Livestock", "Domestic"]) 
            .range(["lightsteelblue", "navy" , "darkgrey", "goldenrod", "maroon", "peru", "darkolivegreen", "indianred", "darkcyan"])'
         
         s <- sankeyNetwork(Links = links, Nodes = nodes, LinkGroup = "Type",
                            Source = "IDsource", Target = "IDtarget",
                            Value = "Total_MGD", NodeID = "name", 
                            sinksRight=FALSE, colourScale = colorAssign, 
                            fontSize = 10, units = "MGD")
         s
         
      }
   # Can also add HUC6/HUC8 - I've got draft versions of those fxns -SAB     

   # --- USACE Division --------------------------------------------------------
     
      USACEDivisionSankey <- function(USACEDivisions, year = "2015") {
         div.sel <- USACEDivisions
         year.sel <- year
         
         divyr <- USACEuse %>% filter(Division.y == div.sel & Year == year.sel)
         
         divsum <- distyr %>% group_by(USACEDivision, Year, Type, Category) %>% summarize(Total_MGD = sum(MGD, na.rm=TRUE), .groups = "drop")
         
         links <- divsum [c("Type","Category", "Total_MGD")]
         links <- filter(links, links$Type != "Total", links$Category != "Total") #Removes "Total" values for accurate scaling
         as.data.frame(links)
         
         nodes <- data.frame(
            name=c(as.character(links$Type), 
                   as.character(links$Category)) %>% unique()
         )
         
         links$IDsource <- match(links$Type, nodes$name)-1 
         links$IDtarget <- match(links$Category, nodes$name)-1
         
         colorAssign <- 'd3.scaleOrdinal() .domain(["SW", "GW","Industrial", "Power", "Public", "Mining", "Irrigation", "Livestock", "Domestic"]) 
            .range(["lightsteelblue", "navy" , "darkgrey", "goldenrod", "maroon", "peru", "darkolivegreen", "indianred", "darkcyan"])'
         
         s <- sankeyNetwork(Links = links, Nodes = nodes, LinkGroup = "Type",
                            Source = "IDsource", Target = "IDtarget",
                            Value = "Total_MGD", NodeID = "name", 
                            sinksRight=FALSE, colourScale = colorAssign, 
                            fontSize = 10, units = "MGD")
         
         
      }
      
      
   # --- USACE District --------------------------------------------------------
      

   USACEDistrictSankey <- function(USACEDistricts, year = "2015") {
      dis.sel <- USACEDistricts
      year.sel <- year
      
      distyr <- USACEuse %>% filter(District.y == dis.sel & Year == year.sel)
      
      dissum <- distyr %>% group_by(USACEDistrict, Year, Type, Category) %>% summarize(Total_MGD = sum(MGD, na.rm=TRUE), .groups = "drop")
      
      links <- dissum [c("Type","Category", "Total_MGD")]
      links <- filter(links, links$Type != "Total", links$Category != "Total") #Removes "Total" values for accurate scaling
      as.data.frame(links)
      
      nodes <- data.frame(
         name=c(as.character(links$Type), 
                as.character(links$Category)) %>% unique()
      )
      
      links$IDsource <- match(links$Type, nodes$name)-1 
      links$IDtarget <- match(links$Category, nodes$name)-1
      
      colorAssign <- 'd3.scaleOrdinal() .domain(["SW", "GW","Industrial", "Power", "Public", "Mining", "Irrigation", "Livestock", "Domestic"]) 
            .range(["lightsteelblue", "navy" , "darkgrey", "goldenrod", "maroon", "peru", "darkolivegreen", "indianred", "darkcyan"])'
      
      s <- sankeyNetwork(Links = links, Nodes = nodes, LinkGroup = "Type",
                         Source = "IDsource", Target = "IDtarget",
                         Value = "Total_MGD", NodeID = "name", 
                         sinksRight=FALSE, colourScale = colorAssign, 
                         fontSize = 10, units = "MGD")
      
      
   }
      
############################ DEFINE APP UI #####################################

ui <- fluidPage(
   # Set the theme
   theme = shinytheme("united"),

    # Application title
    titlePanel(h1("Water Usage by Source & Category")),
               
    # App credits
    fluidRow(
       column(width = 5, offset = 2.5, 
              em("Created by Rebecca Murphy and Sophia Bryson \n for ENV 872 - Environmental Data Analytics (Spring 2022)"))
       ), #end fluid row
    hr(),

   mainPanel(
   # Create tabs to allow for geometry type selection
      tabsetPanel(type = "tabs",
                  tabPanel("About the dashboard", 
                           h3("The Data"),
                           "The data in this dashboard are from USGS records of water use across the nation, reported by source and category.",
                           "These usage estimates are provided at 5-year intervals from 1985 through 2015.",
                           "The data and more information can be found on the ", 
                           tags$a(href = 'https://waterdata.usgs.gov/nwis/wu', 'USGS Water Data for the Nation website'),
                           h3("\n\nUsing the dashboard"),
                           "Water use data can be visualized for a variety of geographic areas and extents.",
                           "Select a tab above corresponding to a political (state or county) or administrative (EPA region, USACE Division or District) geography,",
                           "then use the menus to select a specific geographic unit and year.",
                           "Reference maps are provided to identify the locations and extentes of EPA regions and USACE Districts and Divisions."), 
                  tabPanel("By State", 
                           "\n \n", #create some space
                           selectInput("state", label = "Select a state",
                                       choices = statelist),
                           selectInput("state_year", label = "Select a year", 
                                       choices = yearlist),
                           sankeyNetworkOutput("stateSankey")),
                  tabPanel("By County", 
                           "\n \n", #create some space
                           selectInput("state_county", label = "Select a state",
                                       choices = statelist, 
                                       selected = "AL"),
                           selectInput("county", label = "Select a county",
                                       choices = countylist), 
                           selectInput("county_year", label = "Select a year", 
                                       choices = yearlist),
                           sankeyNetworkOutput("countySankey")),
                  # tabPanel("By Watershed", sankeyNetworkOutput("HUC4Sankey")),
                  tabPanel("By EPA region", 
                           "\n \n", #create some space
                           img(src = "EPARegionMap.png", height="70%", width="70%", align="left"),
                           selectInput("EPARegion", label = "Select an EPA region",
                                       choices = EPARegions), 
                           selectInput("region_year", label = "Select a year", 
                                       choices = yearlist),
                           sankeyNetworkOutput("EPASankey")) ,
                  tabPanel("By USACE division",
                            "\n \n", #create some space,
                            img(src = "USACEDivisionMap.png", align = "left", height = "70%", width = "70%"),
                           selectInput("USACEDivision", label = "Select a USACE Division",
                                        choices = USACEDivisions), 
                            selectInput("division_year", label = "Select a year", 
                                        choices = yearlist),
                           sankeyNetworkOutput("DistrictSankey")),
                  tabPanel("By USACE district",
                            "\n \n", #create some space
                             img(src = "USACEDistrictMap.jfif", align = "left", height = "70%", width = "70%"),
                           selectInput("USACEDistrict", label = "Select a USACE District",
                                        choices = USACEDistricts), 
                            selectInput("district_year", label = "Select a year", 
                                        choices = yearlist),
                            sankeyNetworkOutput("USACEDistrictSankey")) #,
                  # tabPanel("Over Time",
                  #          "\n \n",
                  #          "ADD A gganimate here??") #TEMP
         ) #end tabset
    ) #end main panel
) #end fluid page

############################ DEFINE APP SERVER #################################

server <- function(input, output, session) {
   
   # Constrain list of counties to only those in the selected state - see https://community.rstudio.com/t/how-to-update-one-selectinput-w-r-t-to-other-these-inputs-store-values-of-two-columns-from-a-single-data-frame/22133/3
   observeEvent(input$state_county, {
      updateSelectInput(session, "county", choices = alluse$Name[alluse$State == input$state_county])
   })
   

   output$stateSankey <- renderSankeyNetwork(expr = StateSankey(state = input$state, 
                                                                year = input$state_year))
   
   output$countySankey <- renderSankeyNetwork(expr = CountySankey(state = input$state_county, 
                                                                  county = input$county, 
                                                                  year = input$county_year))
   
   # output$HUC4Sankey <- renderSankeyNetwork()
   #    
   # output$HUC8Sankey <- renderSankeyNetwork({})
      
   output$EPASankey <- renderSankeyNetwork(expr = EPARegionSankey(EPARegion = input$EPARegion,
                                                                  year = input$region_year))
   
   output$USACEDistrictSankey <- renderSankeyNetwork(expr = USACEDistrictSankey(USACEDistrict = input$USACEDistricts,
                                                                                year = input@district_year))
   
   output$USACEDivisionSankey <- renderSankeyNetwork(expr = USACEDivisionSankey(USACEDivision = input$USACEDivisions,
                                                                                year = input@division_year))
}

################################## RUN APP #####################################

shinyApp(ui = ui, server = server)




########### NOTES ##############################################################
# Visualize by:
# + National      
# + State
# + County
# + HUC
# + EPA region (state match)

# Select:
# + Region of interest (state, huc, EPA)
# + Year

# Animate? could maybe use gganimate to show changes in water use over time? 
      
#Possible to select by map rather than by list? Worth trying?  https://community.rstudio.com/t/select-polygon-by-clicking-on-map-changing-item-selected-via-dropdown/89211/2 
      
# Add values (MGD) to diagrams? 
   