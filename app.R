#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

#set up for the shiny app
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(readr)
library(utils)
library(utils)
library(terra)

source("C:/Users/sageb/OneDrive/Desktop/ESS_GitHub/Final-Projects-Class/setup.R")

load("C:/Users/sageb/OneDrive/Desktop/ESS_GitHub/Final-Projects-Class/data/spatdat.RData")

#read in the elevation and landcover rasters
landcover <- terra::rast("C:/Users/sageb/OneDrive/Desktop/ESS_GitHub/Final-Projects-Class/data/NLCD_CO.tif")
elevation <- terra::rast("C:/Users/sageb/OneDrive/Desktop/ESS_GitHub/Final-Projects-Class/data/elevation.tif")

# read in data
load("C:/Users/sageb/OneDrive/Desktop/ESS_GitHub/Final-Projects-Class/data/shinyDemoData.RData") # includes `occ` and `ROMO`

# Specify the path to the Zip file
zip_file <- "C:/Users/sageb/OneDrive/Desktop/ESS_GitHub/Final-Projects-Class/data/observations-734996.csv.zip"

# Extract files from the Zip archive
unzip(zip_file, exdir = "C:/Users/sageb/OneDrive/Desktop/ESS_GitHub/Final-Projects-Class/Shiny_Final")

iNat <- read.csv("C:/Users/sageb/OneDrive/Desktop/ESS_GitHub/Final-Projects-Class/Shiny_Final/observations-734996.csv")

nv_counties <- counties(state = "NV")

#Clean-up
# clean up the occ data and make spatial
iNat <- bind_rows(iNat) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

#Elevation
nv_elevation <- get_elev_raster(nv_counties, z = 7)
nv_elevation_terra <- rast(nv_elevation)
iNat$elevation <- nv_elev_terra[,2]

#landcover
landcover <- terra::rast("C:/Users/sageb/OneDrive/Desktop/ESS_GitHub/Final-Projects-Class/Shiny_Final/Annual_NLCD_CONUSV1_Ref_Data_Release/lcnext-1.0-stratum-map-Clipped.tif")
landcover_prj <- project(landcover, crs(iNat))
extract(landcover_prj, iNat)
iNat <- iNat %>%
  mutate(common_landcover = extract(landcover_prj, iNat)) %>%
  unnest(common_landcover) %>% 
  #lets rename the land cover column which is now called "NLCD Land Cover Class"
  rename(common_landcover = "NLCD Land Cover Class")

# Define UI for application that draws a histogram
ui <- fluidPage(
  #App title
  titlePanel("Free-Roaming Horse Distribution"),
  
  # Add some informational text using and HTML tag (i.e., a level 5 heading)
  h5(
    "In this app you can filter occurrences by species, type of observation, and elevation. You can also click on individual occurrences to view metadata."
  ),
  
  # Sidebar layout
  sidebarLayout(
    # Sidebar panel for widgets that users can interact with
    sidebarPanel(
      # Input: select species shown on map
      checkboxGroupInput(
        inputId = "common_name",
        label = "common_name",
        # these names should match that in the dataset, if they didn't you would use 'choiceNames' and 'choiceValues' like we do for the next widget
        choices = list("Domestic Horse", "Donkey", "Equines (not identified to species)"),
        # selected = sets which are selected by default
        selected = c("Domestic Horse", "Donkey", "Equines - not identified to species")
      ),
      
      # Input: select landcover shown on map
      checkboxGroupInput(
        inputId = "landcover",
        label = "landcover",
        # these names should match that in the dataset, if they didn't you would use 'choiceNames' and 'choiceValues' like we do for the next widget
        choices = list("D1", "Donkey", "Equines (not identified to species)"),
        # selected = sets which are selected by default
        selected = c("Domestic Horse", "Donkey", "Equines - not identified to species")
      ),
      
      # Input: Filter by elevation
      sliderInput(
        inputId = "elevation",
        label = "Elevation",
        min = 371,
        max = 3816,
        value = c(371, 3816)
      )
      
    ),
    
    
    # Main panel for displaying output (our map)
    mainPanel(# Output: interactive tmap object
      leafletOutput("map"))
    
  )
  
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  # Make a reactive object for the occ data by calling inputIDs to extract the values the user chose
  iNat_react <- reactive(
    iNat %>%
      filter(common_name %in% input$common_name) %>%
      filter(landcover %in% input$landcover)%>%
      filter(elevation >= input$elevation[1] &
               elevation <= input$elevation[2])
  )
  
  # Render the map based on our reactive occurrence dataset
  
  # Create color palette for species
  pal <- colorFactor(palette = "Dark2", domain = iNat$common_name)
  output$map <- renderLeaflet({
    # Create leaflet map
    leaflet() %>%
      addTiles() %>%
      # Add species occurrence points
      addCircleMarkers(
        data = iNat_react(),  # Note the () after occ_react!
        radius = 3,
        color = ~ pal(common_name),
        fillOpacity = 0.7,
        stroke = FALSE,
        popup = ~ paste0(
          "<b>Species:</b> ",
          common_name,
          "<br>",
          "<b>Landcover:</b> ",
          landcover,
          "<br>",
          "<b>Elevation (m):</b> ",
          elevation
        )
      ) %>%
      # Add ROMO polygon
      addPolygons(
        data = nv_counties,
        fillOpacity = 0.7,
        weight = 2,
        color = "black",
        fillColor = "gray"
      ) %>%
      # Add legend
      addLegend(
        position = "bottomright",
        pal = pal,
        values = iNat$common_name,
        title = "Species Occurrences"
      )
    addLegend(
      position = "bottomleft",
      pal = pal,
      values = iNat$landcover,
      title = "Landcover Types"
    )
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
