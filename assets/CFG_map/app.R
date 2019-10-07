#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rgdal)
library(rgeos)
library(sp)
library(leaflet)
library(tidyverse)


#setwd("../Cave_Fauna_Greece_Analysis/Spatial_caves_CFG")
dataPath <- getwd()
#print(getwd())
katafygia_agrias_zwhs_simple <- rgdal::readOGR("kaz_wgs84/kaz_wgs84_simple.shp",verbose=F)


natura2000_new_shapefile_v30  <- rgdal::readOGR("natura2000_new_shapefile_v30_wgs84_simple/natura2000_new_shapefile.shp",verbose = F)
#natura2000_new_shapefile_v30_wgs84 <- spTransform(natura2000_new_shapefile_v30, CRS("+proj=longlat +datum=GGRS87 +no_defs"))

pal_natura <- colorFactor(palette = c("skyblue1","chocolate4","forestgreen"),domain = c("SCI","SPA","SCISPA"),na.color = "#000000")

# best way to simplify is ms_simplify
#natura2000_new_shapefile_v30_wgs84_simple_ok <- rmapshaper::ms_simplify(natura2000_new_shapefile_v30_wgs84,keep = 0.05,keep_shapes =T,drop_null_geometries = TRUE)
# writeOGR(obj=natura2000_new_shapefile_v30_wgs84_simple_ok,
#          layer ="natura2000_new_shapefile",  
#          dsn="natura2000_new_shapefile_v30_wgs84_simple",
#          driver="ESRI Shapefile") # this is in geographical projection
# #writeSpatialShape(natura2000_new_shapefile_v30_wgs84_simple_ok, "natura2000_new_shapefile_v30_wgs84_simple_ok")

#rm(natura2000_new_shapefile_v30,natura2000_new_shapefile_v30_wgs84)
# must copy paste the new caves file from Data folder
caves <- read_delim(file = grep("Caves",list.files(),value = TRUE),delim = "\t")

# to create links to CFG space and other characters must be converted according to ASCII Encoding Reference
# https://www.w3schools.com/tags/ref_urlencode.asp


pal_cave <- colorFactor(c("black", "red","orange"), domain = c("Artificial", "Natural","Natural Modified"))

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("mymap", width = "100%", height = "100%"), p()
)

server <- function(input, output, session) {
  
  
  output$mymap <- renderLeaflet({
        leaflet() %>% addCircleMarkers(data = caves,~Longitude, ~Latitude,color=pal_cave(caves$Cave_Type),radius= 1.5, popup = ~as.character(Cave_ID), label = ~as.character(Cave_Name),highlightOptions(bringToFront=T),group = "Caves") %>% addPolygons(data=katafygia_agrias_zwhs_simple ,color = "orange", weight = 0.3, smoothFactor = 0.5, opacity = 0.4, fillOpacity = 0.4,fillColor = "orange",highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = F),group = "Wildlife refuge") %>% addPolygons(data=natura2000_new_shapefile_v30 ,color = "springgreen2", weight = 0.3, smoothFactor = 0.5,opacity = 0.6, fillOpacity = 0.5,fillColor = ~pal_natura(natura2000_new_shapefile_v30@data$SITETYPE),highlightOptions=highlightOptions(color = "white", weight = 2,bringToFront = FALSE),group = "Natura2000") %>% addProviderTiles("Stamen.Terrain", group = "Terrain") %>% addProviderTiles("Esri.WorldImagery", group = "World Imagery") %>% addProviderTiles("Stamen.TonerLite", group = "Toner") %>% addLegend("bottomright", pal = pal_natura, values = natura2000_new_shapefile_v30@data$SITETYPE, title = "Natura2000", labFormat = labelFormat(prefix = ""), opacity = 1) %>% addLayersControl( baseGroups = c("Terrain","World Imagery","Toner"), overlayGroups = c("Wildlife refuge","Natura2000","Caves"), options = layersControlOptions(collapsed = FALSE))
  })
}


shinyApp(ui, server)
