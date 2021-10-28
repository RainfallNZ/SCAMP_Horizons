library(shiny)
library(leaflet)
library(htmltools)

#r_colors <- rgb(t(col2rgb(colors()) / 255))
#names(r_colors) <- colors()
reprojected.data.WGS84 <- readRDS("Data/SpatialData.RDS")

#Set the online map source to TOPO NZ maps
NZTopo250 = 'http://tiles-a.data-cdn.linz.govt.nz/services;key=8ed417cc81ea45a0b92d597307229b80/tiles/v4/layer=52324/EPSG:3857/{z}/{x}/{y}.png'
NZTopo50 = 'http://tiles-a.data-cdn.linz.govt.nz/services;key=8ed417cc81ea45a0b92d597307229b80/tiles/v4/layer=52343/EPSG:3857/{z}/{x}/{y}.png'

map <- leaflet::leaflet() %>%
  addTiles(urlTemplate = NZTopo250, options = providerTileOptions(maxZoom=14),attribution = "<a href=\"http://https://www.linz.govt.nz/\">LINZ</a>", group = "LINZ Topographic") %>%
  addTiles(urlTemplate = NZTopo50, options = providerTileOptions(minZoom=14), group = "LINZ Topographic") %>%
  #leaflet::addProviderTiles(providers$OpenStreetMap) %>%
  setView(lng=175.5,lat=-40.0,zoom=8) %>%
  addPolygons(data = reprojected.data.WGS84$SubZones, color = "black", weight = 3, fillColor = "transparent", label = ~htmlEscape(Zone_Code)) %>%
  addCircleMarkers(data = reprojected.data.WGS84$MeasurementSites, color = "red",label = ~htmlEscape(sID)) %>%
  addCircleMarkers(data = reprojected.data.WGS84$PointSourceSites, color = "black", label = ~htmlEscape(Site.Name)) %>%
  addPolylines(data = reprojected.data.WGS84$RiverNetwork, color= "blue", label = ~htmlEscape(Label))

ui <- fluidPage(
  titlePanel("LWP"),
  leafletOutput("mymap", height = "100vh"),
  
)

server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({map})
  
}

shinyApp(ui, server)