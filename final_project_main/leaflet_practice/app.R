library(shiny)
library(leaflet)
library(sp)

# Define UI for Shiny app
ui <- fluidPage(
  leafletOutput("map"),
  selectInput("layer_type", "Choose layer type", choices = c("Polygon", "Line")),
  numericInput("layer_opacity", "Layer opacity", value = 0.5, min = 0, max = 1, step = 0.1)
)

# Define server logic for Shiny app
server <- function(input, output, session) {
  
  # Create initial leaflet map
  output$map <- renderLeaflet({
    leaflet() %>% addTiles()
  })
  
  # Add polygons to map
  observe({
    if (input$layer_type == "Polygon") {
      leafletProxy("map") %>% 
        clearShapes() %>% 
        addPolygons(data = sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(rbind(c(-118, 34), c(-118, 36), c(-116, 36), c(-116, 34)))))),
                                               fillColor = "red",
                                               fillOpacity = input$layer_opacity)
    )}
  })
      
      # Add lines to map
      observe({
        if (input$layer_type == "Line") {
          leafletProxy("map") %>% 
            clearShapes() %>% 
            addPolylines(data = sp::SpatialLines(list(sp::Lines(list(sp::Line(rbind(c(-118, 34), c(-118, 36), c(-116, 36), c(-116, 34)))))),
                                                 color = "blue",
                                                 opacity = input$layer_opacity)
        }
      }))
          
          # Add markers to map
          observe({
            leafletProxy("map") %>% 
              clearMarkers() %>% 
              addMarkers(lng = -117.2, lat = 34.05, popup = "Marker")
          })
          
          # Add heatmap to map
          observe({
            leafletProxy("map") %>% 
              clearHeatmap() %>% 
              addHeatmap(lng = -117.5, lat = 34, intensity = 10)
          })
          
          # Add polygons to map
          observe({
            leafletProxy("map") %>% 
              clearShapes() %>% 
              addPolygons(data = sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(rbind(c(-118.4, 34.1), c(-118.3, 34.1), c(-118.3, 34), c(-118.4, 34)))))),
                                                     fillColor = "green",
                                                     fillOpacity = 0.5)
          }))
            
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
