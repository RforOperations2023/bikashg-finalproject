library(shiny)
library(leaflet)
library(RColorBrewer)

amsterdam <- read.csv('amsterdam_weekdays.csv') %>% 
  mutate(realSum = round(realSum,0))

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  leafletOutput("map", width = "100%", height = "100%"),
  
  absolutePanel(top = 10, right = 10, draggable = TRUE,
                sliderInput("range", "Price Range", min(amsterdam$realSum), max(amsterdam$realSum),
                            value = range(amsterdam$realSum), step = 1
                ),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                checkboxInput("legend", "Show legend", TRUE)
                
                
               # absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                #              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                 #             width = 330, height = "auto
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    amsterdam[amsterdam$realSum >= input$range[1] & amsterdam$realSum <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, amsterdam$realSum)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(amsterdam) %>% addTiles() %>%
      fitBounds(~min(lng), ~min(lat), ~max(lng), ~max(lat))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(weight = 5, color = "#777777",
                 fillColor = ~pal(realSum), fillOpacity = 0.7, popup = ~paste(realSum)
      ) %>%
      addMarkers(clusterOptions = markerClusterOptions())
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = amsterdam)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~realSum
      )
    }
  })
}

shinyApp(ui, server)