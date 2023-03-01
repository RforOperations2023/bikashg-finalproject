library(shiny)
library(leaflet)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

amsterdam <- read.csv('amsterdam_weekdays.csv')

ui <- fluidPage(
  leafletOutput("mymap"),
  p(),
  actionButton("recalc", "New points")
)

server <- function(input, output, session) {
  
  points <- eventReactive(input$recalc, {
    cbind(amsterdam$lat, amsterdam$lng)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      #addCircles(data=amsterdam, lng= ~lng, lat= ~lat)
      #addMarkers(data = points())
      addCircles(data=amsterdam, lng= ~lng, lat = ~lat)
  })
}

shinyApp(ui, server)