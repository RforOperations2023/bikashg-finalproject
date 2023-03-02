library(httr)
library(sf)
library(leaflet)

t <- GET("https://api.data.amsterdam.nl/v1/winkelgebieden/winkelgebieden/?_format=geojson&categorienaam=Buurtcentrum")
t2 <- st_read(content(t, "text"))
plot(t2)

#leaflet(t2) %>% addProviderTiles(providers$OpenStreetMap) %>% addPolygons()


leaflet(t2) %>% addProviderTiles(providers$OpenStreetMap) %>% 
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = 'red', # ~colorQuantile("YlOrRd"), #ALAND)(ALAND),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))

# reactive ({ filter == api thing}), observer(addPolygon)
#####

library(shiny)
library(leaflet)
library(httr)

ui <- fluidPage(
  textInput("name", "Name", ""),
  sliderInput("age", "Age", min = 0, max = 100, value = 50),
  actionButton("submit", "Submit")
)

server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet() %>% addTiles()
  })
  
  observeEvent(input$submit, {
    url <- paste0("https://api.data.amsterdam.nl/v1/winkelgebieden/winkelgebieden/?_format=geojson", "&categorienaam=", 'Buurtcentrum')
    response <- GET(url)
    data <- content(response)
    leafletProxy("map") %>% addPolygons()
  })
}

shinyApp(ui, server)
In this example, the user interface includes a text input for the user to input a name and a slider input for the user to input an age. When the user clicks the "Submit" button, the observeEvent function constructs a URL for the API call using the user input, makes the API call using the GET function from the httr package, and plots the resulting data on the Leaflet map using the addMarkers function.


url
