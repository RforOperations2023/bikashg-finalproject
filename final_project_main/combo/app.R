library(shiny)
library(leaflet)
library(RColorBrewer)
library(dplyr)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(stringr)
library(DT)

# To Dos:
# 1. One leaflet map requirements: (?) 
# 2. download HTML is not working 
# 3. tethering scatter plot to price range

# 2. Clarify what two different types of layer is sought
# 3. Is use of the leaflet proxy sufficient in this case?
# 4. Why am I downloading in HTML? 
# 5. Extra credit: Students who use one or more API's to feed either their map or 
# data displayed will receive up to 20 Bonus points on the assignment. Will I have to redo?
# 6. Can I keep everything same? Just import new data.
# 7. I need extra points. How much of the bump would the extra credit bring to me? 

#====

## TO DOS
# 1. I can have multiple amsterdam json file from this website:
# https://maps.amsterdam.nl/open_geodata/?LANG=en
# 2. I can create multiple map object
# 3. I can add them leaflet() %>% addCircles or addPolygons 
# 4. I can create interactive layer 
# 5. Clean the dashboard
# 6. If possible, add a photo (see google chat)

# To get various json file 

amsterdam <- read.csv('amsterdam_weekdays.csv') %>% 
  mutate(realSum = round(realSum,0)) %>% 
  filter(realSum <= 4000)

# market 
#t1 <- GET("https://maps.amsterdam.nl/open_geodata/geojson_lnglat.php?KAARTLAAG=MARKTEN&THEMA=markten")

# world heritage site: 
t1 <- GET("https://maps.amsterdam.nl/open_geodata/geojson_lnglat.php?KAARTLAAG=UNESCO&THEMA=cultuurhistorie")

#t_list = list(t0, t1)

#t <- GET("https://api.data.amsterdam.nl/v1/winkelgebieden/winkelgebieden/?_format=geojson&categorienaam=Buurtcentrum")
t2 <- st_read(content(t1, "text"))
#plot(t2)

# Here is what I can do, [This does not work]
# have multiple links name: t1, t2, t3, t4, t5
# select input, or drop down menu, where input = c(t1, t2, t3, t4)
# t2 <- st_read(content(input$fromabove, "text"))

#leaflet(t2) %>% addProviderTiles(providers$OpenStreetMap) %>% addPolygons()


ui <- dashboardPage(
  dashboardHeader(title = "Amsterdam AirBnb Price Dashboard", titleWidth = 350,
                   
                   # Drop down menu with hard coded values ------------------------------
                   dropdownMenu(type = "notifications",
                                notificationItem(text = "Check your upcoming trip to Amsterdam!", 
                                                 icon = icon("globe")),
                                notificationItem(text = "Ryan Air Best Deals",
                                                 icon = icon("plane")),
                                notificationItem(text = "Hiking spots in Belgium",
                                                 icon = icon("person-hiking"),
                                                 status = "warning")
                   ),
                   dropdownMenu(type = "tasks", badgeStatus = "success",
                                taskItem(value = 90, color = "green",
                                         "Hotel Reservations"),
                                taskItem(value = 50, color = "aqua",
                                         "Salsa in Spain"),
                                taskItem(value = 32, color ="red",
                                         "Credit Card payment")
                   ),
                   dropdownMenu(type = "messages",
                                messageItem(
                                  from = "Meylin Gonzales",
                                  message = HTML("Welcome to our Airbnb"),
                                  icon = icon("house")),
                                messageItem(
                                  from = "Couch Surfing",
                                  message = "Tell us about your experience",
                                  icon = icon("couch")
                                ),
                                messageItem(
                                  from = 'BBC',
                                  message = 'Read about this new island',
                                  icon = icon("newspaper")
                                )
                   )
  ),
  dashboardSidebar(width = 350, 
    sidebarMenu(id = "tabs",
    
    # Menu Items ----------------------------------------------
    menuItem("Source Code", icon=icon("code"), 
             href="https://github.com/RforOperations2023/bikashg-finalproject")),
    
    sliderInput("range", "Price Range", min(amsterdam$realSum), max(amsterdam$realSum),
                value = range(amsterdam$realSum), step = 1),
    
    selectInput("colors", "Choose color schemes for your Legend:",
                rownames(subset(brewer.pal.info, category %in% c("seq", "div")))),
    
    #selectInput('maps_choice', "sketch your map",
     #           choices = c('t0', 't1')),
    
    checkboxInput("showPolygons", label='Show Polygons', value=TRUE),
    
    hr(),
    selectInput("x_var", "Select room/place characteristics", 
                choices = c("Distance from the city center" = "dist",
                            "Cleanliness Rating" = "cleanliness_rating", 
                            "Guess Satisfaction Overall" = "guest_satisfaction_overall",
                            "Number of Bedrooms" = "bedrooms", 
                            "Distance from the nearest metro station" = "metro_dist"
                )),
    
    ####========
    actionButton("submit", "Submit"),
    ###==========
    
    # Input y-variable: selecting an input for y-axis (scatter plot)
    selectInput("y_var", "Select the default price column", 
                choices = c("AirBnb Price" = "realSum")),
    checkboxInput("legend", "Show legend", TRUE)
  ),
  dashboardBody(
    tabsetPanel(
      tabPanel("Map", leafletOutput("map", width = "100%", height = "650px")),
      tabPanel("Map2", leafletOutput("map2", width = "100%", height = "650px")),
      tabPanel("Scatterplot", plotlyOutput("scatterplot")),
      tabPanel(" Room Type Pie-chart", plotlyOutput("piechart")),
      tabPanel("Data Table", dataTableOutput("table"),
               downloadButton('downloadData', 'Download Data'))
      

    )
    
  )
)

server <- function(input, output) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    amsterdam[amsterdam$realSum >= input$range[1] & amsterdam$realSum <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  
  colorpal <- reactive({
    colorNumeric(input$colors, filteredData()$realSum)
  })
  
  output$map <- renderLeaflet({

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
      addCircles(weight = 10, 
                 color = ~pal(realSum), fillOpacity = 0.9, 
                 popup = ~paste('Price:','$',round(realSum,0),
                                '<br>', 'Distance From the City Center: ', round(dist,2),'km',
                                '<br>', 'Distance From the Nearest Metro: ',
                                round(metro_dist,2),'km') 
      ) %>%
      addMarkers(clusterOptions = markerClusterOptions()) %>% 
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldStreetMap, group = "Esri") %>%
      addLayersControl(
        baseGroups = c("OSM (default)", "Esri")) #,
       # #overlayGroups = c("Quakes", "Outline"),
       # options = layersControlOptions(collapsed = FALSE)) 
  })
  
  #===========
  
  output$map2 <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      addPolygons(data = t2, fillColor = "red", fillOpacity = 0.5, weight = 2, popup = ~as.character('gebiedsnaam'), group = "Polygons") %>%
      addCircleMarkers(data = filteredData(), weight =1, color = "blue", stroke = FALSE, fillOpacity = 0.8, popup = ~as.character(realSum), group = "Points") %>%
      addLayersControl(overlayGroups = c("Polygons", "Points"), options = layersControlOptions(collapsed = FALSE))
    
    #leaflet(t2) %>% addProviderTiles(providers$OpenStreetMap) %>% 
      #addCircleMarkers()
      #addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
       #           opacity = 1.0, fillOpacity = 0.5,
        #          fillColor = 'red', # ~colorQuantile("YlOrRd"), #ALAND)(ALAND),
         #         highlightOptions = highlightOptions(color = "white", weight = 2,
          #                                            bringToFront = TRUE))
  })
  
  
  
  ##==================
  
  #observeEvent(input$submit, {
   # url <- paste0("https://api.data.amsterdam.nl/v1/winkelgebieden/winkelgebieden/?_format=geojson", "&categorienaam=", 'Buurtcentrum')
    #response <- GET(url)
    #data_map <- st_read(content(response, "text"))
    
    #data_map <- content(response)
    #leafletProxy("map2") %>% addPolygons(data=data_map)
    #data_map %>% leafletProxy("map2") %>% setView(lng=52.3676, lat=4.9041) %>% addPolygons()
  #})

  
  #===============
  
  
  # Scatterplot output
  output$scatterplot <- renderPlotly({
    ggplot(filteredData(), aes_string(x = input$x_var, y = input$y_var)) + 
      geom_point() + labs(x = str_replace_all(input$x_var, "[.]", " "),
                          y = str_replace_all(input$y_var, "[.]", " ")) 
  })
  
  
  output$piechart <- renderPlotly({
    pie_data <- filteredData() %>%
      group_by(room_type) %>%
      summarise(n = n(), avg_price = mean(realSum, na.rm=TRUE)) # initially `realSum`
    
    plot_ly(pie_data, labels = ~room_type, values = ~n, type = "pie")
    
  })
  
  
  # Data table of places ----------------------------------------------
  output$table <- DT::renderDataTable({
    subset(filteredData(), select = c(room_type, 
                                 realSum, 
                                 bedrooms,
                                 dist,
                                 metro_dist,
                                 lng,
                                 lat
    ))
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="_")
    },
    content = function(file){
      write.csv(filteredData(), file, row.names=FALSE)
    }
  ) 
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = filteredData())
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~realSum, title = 'AirBnb Price'
      )
    }
  })
}

shinyApp(ui, server)