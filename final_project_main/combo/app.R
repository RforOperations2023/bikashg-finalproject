## Bikash Gupta


# Description of the project: Amsterdam is a popular tourist destination, receiving more than 15 million 
# visitors a year. Thousands of tourists look for AirBnb each day. These tourists also look for relevant information
# regarding housing and sites. This dashboard allows user to navigate the prices of airbnb scattered in the city.
# Additionally, it offers official information on various places such as world heritage sites. It also has
# maps that allows user to find relationships between various variables by playing with widget. 


### ====================== Install the following packages if not already ==================

# uncomment the line to install packages, if not already

# install.packages(c('shiny', 'leaflet', 'RColorBrewer', 'dplyr', 'shinydashboard', 'ggplot2',
#                   'plotly', 'stringr', 'DT', 'httr','rjson', 'sf'))

### =================== Running the libraries ======================
library(shiny)
library(leaflet)
library(RColorBrewer)
library(dplyr)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(stringr)
library(DT)
library(httr)
library(rjson)
library(sf)


###========================== Amsterdam File ================================

amsterdam <- read.csv('amsterdam_weekdays.csv') %>% 
  mutate(realSum = round(realSum,0)) %>% 
  filter(realSum <= 4000)


### ========================== JSON Files =============================


# we will be using these JSON files straight from Amsterdam's local government website:

# historical building
history_building <- GET("https://maps.amsterdam.nl/open_geodata/geojson_lnglat.php?KAARTLAAG=HISTORISCHE_BEBOUWING&THEMA=archeologie")
history_b_json <- st_read(content(history_building, "text"))

# world heritage site
whs <- GET("https://maps.amsterdam.nl/open_geodata/geojson_lnglat.php?KAARTLAAG=UNESCO&THEMA=cultuurhistorie")
whs_json <- st_read(content(whs, "text"))


### =========================== UI Server ===================================


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
    
    # Source Code Link ----------------------------------------------
    
    menuItem("Source Code", icon=icon("code"), 
             href="https://github.com/RforOperations2023/bikashg-finalproject")),
    
    
    # Price Range Slider----------------------------------------------
    
    sliderInput("range", "Choose the AirBnb Price Range", min(amsterdam$realSum), max(amsterdam$realSum),
                value = range(amsterdam$realSum), step = 1),
    
    
    # Drop Down Menu for Color Palette----------------------------------------------
    
    selectInput("colors", "Choose color schemes for your Legend:",
                rownames(subset(brewer.pal.info, category %in% c("seq", "div")))),
    
    
    # A horizontal line for visual distinction----------------------------------------------
    hr(),
    
    h5('For Scatterplot, choose from the following:', align = 'center'),

        
    # X-variable for scatter plot ----------------------------------------------
    
    selectInput("x_var", "Select room/place characteristics: ", 
                choices = c("Distance from the city center" = "dist",
                            "Cleanliness Rating" = "cleanliness_rating", 
                            "Guess Satisfaction Overall" = "guest_satisfaction_overall",
                            "Number of Bedrooms" = "bedrooms", 
                            "Distance from the nearest metro station" = "metro_dist"
                )),
    

    # Y- variable: selecting an input for y-axis (scatter plot)----------------------
    
    selectInput("y_var", "Select the default price column: ", 
                choices = c("AirBnb Price" = "realSum")),
    
    
    # Legend Checkbox --------------------------------------------------
    checkboxInput("legend", "Show legend", TRUE)
    
  ),
  
  ### ---------------------------------Dashboard Body
  
  dashboardBody(
    
    # Altogether 5 Tab Panels: Includes two maps, scatterplot, piechart, and table:
    
    tabsetPanel(
      tabPanel("AirBnB Rooms", leafletOutput("map", width = "100%", height = "650px"),icon=icon('globe')),
      
      tabPanel("Amsterdam Sites", leafletOutput("map2", width = "100%", height = "650px"), icon=icon('city')),
      
      tabPanel("Scatterplot", 
      h4('Select', span(em("from the drop down menus on your left"), style='color:green'), 
      "the variables for the scatterplot. Important to note that price is the only dependent variable."),
      plotlyOutput("scatterplot"), icon=icon('chart-simple')),
      
      tabPanel(" Room Type Pie-chart", 
      h4('Toggle the', span(em("price range widget"), style='color:green'), 
      "on your left to see how the proportions of room type changes."),
      plotlyOutput("piechart"), icon=icon('chart-pie')),
      
      tabPanel("Data Table", dataTableOutput("table"), icon=icon('table'), br(), 
      p('To download the reactive table, please click the download button below.'),
      downloadButton('downloadData', 'Download Data'))

    )
    
  )
)


### ===================================== Server ==============================

server <- function(input, output) {
  
  # Reactive expression for the data subsetted to what the user selected -----------------------
  
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
  
  
  # Incremental changes to the map (in this case, replacing the circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change should be managed in its own observer.
  
  # First Map --------------------------------------------------------------------------
  
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
        baseGroups = c("OSM (default)", "Esri"))
  })
  
  # Second Map --------------------------------------------------------------------------
  
  output$map2 <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      setView(lng = 4.9041,lat =52.3676, zoom=15) %>% 
      addPolygons(data = whs_json, fillColor = "red", fillOpacity = 0.5, weight = 2, popup = ~as.character('UNESCO World Heritage Area'), group = "World Heritage Site") %>%
      addCircles(data = filteredData(), weight =10, color = "blue", stroke = FALSE, fillOpacity = 0.8, 
                 popup = ~paste('Price:','$',round(realSum,0),
                                        '<br>', 'Distance From the City Center: ', round(dist,2),'km',
                                        '<br>', 'Distance From the Nearest Metro: ',
                                        round(metro_dist,2),'km'), group = "AirBnb") %>%
      addCircleMarkers(data = history_b_json, weight =1, color = "green", stroke = TRUE, fillOpacity = 0.8,
                       popup = ~paste('Site Name:', Naam,
                                      '<br>', 'Main Group: ', Hoofdgroep,
                                      '<br>', 'Begin: ', Begin,
                                      '<br>', 'End: ', Eind,
                                      '<br>', 'Selection: ', SELECTIE),group = "Historical Places") %>%
      addLayersControl(overlayGroups = c("World Heritage Site","AirBnb", "Historical Places"), options = layersControlOptions(collapsed = FALSE))
    

  })
  
  
  # Scatterplot ----------------------------------------------------------
  
  output$scatterplot <- renderPlotly({
    ggplot(filteredData(), aes_string(x = input$x_var, y = input$y_var)) + 
      geom_point() + labs(x = str_replace_all(input$x_var, "[.]", " "),
                          y = str_replace_all(input$y_var, "[.]", " ")) 
  })
  
  
  # Pie-chart ----------------------------------------------------------
  
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
  
  # Download data button -------------------------------------------------------
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="_")
    },
    content = function(file){
      write.csv(filteredData(), file, row.names=FALSE)
    }
  ) 
  
  # Use a separate observer to recreate the legend as needed----------------------
  
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