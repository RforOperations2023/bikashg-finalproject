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
# 1. Clean this one more time before the class
# 2. Clarify what two different types of layer is sought
# 3. Is use of the leaflet proxy sufficient in this case?
# 4. Why am I downloading in HTML? 
# 5. Extra credit: Students who use one or more API's to feed either their map or 
# data displayed will receive up to 20 Bonus points on the assignment. Will I have to redo?
# 6. Can I keep everything same? Just import new data.
# 7. I need extra points. How much of the bump would the extra credit bring to me? 


amsterdam <- read.csv('amsterdam_weekdays.csv') %>% 
  mutate(realSum = round(realSum,0))

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
    selectInput("colors", "Color Scheme",
                rownames(subset(brewer.pal.info, category %in% c("seq", "div")))),
    hr(),
    selectInput("x_var", "Select room/place characteristics", 
                choices = c("Cleanliness Rating" = "cleanliness_rating", 
                            "Guess Satisfaction Overall" = "guest_satisfaction_overall",
                            "Number of Bedrooms" = "bedrooms", 
                            "Distance from the city center" = "dist",
                            "Distance from the nearest metro station" = "metro_dist"
                )),
    
    # Input y-variable: selecting an input for y-axis (scatter plot)
    selectInput("y_var", "Select the default price column", 
                choices = c("AirBnb Price" = "realSum")),
    checkboxInput("legend", "Show legend", TRUE)
  ),
  dashboardBody(
    tabsetPanel(
      tabPanel("Map", leafletOutput("map", width = "100%", height = "650px")),
      tabPanel("Scatterplot", plotlyOutput("scatterplot")),
      tabPanel(" Room Type Pie-chart", plotlyOutput("piechart")),
      tabPanel("Data Table", dataTableOutput("table"),
               downloadButton('dowloadData', 'Download Data'))
      

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
      addCircles(weight = 10, color = "#777777",
                 fillColor = ~pal(realSum), fillOpacity = 0.9, popup = ~paste('Price:','$',round(realSum,0),
                                                                              '<br>', 'Distance From the City Center: ', round(dist,2),'km',
                                                                              '<br>', 'Distance From the Nearest Metro: ', 
                                                                              round(metro_dist,2),'km') 
      ) %>%
      addMarkers(clusterOptions = markerClusterOptions())
  })
  
  
  # Scatterplot output
  output$scatterplot <- renderPlotly({
    ggplot(amsterdam, aes_string(x = input$x_var, y = input$y_var)) + 
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
  ) # why is it downloading in html?
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = amsterdam)
    
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