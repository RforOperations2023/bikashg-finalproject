library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)
library(leaflet)


# Load and clean data ----------------------------------------------

amsterdam <- read.csv('amsterdam_weekdays.csv')

amsterdam <- amsterdam %>%
  mutate(room_type = as.factor(room_type),
         room_shared = as.factor(room_shared))

# Avoid plotly issues ----------------------------------------------
pdf(NULL)


# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Amsterdam AirBnb Price Dashboard", titleWidth = 350,
                          
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
)

# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(width=350,
                            sidebarMenu(
                              id = "tabs",
                              
                              # Menu Items ----------------------------------------------
                              menuItem("Data Viz", icon = icon("chart-column"), tabName = "plot"),
                              menuItem("Table", icon = icon("table"), tabName = "table", badgeLabel = "new", 
                                       badgeColor = "green"),
                              menuItem("Source Code", icon=icon("code"), 
                                       href="https://github.com/RforOperations2023/bikashg-finalproject"),
                              
                              
                              # GDP per Capita Range Selection ----------------------------------------------
                              sliderInput("dist",
                                          "Choose the distance from the airport:",
                                          min = min(amsterdam$dist, na.rm = T),
                                          max = max(amsterdam$dist, na.rm = T),
                                          value = c(min(amsterdam$dist, na.rm = T), 
                                                    max(amsterdam$dist, na.rm = T)),
                                          step = 0.5),
                              
                              # Inputs: Narrow down or expand your geographical regions -------------------------
                              selectInput("room",
                                          "Select Room Type: ",
                                          choices = sort(unique(amsterdam$room_type)),
                                          multiple = FALSE,
                                          selectize = TRUE,
                                          selected = c("Private room")),
                              
                              hr(),
                              
                              # Input x-variable: selecting an input for x-axis (scatter plot)
                              selectInput("x_var", "Select room/place characteristics", 
                                          choices = c("Cleanliness Rating" = "cleanliness_rating", 
                                                      "Guess Satisfaction Overall" = "guest_satisfaction_overall",
                                                      "Number of Bedrooms" = "bedrooms", 
                                                      "Distance from the city center" = "dist",
                                                      "Distance from the nearest metro station" = "metro_dist"
                                          )),
                              
                              # Input y-variable: selecting an input for y-axis (scatter plot)
                             selectInput("y_var", "Select the default price column", 
                                         choices = c("AirBnb Price" = "realSum"))
                             
                          #   selectInput("attr", "Select attribute to map", 
                           #              choices = c("Cleanliness Rating" = "cleanliness_rating", 
                            #                         "Guess Satisfaction Overall" = "guest_satisfaction_overall",
                             #                        "Number of Bedrooms" = "bedrooms", 
                              #                       "Distance from the city center" = "dist",
                               #                      "Distance from the nearest metro station" = "metro_dist"
                                #         )),
                              
                            )
)

# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(
  
  # Plot page ----------------------------------------------
  tabItem("plot",
          
          
          # Plot ----------------------------------------------
          fluidRow(
            tabBox(title = "Data Visualization",
                   width = 12,
                    #tabPanel(leafletOutput('map')),
                    tabPanel("Scatterplot", plotlyOutput("scatterplot")),
                  # tabPanel("Food Production", plotlyOutput("plot_food")),
                  # tabPanel("Women Leaders", plotlyOutput("plot_female")),
                    tabPanel(" Room Type Pie-chart", plotlyOutput("piechart")))
          )
  ),
  
  # Data Table Page ----------------------------------------------
  tabItem("table",
          fluidPage(
            box(title = "Selected AirBnB", DT::dataTableOutput("table"), width = 12)),
          
          fluidPage(downloadButton('download_data', 'Dowload Data'))
  )
  

  
)
)

ui <- dashboardPage(header, sidebar, body, skin='purple')

# Define server function required to create plots and value boxes -----
server <- function(input, output) {
  
  #data <- reactive({
   # switch(input$x_var,
    #       "Cleanliness Rating" = "cleanliness_rating", 
     #      "Guess Satisfaction Overall" = "guest_satisfaction_overall",
      #     "Number of Bedrooms" = "bedrooms", 
       #    "Distance from the city center" = "dist",
        #   "Distance from the nearest metro station" = "metro_dist")
#  })
  
  # Create the map
 # output$map <- renderLeaflet({
  #  leaflet() %>%
   #   addTiles() %>%
    #  addProviderTiles(providers$OpenStreetMap) %>% 
     # addCircles(data = amsterdam, 
      #            fillColor = colorNumeric(palette = "Reds", domain = data())(data()),
       #           stroke = FALSE,
        #          #smoothFactor = 0.2,
         #         fillOpacity = 0.7,
          #        highlightOptions = highlightOptions(color = "white", weight = 2,
           #                                           bringToFront = TRUE))
#  })
  
  
  # Reactive data function -------------------------------------------
  swInput <- reactive({
    amsterdam_sub <- amsterdam %>%
      
      # GDP Per Capita Filter ----------------------------------------------
    filter(dist >= input$dist[1],
           dist <= input$dist[2]) # earlier was `dist`
    
    # Region Filter ----------------------------------------------
    if (length(input$room) > 0 ) {
      amsterdam_sub <- subset(amsterdam, room_type %in% input$room)
    }
    
    # Return dataframe ----------------------------------------------
    return(amsterdam_sub)
  })
  
  # Reactive melted data ----------------------------------------------
  mwInput <- reactive({
    swInput() %>%
      melt(id = "room_type") 
  })
  
  
  # A plot showing a scatter plot
  output$scatterplot <- renderPlotly({
    print(summary(swInput()[input$x_var]))
    print(summary(swInput()[input$y_var]))
    
    ggplot(swInput(), aes_string(x = input$x_var, y = input$y_var)) +
      geom_point() + labs(x = str_replace_all(input$x_var, "[.]", " "),
                          y = str_replace_all(input$y_var, "[.]", " ")) 
  })
  
  
  # A plot showing the food production -----------------------------
 # output$plot_food <- renderPlotly({
  #  dat <- subset(mwInput(), variable == "Food.production.index..2004.2006.100.")
    
    # Generate Plot ----------------------------------------------
   # ggplot(data = dat, aes(x = country, y = as.numeric(value), fill = country)) + 
    #  geom_bar(stat = "identity") + labs(y = 'Food Production Index', x = 'Country') + 
     # theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5))
    
#  })
  
  # A plot showing seats held by women in national parliament -----------------------------------
 # output$plot_female <- renderPlotly({
  #  dat <- subset(mwInput(),  variable == "Seats.held.by.women.in.national.parliaments..")
    
   # ggplot(data = dat, aes(x = country, y = as.numeric(value), fill = country)) + geom_bar(stat = "identity") + 
    #  labs(y = 'Seats Held by Women in National Parliament', x = 'Country') + 
     # theme(axis.text.x=element_text(angle=45, hjust=1, vjust=0.5))
#  })
  
  
  # Reactive pie chart based on input values
  output$piechart <- renderPlotly({
    pie_data <- swInput() %>%
      group_by(room_type) %>%
      summarise(n = n(), avg_price = mean(realSum, na.rm=TRUE)) # initially `realSum`
    
    plot_ly(pie_data, labels = ~room_type, values = ~avg_price, type = "pie")
    
  })
  
  
  # Data table of country ----------------------------------------------
  output$table <- DT::renderDataTable({
    subset(swInput(), select = c(room_type, 
                                 realSum, 
                                 bedrooms,
                                 dist,
                                 metro_dist,
                                 lng,
                                 lat
    ))
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      write.csv(swInput(), file)
    }
  )
  
  
}

  
# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)