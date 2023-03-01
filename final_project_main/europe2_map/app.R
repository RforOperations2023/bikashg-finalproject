library(shiny)
library(leaflet)
library(DT)
library(rgdal)
library(dplyr)
library(shinydashboard)

amsterdam <- read.csv('amsterdam_weekdays.csv') 

ui <-dashboardPage(
  skin='red',
  dashboardHeader(title='Amsterdam AirBnb Data'),
  dashboardSidebar(
    sliderInput('price_range', label = 'Price Range',
                min = min(amsterdam$realSum),
                max = max(amsterdam$realSum),
                value = c(min(amsterdam$realSum), max(amsterdam$realSum)),
                step=1
  ),
  
  dashboardBody(
    fluidRow(box(width=12, leafletOutput(outputId="mymap"))),
    fluidRow(box(width=12, dataTableOutput(outputId ="summary_table")))
  )
  
))

server <-function(input,output) {
  
  data_input <- reactive({
    amsterdam %>% 
      filter(realSum >= input$price_range[1]) %>% 
      filter(realSum <= input$price_range[2])
  })
  
 output$mymap <- renderLeaflet({
   leaflet() %>% 
     addProviderTiles(providers$OpenStreetMap) %>% 
     addCircles(data = amsterdam, lat = ~lat, lng= ~lng)
 })
    
 output$summary_table <- renderDataTable(data_input())
  
}

shinyApp(ui = ui, server = server)

