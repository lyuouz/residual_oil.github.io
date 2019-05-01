library(shiny)
library(leaflet)
library(shinydashboard)
library(sf)
library(stringr)
library(tidyverse)
library(BAMMtools)
library(RColorBrewer)

nyc_tract <- st_read(dsn = '/Users/zhanglvou/Desktop/GoMailman/research/number_boilers/fuel_data/data/so2_abs_lag', layer = 'model_so2_abs_lag') %>%
  rename(
    'PM2.5_2011' = 'm__2012',
    'PM2.5_2016' = 'm__2016',
    'SO2_2011' = 'm_2_2012',
    'SO2_2016' = 'm_2_2016',
    'SO2_change' = 'so2_dff',
    'PM2.5_change' = 'pm_diff',
    'yearbuilt' = 'avg_yer'
  )

ui <- navbarPage('Explore heating oil and air pollution data', id = 'ro_air',
           
           tabPanel(title = 'Descriptive',
                  
                    absolutePanel(
                         selectInput("desc_var", label = "Select a variable from the list", choices = names(nyc_tract), selected = nyc_tract$PM2.5_2011)
                                  
                    ),
                    leafletOutput('map', width = "100%", height = 400)
  
            
                    ),
           tabPanel(title = 'Results')
           
)

server <- function(input, output){

  new_map <- reactive({
    nyc_tract %>% 
      select(geoid, input$desc_var) %>% 
      rename(selected_var = input$desc_var) %>% 
      as_Spatial()

  })
  
  bins <- reactive({
    getJenksBreaks(new_map()$selected_var, 5)
  })
  
  
  
  pal <- colorNumeric('Greens', domain = c(0, 15))
    
    

  
  output$map <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lat = 40.7128, lng = -74.00600, zoom = 10) %>% 
      addPolygons(data = as_Spatial(nyc_tract))
      
    
  })
  
  observe({
    
    
    leafletProxy("map", data = new_map()) %>%
      addPolygons(
        
        fillColor = pal(new_map()$selected_var),
        weight = 1,
        fillOpacity = 1,
        highlight = highlightOptions(
          weight = 4,
          color = "#666",
          fillOpacity = 0.3,
          bringToFront = TRUE))
    
    
    
  })
  
  
}

shinyApp(ui, server)

