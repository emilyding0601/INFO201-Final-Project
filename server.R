library(shiny)
library(leaflet)
source("process.R")

# The server is a function that takes
# `input` and `output` arguments
server <- function(input, output) {
  # use values from `input` list
  # assign values to `output` list
  # we'll fill this in soon
  
  # generate the map
  output$map <- renderLeaflet({
    # coming soon
    
    return(leaflet() %>% 
             addTiles() 
             )
  })
}

shinyServer(server)
