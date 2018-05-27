library(shiny)
library(leaflet)
library(plotly)
library(dplyr)

source("process.R")

# The server is a function that takes
# `input` and `output` arguments
server <- function(input, output) {

  #---------------------------------------------------------------------------------
# This is for admission rate Panel
 output$college_names <- renderUI({
   statewise_college <- unique(filter(admission, 
                                      State.Postcode == state.abb[match(input$state, state.name)])$Institution.Name)
   statewise_college <- statewise_college[order(statewise_college)]
   selectInput("school", label = "College Option", c(Choose = 'All', as.list(statewise_college)), selectize = FALSE)
 })
  
  filtered <- reactive({
    admission_table_select <- admission %>%
      select(Institution.Name, State.Postcode, Year, Admission.Rate) %>%
      filter(Year >= input$year[[1]], Year <= input$year[[2]]) %>%
      filter(State.Postcode == state.abb[match(input$state,state.name)]) %>%
      filter(Institution.Name == input$school)
    return(admission_table_select)
  })
  
  
  output$admission_table <- renderDataTable({
    if(input$state == 'All' & input$school == 'All') {
      admission_table_select <- admission %>%
        select(Institution.Name, State.Postcode, Year, Admission.Rate)
      admission_table_select
    } else if(input$state != 'All' & input$school == 'All') {
      admission_table_select <- admission %>%
        select(Institution.Name, State.Postcode, Year, Admission.Rate) %>%
        filter(Year >= input$year[[1]], Year <= input$year[[2]]) %>%
        filter(State.Postcode == state.abb[match(input$state,state.name)])
      admission_table_select
      } else{
        filtered()
      }
  })
  
  output$admission_rate_ui <- renderUI({
    if(input$school == 'All'){
      return(h4(strong("Please choose a school first.")))
    } else {
    plotlyOutput("admission_rate_plot")
    }
  })

  # Admission Rate Plot
   output$admission_rate_plot <- renderPlotly({
     plot1 <- ggplot(data = filtered(), mapping = aes(x = Year, y = Admission.Rate)) +
       geom_point(aes()) +
       geom_line(aes()) +
       labs(
         title = "Admission Rate vs. Year",
         x = "Year",
         y = "Admission Rate"
       )
     plot1 <- ggplotly(plot1)
   })
#---------------------------------------------------------------------------------
# This is for SAT Panel
  filtered_2 <- reactive({
    SAT_table_select <- admission %>%
      select(Institution.Name, State.Postcode, Year, Avg.SAT) %>%
      filter(Year >= input$year[[1]], Year <= input$year[[2]]) %>%
      filter(State.Postcode == state.abb[match(input$state,state.name)]) %>%
      filter(Institution.Name == input$school)
    return(SAT_table_select)
  })
  
  
  output$SAT_table <- renderDataTable({
    if(input$state == 'All' & input$school == 'All') {
      SAT_table_select <- admission %>%
        select(Institution.Name, State.Postcode, Year, Avg.SAT)
      SAT_table_select
    } else if(input$state != 'All' & input$school == 'All') {
      SAT_table_select <- admission %>%
        select(Institution.Name, State.Postcode, Year, Avg.SAT) %>%
        filter(Year >= input$year[[1]], Year <= input$year[[2]]) %>%
        filter(State.Postcode == state.abb[match(input$state,state.name)])
      SAT_table_select
    } else{
      filtered_2()
    }
  })
  
  output$SAT_ui <- renderUI({
    if(input$school == 'All'){
      return(h4(strong("Please choose a school first.")))
    } else {
      plotlyOutput("SAT_plot")
    }
  })
  
  output$SAT_plot <- renderPlotly({
    plot2 <- ggplot(data = filtered_2(), mapping = aes(x = Year, y = Avg.SAT)) +
      geom_point(aes()) +
      geom_line(aes()) +
      labs(
        title = "Average SAT Score vs. Year",
        x = "Year",
        y = "Average SAT Score"
      )
    plot2 <- ggplotly(plot2)
  })

  #---------------------------------------------------------------------------------
  # generate the map
  output$map <- renderLeaflet({
    # coming soon
    
    return(leaflet() %>% 
             addTiles() 
             )
  })
}

shinyServer(server)
