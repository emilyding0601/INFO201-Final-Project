# Extract data set from `process.R`
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
  # generate the maps
  
  # observe({
  # 
  #   filtered_city <- getCity(input$city_map)
  # 
  #   text <- paste("<h4/>", filtered_city$Institution.Name, "<br/>", "<br/>",
  #                 "Enrollment Number: ", filtered_city$Enrollment, "<br/>", 
  #                 "In-state Tuition: ", filtered_city$`In-State.Tuition`, "<br/>",
  #                 "Out-state Tuition: ", filtered_city$`Out-State.Tuition`, "<br/>",
  #                 "Admission Rate: ", filtered_city$Admission.Rate, "<br/>",
  #                 "Average Age: ", round(filtered_city$Avg.Age, 0), "<br/>",
  #                 sep = "") %>% lapply(htmltools::HTML)
  # 
  #   leafletProxy('map') %>% clearMarkers() %>%
  #     addCircleMarkers(lng = filtered_city$Long,
  #                      lat = filtered_city$Lat,
  #                      label = text)
  # 
  # 
  # })

  # observe({
  # 
  #   filtered_state_city <- getState(input$state_map) %>% filter(City == input$city_map)
  # 
  #   text <- paste("<h4/>", filtered_state_city$Institution.Name, "<br/>", "<br/>",
  #                 "Enrollment Number: ", filtered_state_city$Enrollment, "<br/>", 
  #                 "In-state Tuition: ", filtered_state_city$`In-State.Tuition`, "<br/>",
  #                 "Out-state Tuition: ", filtered_state_city$`Out-State.Tuition`, "<br/>",
  #                 "Admission Rate: ", filtered_state_city$Admission.Rate, "<br/>",
  #                 "Average Age: ", round(filtered_state_city$Avg.Age, 0), "<br/>",
  #                 sep = "") %>% lapply(htmltools::HTML)
  # 
  #   leafletProxy('map', data = filtered_state_city) %>% clearMarkers() %>%
  #     addCircleMarkers(lng = filtered_state_city$Long,
  #                      lat = filtered_state_city$Lat,
  #                      label = text)
  # 
  # })
  
  observe({
    
    filtered_state <- getState(input$state_map)
    
    text <- paste(filtered_state$Institution.Name, "<br/>", "<br/>",
                  "Enrollment Number: ", filtered_state$Enrollment, "<br/>", 
                  "In-state Tuition: ", " $", filtered_state$`In-State.Tuition`, "<br/>",
                  "Out-state Tuition: ", " $", filtered_state$`Out-State.Tuition`, "<br/>",
                  "Admission Rate: ", round(filtered_state$Admission.Rate, 2) * 100, "%", "<br/>",
                  "Average Age: ", round(filtered_state$Avg.Age, 0), "<br/>",
                  sep = "") %>% lapply(htmltools::HTML)
    
    leafletProxy('map') %>% clearMarkers() %>% 
      setView(lng = filtered_state$Long[1], lat = filtered_state$Lat[1], zoom = 7) %>% 
      addMarkers(lng = filtered_state$Long,
                       lat = filtered_state$Lat,
                       label = text,
                 labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                          padding = "5px 10px"), 
                                             textsize = "16px", direction = "auto"))
    
  })
  
  # initialize map
  output$map <- renderLeaflet({
    # select state and city data set 
    leaflet() %>% 
      addTiles() %>% 
      setView(lng = -122.3321, lat = 47.6062, zoom = 7) %>% 
      addMarkers(lng = df_2015$Long,
                       lat = df_2015$Lat,
                       label = text,
                 labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                          padding = "5px 10px"), 
                                             textsize = "16px", direction = "auto"))

  })
  
  map_table <- reactive({

      filtered <- getState(input$state_map) %>%
        select(UnitID, Institution.Name, Institution.URL, Avg.SAT)

      filtered$Institution.URL <- paste0("<a href='",filtered$Institution.URL,"'>", 
                                         filtered$Institution.URL,"</a>")

    return(filtered)
  })

  output$maptable <- DT::renderDataTable({
      
    # has a bug: cannot empty the ``selectInput
      DT::datatable(map_table(), escape = FALSE)
    
    
  })
    
}

shinyServer(server)
