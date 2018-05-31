# Extract data set from `process.R`
source("process.R")

# The server is a function that takes `input` and `output` arguments
server <- function(input, output) {
  #---------------------------------------------------------------------------------
# This is for school search Panel
  output$college_names <- renderUI({
    statewise_college <- unique(filter(admission, 
                                       State.Postcode == state.abb[match(input$state, state.name)])$Institution.Name)
    statewise_college <- statewise_college[order(statewise_college)]
    selectInput('school', label = "College Option (Select or Type)", 
                choices =  c("", as.list(statewise_college)), 
                multiple = F, selected = F)
  })
  
  output$college_names_2 <- renderUI({
    all_college_names <- unique(admission$Institution.Name)
    all_college_names <- all_college_names[order(all_college_names)]
    selectInput('school_2', label = "Type in one College name", 
                choices =  c("", as.list(all_college_names)), 
                multiple = F, selected = F)
  })
  
  filtered_for_college_name_2 <- reactive({
    admission_table_select_2 <- admission %>%
      select(Institution.Name, State.Postcode, Year, Admission.Rate) %>%
      filter(Year >= input$year[[1]], Year <= input$year[[2]]) %>%
      filter(Institution.Name == input$school_2)
    return(admission_table_select_2)
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
    if(input$state == '' & input$school == '' & input$school_2 == '') {
      admission_table_select <- admission %>%
        filter(Year >= input$year[[1]], Year <= input$year[[2]]) %>%
        select(Institution.Name, State.Postcode, Year, Admission.Rate)
      admission_table_select
    } else if(input$state != '' & input$school == '' & input$school_2 == '') {
      admission_table_select <- admission %>%
        select(Institution.Name, State.Postcode, Year, Admission.Rate) %>%
        filter(Year >= input$year[[1]], Year <= input$year[[2]]) %>%
        filter(State.Postcode == state.abb[match(input$state,state.name)])
      admission_table_select
      } else if(input$state != '' & input$school != '' & input$school_2 == '') {
        filtered()
      } else if(input$state == '' & input$school == '' & input$school_2 != '') {
        filtered_for_college_name_2()
      }
  })
  
  output$admission_rate_ui <- renderUI({
    if(input$school == '' & input$school_2 == ''){
      return(h4(strong("Please choose a school first.")))
    } else if(input$school != '' & input$school_2 == '') {
      plotlyOutput("admission_rate_plot")
    } else if(input$school == '' & input$school_2 != '') {
      plotlyOutput("admission_rate_plot_2")
    } else {
      return(h4(strong("Selection conflict: You can only use one selection method.")))
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
   
   output$admission_rate_plot_2 <- renderPlotly({
     plot2 <- ggplot(data = filtered_for_college_name_2(), mapping = aes(x = Year, y = Admission.Rate)) +
       geom_point(aes()) +
       geom_line(aes()) +
       labs(
         title = "Admission Rate vs. Year",
         x = "Year",
         y = "Admission Rate"
       )
     plot2 <- ggplotly(plot2)
   })
   
#---------------------------------------------------------------------------------
# This is for SAT Panel
  filtered_for_college_name_SAT <- reactive({
     SAT_table_select_2 <- admission %>%
       select(Institution.Name, State.Postcode, Year, Avg.SAT) %>%
       filter(Year >= input$year[[1]], Year <= input$year[[2]]) %>%
       filter(Institution.Name == input$school_2)
     return(SAT_table_select_2)
   })

  filtered_2 <- reactive({
    SAT_table_select <- admission %>%
      select(Institution.Name, State.Postcode, Year, Avg.SAT) %>%
      filter(Year >= input$year[[1]], Year <= input$year[[2]]) %>%
      filter(State.Postcode == state.abb[match(input$state,state.name)]) %>%
      filter(Institution.Name == input$school)
    return(SAT_table_select)
  })
  
  
  output$SAT_table <- renderDataTable({
    if(input$state == '' & input$school == '' & input$school_2 == '') {
      SAT_table_select <- admission %>%
        filter(Year >= input$year[[1]], Year <= input$year[[2]]) %>%
        select(Institution.Name, State.Postcode, Year, Avg.SAT)
      SAT_table_select
    } else if(input$state != '' & input$school == '' & input$school_2 == '') {
      SAT_table_select <- admission %>%
        select(Institution.Name, State.Postcode, Year, Avg.SAT) %>%
        filter(Year >= input$year[[1]], Year <= input$year[[2]]) %>%
        filter(State.Postcode == state.abb[match(input$state,state.name)])
      SAT_table_select
    } else if(input$state != '' & input$school != '' & input$school_2 == ''){
      filtered_2()
    } else if(input$state == '' & input$school == '' & input$school_2 != '') {
      filtered_for_college_name_SAT()
    }
  })
  
  
  output$SAT_ui <- renderUI({
    if(input$school == '' & input$school_2 == '') {
      return(h4(strong("Please choose a school first.")))
    } else if(input$school != '' & input$school_2 == '') {
      plotlyOutput("SAT_plot")
    } else if(input$school == '' & input$school_2 != '') {
      plotlyOutput("SAT_plot_2")
    } else {
      return(h4(strong("Selection conflict: You can only use one selection method.")))
    }
  })
  
  output$SAT_plot <- renderPlotly({
    plot3 <- ggplot(data = filtered_2(), mapping = aes(x = Year, y = Avg.SAT)) +
      geom_point(aes()) +
      geom_line(aes()) +
      labs(
        title = "Average SAT Score vs. Year",
        x = "Year",
        y = "Average SAT Score"
      )
    plot3 <- ggplotly(plot3)
  })

  output$SAT_plot_2 <- renderPlotly({
    plot4 <- ggplot(data = filtered_for_college_name_SAT(), mapping = aes(x = Year, y = Avg.SAT)) +
      geom_point(aes()) +
      geom_line(aes()) +
      labs(
        title = "Average SAT Score vs. Year",
        x = "Year",
        y = "Average SAT Score"
      )
    plot4 <- ggplotly(plot4)
  })
  
  #---------------------------------------------------------------------------------
  # This is for admission plot Panel
  output$admission_plot_page <- renderPlotly({
    plot_ly(admission, x = ~Avg.SAT, y = ~Admission.Rate,
            text = ~paste("School: ", Institution.Name,'\nYear:', Year, '\nCity:', City, '\nState:', State.Postcode),
            color = ~Avg.SAT, size = ~Avg.SAT
            )
  })
  
  
  #---------------------------------------------------------------------------------
  # This is for the cost page

 output$filter_cost_table <- renderDataTable({
    for (i in 1:nrow(cost_page_tution)) {
      # International student or Private Schools
      if (input$state_for_cost == '' | cost_page_tution$school_type[i] == "private") {
        cost_page_tution$your_tuition[i] <- cost_page_tution$Out_State_Tuition[i]
      } 
      # US citizen
      else {
        if(cost_page_tution$school_type[i] == "public" & 
           cost_page_tution$State[i] != state.abb[match(input$state_for_cost,state.name)]) 
        {
          cost_page_tution$your_tuition[i] <- cost_page_tution$Out_State_Tuition[i]
        }
        else
        {
          cost_page_tution$your_tuition[i] <- cost_page_tution$In_State_Tuition[i]
        }
      }
    }
   cost_page_tution_select <- cost_page_tution %>%
     filter(your_tuition > input$tuition_slider[[1]], your_tuition < input$tuition_slider[[2]])
   return(cost_page_tution_select)
 })
   
  
  
 
  
  #---------------------------------------------------------------------------------
  # generate the maps
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
  
  ## add the summary table with hyperlink
  # reactive the value for output
  map_table <- reactive({

      filtered <- getState(input$state_map) %>%
        select(UnitID, Institution.Name, Institution.URL, Avg.SAT, Admission.Rate, Open.Admissions.Policy)

      filtered$Institution.URL <- paste0("<a href='",filtered$Institution.URL,"'>", 
                                         filtered$Institution.URL,"</a>")

    return(filtered)
  })

  # output the table
  output$maptable <- DT::renderDataTable({
      
      DT::datatable(map_table(), escape = FALSE)

  })
  
  #-----------------------------------------------------------------------------------
  # Diversity data
  output$state_output <- renderUI({
    citywise <- unique(filter(diversity_data,
                               state == input$state)$City)
    # citywise <- citywise[order(citywise)]
    selectInput('city', label = "City Option (Select or Type)", 
                choices =  citywise, 
                multiple = F, selected = F)
  })
  
  filtered_city <- reactive({
    diversity_table <- diversity_data %>%
      select(Institution.Name, City, state, Year, 
             `Percent.1st-generation`, Total.Enrolled.Men, Total.Enrolled.Women) %>%
      filter(Year >= input$year[[1]], Year <= input$year[[2]]) %>%
      filter(state == input$state) %>%
      filter(City == input$city)
    return(diversity_table)
  })
  
  filtered_state <- reactive({
    state_table <- diversity_data %>%
      select(Institution.Name, City, state, Year, 
             `Percent.1st-generation`, Total.Enrolled.Men, Total.Enrolled.Women) %>%
      filter(Year >= input$year[[1]], Year <= input$year[[2]]) %>%
      filter(state == input$state)
    return(state_table)
  })
  
  # output$menwomen <- renderPlotly({
  #   plot5 <- ggplot(data = filtered_city(), na.rm = TRUE, mapping = aes(x = Institution.Name, y = Total.Enrolled.Men)) +
  #     geom_point(aes()) +
  #     geom_line(aes()) +
  #     labs(
  #       title = "Admission Rate vs. Year",
  #       x = "Year",
  #       y = "Admission Rate"
  #     )
  #   plot5 <- ggplotly(plot5)
  # })
  
  output$diversity_ui <- renderUI({
    if(input$state  == '' & input$city == ''){
      return(h4(strong("Please choose a State first.")))
    } else if(input$state != '' & input$city == '') {
      plotlyOutput("diversity_state_plot")
    } else if(input$state == '' & input$city != '') {
      plotlyOutput("diversity_city_plot")
    } else {
      return(h4(strong("Selection conflict: You can only use one selection method.")))
    }
  })
  
  output$diversity_state_plot <- renderPlotly({
    plot5 <- ggplot(data = filtered_state(), mapping = aes(x = Institution.Name, y = Total.Enrolled.Men)) +
      geom_point(aes()) +
      geom_line(aes()) +
      labs(
        title = "Admission Rate vs. Year",
        x = "Year",
        y = "Admission Rate"
      )
    plot5 <- ggplotly(plot5)
  })
  
  output$diversity_city_plot <- renderPlotly({
    plot6 <- ggplot(data = filtered_city(), mapping = aes(x = Institution.Name, y = Total.Enrolled.Men)) +
      geom_point(aes()) +
      geom_line(aes()) +
      labs(
        title = "Admission Rate vs. Year",
        x = "Year",
        y = "Admission Rate"
      )
    plot6 <- ggplotly(plot6)
  })
  
}

shinyServer(server)