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
    plot3 <- ggplot(data = filtered_2(), mapping = aes(x = Year, y = Avg.SAT, color = Avg.SAT)) +
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
    plot4 <- ggplot(data = filtered_for_college_name_SAT(), 
                    mapping = aes(x = Year, y = Avg.SAT, color = Avg.SAT)) +
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
  # This is for school filter plot Panel
  school_filtered <- reactive({
    if(input$state_admission_plot == '') {
      all_data_needed <- admission %>%
        filter(Year >= input$year_admission_plot[[1]], Year <= input$year_admission_plot[[2]]) %>%
        filter(Avg.SAT >= input$SAT_admission_plot[[1]], Avg.SAT <= input$SAT_admission_plot[[2]]) %>%
        filter(Admission.Rate >= input$admission_rate_admission_plot[[1]], Admission.Rate <= input$admission_rate_admission_plot[[2]])
      return(all_data_needed)
    } else {
      all_data_needed <- admission %>%
        filter(Year >= input$year_admission_plot[[1]], Year <= input$year_admission_plot[[2]]) %>%
        filter(Avg.SAT >= input$SAT_admission_plot[[1]], Avg.SAT <= input$SAT_admission_plot[[2]]) %>%
        filter(State.Postcode == state.abb[match(input$state_admission_plot,state.name)]) %>%
        filter(Admission.Rate >= input$admission_rate_admission_plot[[1]], Admission.Rate <= input$admission_rate_admission_plot[[2]]) 
      return(all_data_needed)
    }
  })
  
  output$school_filter <- renderPlotly({
    plot_ly(school_filtered(), x = ~Avg.SAT, y = ~Admission.Rate,
            text = ~paste("School: ", Institution.Name,'\nYear:', Year, '\nCity:', City, '\nState:', State.Postcode),
            color = ~Avg.SAT, size = ~Avg.SAT
    ) %>% layout(title = "Total Average SAT vs.Admission Rate")
  })
  
  #---------------------------------------------------------------------------------
  # This is for the cost page
  
  output$filter_cost_table <- renderDataTable({
    for (i in 1:nrow(cost_page_tution)) {
      # International student or Private Schools
      if (input$state_for_cost == '' | cost_page_tution$Type[i] == "Private") {
        cost_page_tution$Tuition[i] <- cost_page_tution$Out.State[i]
      } 
      # US citizen
      else {
        if(cost_page_tution$Type[i] == "Public" & 
           cost_page_tution$State[i] != state.abb[match(input$state_for_cost,state.name)]) 
        {
          cost_page_tution$Tuition[i] <- cost_page_tution$Out.State[i]
        }
        else
        {
          cost_page_tution$Tuition[i] <- cost_page_tution$In.State[i]
        }
      }
    }
    cost_page_tution_select <- cost_page_tution %>%
      filter(State == state.abb[match(input$state_for_cost,state.name)]) %>%
      filter(Tuition > input$tuition_slider[[1]], Tuition < input$tuition_slider[[2]]) 
    return(cost_page_tution_select)
  })
  
  output$tuiton_salary <- renderPlotly({
    for (i in 1:nrow(cost_page_tution)) {
      # International student or Private Schools
      if (input$state_for_cost == '' | cost_page_tution$Type[i] == "Private") {
        cost_page_tution$Tuition[i] <- cost_page_tution$Out.State[i]
      } 
      # US citizen
      else {
        if(cost_page_tution$Type[i] == "Public" & 
           cost_page_tution$State[i] != state.abb[match(input$state_for_cost,state.name)]) 
        {
          cost_page_tution$Tuition[i] <- cost_page_tution$Out.State[i]
        }
        else
        {
          cost_page_tution$Tuition[i] <- cost_page_tution$In.State[i]
        }
      }
    }
    cost_page_tution_select <- cost_page_tution %>%
      filter(Tuition > input$tuition_slider[[1]], Tuition < input$tuition_slider[[2]])
    plot_ly(cost_page_tution_select, x = ~Tuition, y = ~Avg.Faculty.Salary,
            text = ~paste("School: ", Institution.Name,'\nTuition:', Tuition, '\nCity:', City, '\nState:', State),
            color = ~Tuition, size = ~Avg.Faculty.Salary
    ) %>% layout(title = "Total Tuition vs. Faculty Salary")
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
      addCircleMarkers(lng = filtered_state$Long,
                 lat = filtered_state$Lat,
                 fillColor = "#a6bddb", fillOpacity = .7, color = "#3182bd", radius = 7, stroke = TRUE,
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
      addCircleMarkers(lng = wa_data$Long,
                 lat = wa_data$Lat,
                 fillColor = "#a6bddb", fillOpacity = .7, color = "#3182bd", radius = 7, stroke = TRUE,
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
  # Diversity Data
  filtered_state <- reactive({
    diversity_data %>%
      select(Institution.Name, City, State, Year, Total.Men, Total.Women) %>%
      filter(Year == input$year_diver) %>%
      filter(State == input$state_diver)
  })
  
  output$diversity_men <- renderUI({
    plotlyOutput("diversity_men_plot")
  })
  
  output$diversity_men_plot <- renderPlotly({
    plot_ly(filtered_state(), x = ~Institution.Name, y = ~Total.Men, 
            type = 'bar', mode = 'markers', name = 'Total Men',
            text = ~paste('Year: ', input$year_diver)) %>%
      add_trace(y = ~Total.Women, name = 'Total Women') %>%
      layout(title = "Total Men and Total Women vs. Schools",
             yaxis = list(title = 'Total Men Cout'), barmode = 'group',
             xaxis = list(title = ''))
  })
  
  output$diversity_table <- renderDataTable({
    if(input$state_diver == '') {
      table_output <- diversity_data %>%
        filter(Year == input$year_diver) %>%
        select(Year, Institution.Name, State, City, Total.Men, Total.Women)
      table_output
    } else if(input$state_diver != '') {
      filtered_state()
    }
  })
  
  # First Generation Data
  filtered_state_gen <- reactive({
    diversity_data %>%
      select(Institution.Name, City, State, Year, Total.First.Gen) %>%
      filter(Year >= input$year_gen[[1]], Year <= input$year_gen[[2]]) %>%
      filter(State == input$state_diver)
  })
  
  output$generation_ui <- renderUI({
    plotlyOutput("first_gen_plot")
  })
  
  output$first_gen_plot <- renderPlotly({
    plot_ly(filtered_state_gen(), x = ~Year, y = ~Total.First.Gen, color = ~Institution.Name,
            text = ~paste("School: ", Institution.Name,'\nYear:', Year,
                          '\nCity:', City, '\nState:', State)) %>%
      layout(yaxis = list(title = 'Total First Generation Count'), 
             title = "Total First Generation in Colleges each Year", 
             barmode = 'group')
    
  })
  
  output$first_table <- renderDataTable({
    if(input$state_diver == '') {
      table_output <- diversity_data %>%
        filter(Year >= input$year_gen[[1]], Year <= input$year_gen[[2]]) %>%
        select(Year, Institution.Name, State, City, Total.First.Gen)
      table_output
    } else if(input$state_diver != '') {
      filtered_state_gen()
    }
  })
}

shinyServer(server)