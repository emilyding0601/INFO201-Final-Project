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
  
  # observe({
  #   
  #   filtered_city <- getCity(input$city_map)
  #   
  #   text <- paste("Name: ", "<strong/>", filtered_city$Institution.Name, "<strong/>", "<br/>",
  #                 "Enrollment Number: ", "<strong/>", filtered_city$Enrollment, "<strong/>", "<br/>", 
  #                 "In-state Tuition: ","<strong/>", filtered_city$`In-State.Tuition`, "<strong/>", "<br/>",
  #                 "Out-state Tuition: ","<strong/>", filtered_city$`Out-State.Tuition`, "<strong/>", "<br/>",
  #                 "Admission Rate: ","<strong/>", filtered_city$Admission.Rate, "<strong/>", "<br/>",
  #                 "Average Age: ","<strong/>", round(filtered_city$Avg.Age, 0), "<strong/>", "<br/>",
  #                 sep = "") %>% lapply(htmltools::HTML)
  #   
  #   leafletProxy('map') %>% clearMarkers() %>% 
  #     addCircleMarkers(lng = filtered_city$Long,
  #                      lat = filtered_city$Lat,
  #                      label = text)
  #   
  #   
  # })
  # 
  # observe({
  #   
  #   filtered_state_city <- getState(input$state_map) %>% filter(City == input$city_map)
  #   
  #   text <- paste("Name: ", "<strong/>", filtered_state_city$Institution.Name, "<strong/>", "<br/>",
  #                 "Enrollment Number: ", "<strong/>", filtered_state_city$Enrollment, "<strong/>", "<br/>", 
  #                 "In-state Tuition: ","<strong/>", filtered_state_city$`In-State.Tuition`, "<strong/>", "<br/>",
  #                 "Out-state Tuition: ","<strong/>", filtered_state_city$`Out-State.Tuition`, "<strong/>", "<br/>",
  #                 "Admission Rate: ","<strong/>", filtered_state_city$Admission.Rate, "<strong/>", "<br/>",
  #                 "Average Age: ","<strong/>", round(filtered_state_city$Avg.Age, 0), "<strong/>", "<br/>",
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
    
    text <- paste("<h4/>", filtered_state$Institution.Name, "<br/>",
                  "Enrollment Number: ", filtered_state$Enrollment, "<br/>", 
                  "In-state Tuition: ", filtered_state$`In-State.Tuition`, "<br/>",
                  "Out-state Tuition: ", filtered_state$`Out-State.Tuition`, "<br/>",
                  "Admission Rate: ", filtered_state$Admission.Rate, "<br/>",
                  "Average Age: ", round(filtered_state$Avg.Age, 0), "<br/>",
                  sep = "") %>% lapply(htmltools::HTML)
    
    leafletProxy('map') %>% clearMarkers() %>% 
      addCircleMarkers(lng = filtered_state$Long,
                       lat = filtered_state$Lat,
                       label = text,
                       labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                   textsize = "17px", direction = "auto"))
    
  })
  
  
  output$map <- renderLeaflet({
    # select state and city data set 
    leaflet() %>% 
      addTiles() %>% 
      setView(lng = -122.3321, lat = 47.6062, zoom = 7) %>% 
      addCircleMarkers(lng = df_2015$Long,
                       lat = df_2015$Lat,
                       label = text,
                       labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                  textsize = "17px", direction = "auto"))

  })
}

shinyServer(server)
