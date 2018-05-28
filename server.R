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
  # generate the map
  output$map <- renderLeaflet({
    # coming soon
    
    return(leaflet() %>% 
             addTiles() 
             )
  })
}

shinyServer(server)
