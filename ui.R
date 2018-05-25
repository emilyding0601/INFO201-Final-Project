########################
### Install Packages ###
########################

#install.packages("shinythemes")

########################
### Library Packages ###
########################
library(shiny)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("united"), 
  
  # Top navigation bar for Home, Admission, Cost, Diversity, Conclusion
  navbarPage("College Score Card Data", 
             
    # Overview tab in navigation bar
    tabPanel("Overview", 
      #add fluid rows for banner image and overview   
      fluidRow()
    ), 
    
    # Admissions tabe on navigation bar
    tabPanel("Admissions", 
      # Sidebar for input widgets
      sidebarLayout(
        sidebarPanel(
          sliderInput("admissions", label = "Admissions", min = 0, max = 10, value = ""),  # Change Admission min/max values
          sliderInput("sat", label = "SAT", min = 0, max = 10, value = "") # Change SAT min/max values
        ), 
        mainPanel(
          tabsetPanel(
            tabPanel("Admission", plotOutput("admission")), 
            tabPanel("SAT", plotOutput("sat"))
            
          )
        )
      )
    ), 
    
    # Cost tab on navigation bar
    tabPanel("Cost", 
      sidebarLayout(
        sidebarPanel(
          sliderInput("cost", label = "Cost", min = 0, max = 10, value = "") # Change range values accordingly
        ), 
        mainPanel(
          plotOutput("Tuitionvsfaculty")
        )
      )
    ), 
    
    # Diversity (First-Gen Student) tabe on navigation bar  
    tabPanel("Diversity", 
      sidebarLayout(
        sidebarPanel(
          selectInput("state", label = "State", c("Washington")), # Updata choices with all states
          sliderInput("year", label = "Year", min = 2006, max = 2015, value = "2015") # Add range of years, default 2015
        ), 
        mainPanel(
          tabsetPanel(
            tabPanel("Percent Men/Women", plotOutput("genderdiff")),
            tabPanel("Diversity", dataTableOutput("diversity"))
          )
        )
      )
    ), 
    
    # Map of 2015 Universities
    tabPanel("Map", 
      textInput("city", label = "City"), 
      textInput("state", label = "State"), 
      textInput("university", label = "University")
    ),
    
    # Conclusion of analysis and html tags as needed
    tabPanel("Conclusion", 
      fluidRow()
    )
  )
)

shinyUI(ui)