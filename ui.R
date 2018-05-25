########################
### Install Packages ###
########################

#install.packages("shinythemes")

########################
### Library Packages ###
########################
library(shiny)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("sandstone"), 
  
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
          sliderInput("Admissions", label = "admissions", min = 0, max = 10, value = ""),  # Change Admission min/max values
          sliderInput("SAT", label = "SAT", min = 0, max = 10, value = "") # Change SAT min/max values
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
          # Add control Widgets
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
          sliderInput("Year", label = "year", min = 2006, max = 2015, value = "2015"), # Add range of years, default 2015
          textInput("State", label = "state", value = "Washington") # default value to Washington
        ), 
        mainPanel(
          # Table or Plot
          plotOutput("diversity"), 
          dataTableOutput("diversity")
        )
      )
    ), 
    
    # Conclusion of analysis and html tags as needed
    tabPanel("Conclusion", 
      fluidRow()
    )
  )
)

shinyUI(ui)