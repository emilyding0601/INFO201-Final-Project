########################
### Install Packages ###
########################

#install.packages("shinythemes")

########################
### Library Packages ###
########################
library(shiny)
library(shinythemes)
source("process.R")

ui <- fluidPage(theme = shinytheme("united"), 
  includeCSS("finalProject.css"),
  
  # Top navigation bar for Home, Admission, Cost, Diversity, Conclusion
  navbarPage("College Score Card Data", 

    # Overview tab in navigation bar
    tabPanel("Overview", 
      #add fluid rows for banner image and overview   
      fluidRow(
        column(width = 12, img(src = "gradhats.jpg"))
      )
    ), 
    
    # Admissions tabe on navigation bar
    tabPanel("Admissions", 
      # Sidebar for input widgets
      sidebarLayout(
        sidebarPanel(
          sliderInput("year", label = "Year", min = 2006, max = 2015, value = "2015", sep = ""), # Add range of years, default 2015
          p(strong("Note")),
          p("Choose a ", strong("Year"), " for a range"),
          selectInput("state", label = "State", c("Washington")), # Updata choices with all states
          
          selectInput("school", label = "School", c("University of Washington")) # Updata choices with all states
        ), 
        mainPanel(
          tabsetPanel(
            tabPanel("Admission", plotOutput("admission"), # 1st plot
                     tableOutput('amdission_table') # summary of admission table
                     ), 
            tabPanel("SAT", plotOutput("sat") # 2nd plot
                     
                     )
          )
        )
      )
    ), 
    
    # Cost tab on navigation bar
    tabPanel("Tuition", 
      sidebarLayout(
        sidebarPanel(
          sliderInput("tuition", label = "Tuition", min = 0, max = 10, value = "") # Change range values accordingly
        ), 
        mainPanel(
          tabsetPanel(
            tabPanel(
            plotOutput("Tuitionvsfaculty")
            ),
            
            tabPanel(
              DT::dataTableOutput("tuition_table")
            )
          )
        )
      )
    ), 
    
    # Diversity (First-Gen Student) tabe on navigation bar  
    tabPanel("Diversity", 
      sidebarLayout(
        sidebarPanel(
          selectInput("state", label = "State", c("Washington")), # Updata choices with all states
          sliderInput("year", label = "Year", min = 2006, max = 2015, value = "2015", sep = "") # Add range of years, default 2015
        ), 
        mainPanel(
          tabsetPanel(
            tabPanel("Percent Men/Women", plotOutput("menwomen")),
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
    ),
    
    p("INFO 201 | Spring 2018 | April Murrieta, Emily Ding, Xiaotong Yang, Woong Jin Jang", align = "center"),
    p("Link to ", strong(code("INFO201-Final-Project")), a(" GitHub ", href = "https://github.com/aprilynn/INFO201-Final-Project"), align = "center")
  )
)

shinyUI(ui)