########################
### Install Packages ###
########################

#install.packages("shinythemes")

########################
### Library Packages ###
########################
library(shiny)
library(shinythemes)
library(leaflet)
source("process.R")

ui <- fluidPage(theme = shinytheme("united"), 
  includeCSS("finalProject.css"),
  
  # Top navigation bar for Home, Admission, Cost, Diversity, Conclusion
  navbarPage("College Score Card Data", 

    # Overview tab in navigation bar
    tabPanel("Overview", 
      #add fluid rows for banner image and overview   
      fluidRow(
        h2("Left blank for HTML"),
        
        br(),
        hr(),
        column(width = 6, img(src = "students.png")),
        column(width = 6, img(src = "students.png"))
      )
    ), 
    
    # Admissions tabe on navigation bar
    tabPanel("Admissions", 
      # Sidebar for input widgets
      sidebarLayout(
        sidebarPanel(
          sliderInput("year", label = "Year", min = 2006, max = 2015, value = "2015", sep = ""), # Add range of years, default 2015
          helpText(strong("Note")),
          helpText("Choose a ", strong("Year"), " for a range"),
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
    tabPanel("Cost", 
      sidebarLayout(
        sidebarPanel(
          sliderInput("cost", label = "Dollars ($)", min = 0, max = 10, value = "") # Change range values accordingly
        ), 
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Tuition",
            plotOutput('Tuitionvsfaculty'), # 3rd plot: in-state & out-state vs faculty salary
            DT::dataTableOutput("tuition_table") # table of in-state & out-state
            ),
            
            tabPanel(
              "Expenditure",
              plotOutput('expenditurevsfaculty')  # 4th plot: expenditure vs faculty salary
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
            tabPanel("Percent Men/Women", plotOutput("menwomen") # 5th plot
                     
                     ),
            tabPanel("Diversity", dataTableOutput("diversity") # 6th plot
                     
                     )
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
    hr(),
    
    p("INFO 201 | Spring 2018 | April Murrieta, Emily Ding, Xiaotong Yang, Woong Jin Jang", align = "center"),
    p("Link to ", a(strong(code("INFO201-Final-Project")), href = "https://github.com/aprilynn/INFO201-Final-Project"), align = "center")
  )
)

shinyUI(ui)