########################
### Install Packages ###
########################

# install.packages("shinythemes")

########################
### Library Packages ###
########################
library(shiny)
library(shinythemes)
library(leaflet)
library(plotly)
library(dplyr)
library(shinycssloaders)

source("process.R")

#-------------------------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("united"),
  includeCSS("finalProject.css"),

  #-------------------------------------------------------------------------
  # Top navigation bar for Home, Admission, Cost, Diversity, Conclusion
  navbarPage(
    "College Score Card Data",

    # Overview tab in navigation bar
    tabPanel(
      "Overview",
      # add fluid rows for banner image and overview
      fluidRow(
        h2("Left blank for HTML"),

        br(),
        hr(),
        column(width = 6, img(src = "students.png")),
        column(width = 6, img(src = "students.png"))
      )
    ),

    #-------------------------------------------------------------------------
    # Admissions tabe on navigation bar
    tabPanel(
      "Admissions",
      # Sidebar for input widgets
      sidebarLayout(
        sidebarPanel(
          sliderInput("year", "Year", 2006, 2015, value = c(2006, 2015), sep = ""),
          helpText(strong("Note")),
          helpText("Choose a ", strong("Year"), " for a range"),

          br(),
          selectInput("state", "State Option", c(Choose = "All", state.name), selectize = FALSE),

          br(),
          uiOutput("college_names")
        ),
        #-------------------------------------------------------------------------
        mainPanel(
          tabsetPanel(
            #-------------------------------------------------------------------------
            tabPanel(
              "Admission Rate",
              dataTableOutput("admission_table")
            ),
            #-------------------------------------------------------------------------
            tabPanel(
              "Admission Rate Plot",
              uiOutput("admission_rate_ui"),
              em(tags$p("Not all schools have 2006-2015 admission rate data available."))
            ),
            #-------------------------------------------------------------------------
            tabPanel(
              "SAT Table",
              dataTableOutput("SAT_table")
            ),
            #-------------------------------------------------------------------------
            tabPanel(
              "SAT Score Plot",
              uiOutput("SAT_ui"),
              em(tags$p("All scores are converted to the latest SAT with the total score of", strong(code("1600")), ".")),
              em(tags$p("Not all schools have 2006-2015 SAT score data available."))
            ),
            #-------------------------------------------------------------------------
            tabPanel(
              "Summary",
              br(),
              h4(strong("Summary:")), 
              br(),
              p("After observing recent year's admission rate and admitted students' average SAT score of many colleges,
                we noticed that most of the schools, especially schools with academic prestige, have an downward trend
                of admission rate and an upward trend of admitted students' average SAT score. That is, most colleges
                are harder and harder to get into."),
              br(),
              p("We also noticed that despite the trends, the admission rate and average SAT score fluctuate within
                a certain range, about ten percent for the admission rate and fifty points for the SAT score.")
            )
          )
        )
      )
    ),

    #-------------------------------------------------------------------------
    # Cost tab on navigation bar

    tabPanel(
      "Cost",
      sidebarLayout(
        sidebarPanel(
          sliderInput("year", "Year", 2006, 2015, value = c(2006, 2015), sep = ""), # Change range values accordingly

          sliderInput("cost", label = "Dollars ($)", min = 0, max = 10, value = "") # Change range values accordingly
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Visualization",
              plotOutput("Tuitionvsfaculty")
            ),
            #
            #     tabPanel(
            #
            #         "Tuition",
            #        plotOutput('Tuitionvsfaculty') # 3rd plot: in-state & out-state vs faculty salary
            #         DT::dataTableOutput("tuition_table") # table of in-state & out-state
            #      ),

            tabPanel(
              "Expenditure",
              plotOutput("expenditurevsfaculty") # 4th plot: expenditure vs faculty salary
              #
            )
          )
        )
      )
    ),

    #-------------------------------------------------------------------------
    # Diversity (First-Gen Student) tabe on navigation bar
    tabPanel(
      "Diversity",
      sidebarLayout(
        sidebarPanel(
          selectInput("state", label = "State", c("Washington")), # Updata choices with all states
          sliderInput("year", "Year", 2006, 2015, value = c(2006, 2015), sep = "") # Add range of years, default 2015
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Percent Men/Women", plotOutput("menwomen") # 5th plot
            ),
            tabPanel(
              "Diversity", dataTableOutput("diversity") # 6th plot
            )
          )
        )
      )
    ),

    #-------------------------------------------------------------------------
    # Map of 2015 Universities
    tabPanel(
      "Map",
      sidebarLayout(
        sidebarPanel(
          textInput("city", label = "City"),
          textInput("state", label = "State"),
          textInput("university", label = "University")
        ),

        mainPanel(
          leaflet("map")
        )
      )
    ),

    #-------------------------------------------------------------------------
    # Conclusion of analysis and html tags as needed
    tabPanel(
      "Conclusion",
      fluidRow()
    ),
    br(),
    hr(),

    p("INFO 201 | Spring 2018 | April Murrieta, Emily Ding, Xiaotong Yang, Woong Jin Jang", align = "center"),
    p("Link to ", a(strong(code("INFO201-Final-Project")), href = "https://github.com/aprilynn/INFO201-Final-Project"), align = "center")
  )
)

shinyUI(ui)
