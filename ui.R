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
          h5(strong("Note")),
          helpText("Choose a ", strong("Year"), " range"),
          sliderInput("year", "Year", 2006, 2015, value = c(2006, 2015), sep = ""),
          
          br(),
          h5(strong("Note")),
          p("You have", strong(code("two")), "choices of college selection,"),
          p("But you can only use", strong(code("one")), "of the selection methods:"),
          helpText("1) You can", strong(code("select or type")), "a state first and then", 
                   strong(code("select or type")), "a college in that state."),
          selectInput('state', label = "State Option (Select or Type)", 
                      choices =  c("", state.name), 
                      multiple = F, selected = F),
          uiOutput("college_names"),
          
          helpText("2) You can", strong(code("type")), "the college name directly."),
          uiOutput("college_names_2")
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
          h5(strong("Note")),
          helpText("Enter your", strong("state residence"), "."),
          helpText("If you", strong("don't belong to any state,"), 
                   "(such as International Students), leave it", strong("blank")),
          
          selectInput('state_for_cost', label = "State Option (Select or Type)", 
                      choices =  c("", state.name), 
                      multiple = F, selected = F),
          br(),
          helpText("Choose a ", strong("tuition"), " range"),

          sliderInput("tuition_slider", "Tuition ($)", 0, 70000, value = c(0, 70000), 
                      step = 500, pre = "$", sep = ""),
          #helpText("Choose a ", strong("school type")),
          #checkboxGroupInput("school_type", "School Type:", c("private", "public"), selected = c("private", "public")),
          br()
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Tuition Table",
              dataTableOutput("filter_cost_table")
            ),
            tabPanel(
              "Tuition Plot",
              plotOutput("expenditurevsfaculty") 
            )
          )
        )
      )
    ),

    #-------------------------------------------------------------------------
    tabPanel(
      "Prediction",
      sidebarLayout(
        sidebarPanel(
          h4(strong("Clarification:")),
          p(
            "This prediction is solely based on your", strong(code("SAT score")), ", and your desired ",
            strong(code("cost")), strong(code("school type")), "and", strong(code("location")), 
            "so it can be highly inaccurate. But we just want to give you an idea that what schools may suit you."
          )
        ),
        mainPanel()
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
          selectInput('state_map', label = "Select Your State(s)", 
                      choices =  c(state.name), 
                      multiple = TRUE, selected = "Choose"),
# 
#           selectInput('city_map', label = "Select Your Cities",
#                       choices = c(unique(df_2015$City)),
#                       multiple = TRUE, selected = "Choose"),
          
          helpText(strong("Note")),
          helpText("The initial map contains all the schools in ", strong(code("2015")), 
                   "Please select states. You can choose multple states.")
        ),

        mainPanel(
          h4("Here's a map visualization only for ", strong(code("2015")), " . 
             Please be patient for plotting. Use mouse to hover over the bubble to see the data.
             Select states if you like."),
          br(),
          leafletOutput('map')
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
