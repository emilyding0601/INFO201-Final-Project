########################
### Library Packages ###
########################
library(shiny)
library(shinythemes)
library(leaflet)
library(plotly)
library(dplyr)
library(shinycssloaders)
library(DT)
library(RColorBrewer)
library(ggmap)

source("process.R")

#-------------------------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("united"),
  includeCSS("finalProject.css"),

  #-------------------------------------------------------------------------
  # Top navigation bar for Overview, Admission, Cost, Diversity, Conclusion
  navbarPage(
    "College Score Card Data",
    # Overview tab in navigation bar
    tabPanel(
      "Overview",
      # add fluid rows for banner image and overview
      fluidRow(
        column(width = 6, img(src = "students.png")),
        column(width = 6, img(src = "students.png")),
        br(),
        includeHTML("overview.html")
      )
    ),
    #-------------------------------------------------------------------------
    tabPanel(
      "Admission Plot",
      sidebarLayout(
        sidebarPanel(
        p(strong("This is an", strong(code("overview")), "plot of different colleges with recent admission information.")),
        helpText("Please be patient for the graph to load.")
        ),
        mainPanel(
          plotlyOutput("admission_plot_page")
        )
      )
    ),
    #-------------------------------------------------------------------------
    # Admissions tabe on navigation bar
    tabPanel(
      "School Search",
     fluidRow(
        column(4,
          wellPanel(
            p(strong("If you are interested in a particular school, you can use this page to search it and 
            find out its admission rate and average SAT score in recent years.")),
            p("Select Year Range"),
            sliderInput("year", "Year", 2006, 2015, value = c(2006, 2015), sep = ""),
           br(),
           p("Select State and/or College"),
           helpText("There are ", strong("two"), "selection method, you can only use one at a time."),
           
           helpText("1) You can", strong(code("select or type")), "a state first and then",
                    strong(code("select or type")), "a college in that state."),
           selectInput('state', label = "State Option (Select or Type)",
                       choices =  c("", state.name),
                       multiple = F, selected = F),
           uiOutput("college_names"),
           
           helpText("2) You can", strong(code("type")), "the college name directly."),
           uiOutput("college_names_2")
        ), 
        wellPanel(
          h4(strong("Summary:")),
          br(),
          p("After observing recent years admission rate and admitted students average SAT score of multiple colleges.
            We noticed that most of the schools, especially schools with academic prestige, have an downward trend
            of admission rate and an upward trend of admitted students average SAT score. That is, most colleges
            are harder and harder to get into."),
          br(),
          p("We also noticed that despite the trends, the admission rate and average SAT score fluctuate within
            a certain range, about ten percent for the admission rate and fifty points for the SAT score.")
        )
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
          
          selectInput('state_for_cost', label = "Your state residence (Select or Type)", 
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
    # Diversity (First-Gen Student) tabe on navigation bar
    tabPanel(
      "Diversity",
      sidebarLayout(
        sidebarPanel(
          p(strong("See the percet of gender distribution.")),
          p("Select Year Range"),
          sliderInput("year_diver", "Year", 2006, 2015, value = c(2006, 2015), sep = ""),
          br(),
          p("Select State and/or College"),
          helpText("There are ", strong("two"), "selection method, you can only use one at a time."),
          
          helpText("1) You can", strong(code("select or type")), "a state first and then",
                   strong(code("select or type")), "a college in that state."),
          selectInput('state_diver', label = "State Option (Select or Type)",
                      choices =  c(diversity_data$state),
                      multiple = F, selected = F),
          uiOutput("state_output")
        ), 
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Percent Men/Women", 
              uiOutput("diversity_ui")
              # plotOutput("menwomen") # 5th plot
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
      fluidRow(
        column(3,
               wellPanel(
                 h4(strong("Select the state(s)")),
                 selectInput('state_map', label = "",
                              choices =  c(state.name), 
                              multiple = TRUE, selected = "Washington", selectize = FALSE),
          br(),
          h4(strong("How to use?")),
          helpText("The initial visualization contains all the schools at ", strong(" Washington State "),
                   " in ", strong(code("2015")), 
                   ". Please select states. 
                   You can choose multple states by ", strong(code("'Ctrl' + left click")), 
                   " in",strong(" Windows"), " OR ", strong(code("'Command' + left click")), 
                   " in",strong(" Mac"), "."),
          helpText(strong("The table set contains the information of the school with its website."))
        ),
        wellPanel(
          # add the summary
          h4(strong("Summary")),
          helpText("There are ", strong(num_2015), " school in total.", strong(num_no_SAT), 
                   " schools do not require SAT score for general enrollment. 
                   The average age of enrollment entry is ", strong(round(summary$avg.age[1], 0)),
                   " years old. The average in-state tuition is ", strong("$", round(summary$avg.in.tuition[1], 2)),
                   ". The average out-state tuition is ", strong("$", round(summary$avg.out.tuition[1], 2)),
                   ". The most expensive tuition is ", strong("$", max_col(df_2015$`In-State.Tuition`)$`In-State.Tuition`[1]), 
                   ", which is ", strong(max_col(df_2015$`In-State.Tuition`)$Institution.Name[1]),
                   ". The highest average SAT score is ", strong(max_col(df_2015$Avg.SAT)$Avg.SAT[1]), 
                   ", which is ", strong(max_col(df_2015$Avg.SAT)$Institution.Name[1]), 
                   " with ", strong("$", max_col(df_2015$Avg.SAT)$`In-State.Tuition`[1]),
                   " tuition for both in & out-state. The average age for enrollment is ",
                   strong(round(max_col(df_2015$Avg.SAT)$Avg.Age[1])), " years old.")

        ),
        wellPanel(
          # add the legend
          h4(strong("Legend:")),
          helpText(strong(code("UnitID:")), "Unit ID for institution"),
          helpText(strong(code("Avg.SAT:")), "The average score of SAT, values of \"0\" mean 
                   there is no SAT score provided."),
          helpText(strong(code("Open.Admissions.Policy:")), 
                   "Open admissions policy indicator,", strong("1"), " - Yes.",
                   strong("2"), " - No.")
        )
      ),
      
        column(9,
          br(),
          h4("The map visualization contains all the schools in ", strong(code("2015")), "."),
          h5("Please wait for plot to load. Use mouse to hover over the ", 
             strong("Markers"), " to see the detail. 
             Select states if you like."),
          br(),
          leafletOutput('map'),
          br(),
          DT::dataTableOutput('maptable')
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
