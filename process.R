############
## SET UP ##
############

library("tidyr")
library("ggplot2")
library("dplyr")
library("plotly")
library("RColorBrewer")

df_2006_2015 <- read.csv("data/MERGED2006-2015.csv", stringsAsFactors = FALSE)

# View(df_2006_2015)
sapply(df_2006_2015, class)

# set a var to change the column name
col <- c("Year", "UnitID", "OPEID", "Institution.Name", "City",
         "State.Postcode", "Zipcode", "Admission.Rate", "Admission.Rate.For.All",
         "Avg.SAT", "Enrollment", "In-State.Tuition", "Out-State.Tuition",
         "Net.Tuition.Revenue", "Instructional.Expenditures", "Avg.Faculty.Salary",
         "Percent.1st-generation", "Avg.Age", "Total.Enrolled.Men", "Total.Enrolled.Women",
         "Open.Admissions.Policy")
colnames(df_2006_2015) <- col

# transfer the mode for some columns
df_2006_2015[, 14:18] <- as.numeric(unlist(df_2006_2015[, 14:18]), na.rm = TRUE)

# a function to calculate the total number of school in each year
num_schools <- function(year) {
  df_2006_2015 %>% filter(Year == year) %>% nrow()
}

# set a summary table of all school number
num_school_table <- data.frame(Year = c(2006:2015), 
                               Total.Number = c(num_schools(2006),
                                                num_schools(2007),
                                                num_schools(2008),
                                                num_schools(2009),
                                                num_schools(2010),
                                                num_schools(2011),
                                                num_schools(2012),
                                                num_schools(2013),
                                                num_schools(2014),
                                                num_schools(2015)), stringsAsFactors = FALSE)

# admission table for the `admission`

admission <- df_2006_2015 %>% select(1, (4:6), (8:10))
summary_adm <- admission %>% select("Admission.Rate", "Admission.Rate.For.All") %>% summary()

#----------------------------------------------------
# This is for the Cost page
cost_page <- df_2006_2015 %>% select(1, (4:6), (12:16))
cost_page_tution <- cost_page %>% filter(Year == "2015") %>% select(2:6)
colnames(cost_page_tution) <- c("Institution.Name", "City", "State", "In_State_Tuition", "Out_State_Tuition")
cost_page_tution$In_State_Tuition <- as.numeric(as.character(cost_page_tution$In_State_Tuition))
cost_page_tution$Out_State_Tuition <- as.numeric(as.character(cost_page_tution$Out_State_Tuition))

for (i in 1:nrow(cost_page_tution)) {
  if (cost_page_tution$In_State_Tuition[i] == cost_page_tution$Out_State_Tuition[i]) {
    cost_page_tution$school_type[i] <- "private"
  } else {
    cost_page_tution$school_type[i] <- "public"
  }
}

#----------------------------------------------------
# average age by year in each state
avg_age_year <- df_2006_2015 %>% group_by(Year, State.Postcode) %>% 
  summarize(avg.age_year = mean(Avg.Age,  na.rm = TRUE))

#### For map
# extract the data from the raw data set
df_2015 <- read.csv("data/MERGED2015.csv", stringsAsFactors = FALSE)

# set a var to change the column name
col_2015 <- c("Year", "UnitID", "OPEID", "Institution.Name", "City",
              "State.Postcode", "Zipcode", "Institution.URL", "Lat", "Long", 
              "Admission.Rate", "Admission.Rate.For.All",
              "Avg.SAT", "Enrollment", "In-State.Tuition", "Out-State.Tuition",
              "Net.Tuition.Revenue", "Instructional.Expenditures", "Avg.Faculty.Salary",
              "Percent.1st-generation", "Avg.Age", "Total.Enrolled.Men", "Total.Enrolled.Women",
              "Open.Admissions.Policy")

colnames(df_2015) <- col_2015

# sapply(df_2015, class)

# mutate 


# set numeric type for several columns
df_2015[, 20:21] <- as.numeric(unlist(df_2015[, 20:21]), na.rm = TRUE)

cities <- as.vector(unique(df_2015$City))

## for server.R
# select state data
getState <- function(states) {
  selected <- df_2015 %>% filter(State.Postcode %in% state.abb[match(states, state.name)])
  selected
}

# Create a color palette with handmade bins.
# bins <- seq(min(df_2015$Enrollment), max(df_2015$Enrollment), by = 2000)
# palette <- colorBin(palette = "Spectral",
#                     domain = df_2015$Enrollment,
#                     na.color = "transparent", bins = bins)

# Prepar the text for the itnitial map:
text <- paste(df_2015$Institution.Name, "<br/>",
              "Enrollment Number: ", df_2015$Enrollment, "<br/>", 
              "In-state Tuition: ", df_2015$`In-State.Tuition`, "<br/>",
              "Out-state Tuition: ", df_2015$`Out-State.Tuition`, "<br/>",
              "Admission Rate: ", df_2015$Admission.Rate, "<br/>",
              "Average Age: ", round(df_2015$Avg.Age, 0), "<br/>",
              sep = "") %>% lapply(htmltools::HTML)