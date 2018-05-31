########################
### Install Packages ###
########################

# install.packages("shinythemes")

############
## SET UP ##
############

library(dplyr)

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
cost_page_tution <- cost_page %>% filter(Year == "2015") %>% select((2:6), 9)
colnames(cost_page_tution) <- c("Institution.Name", "City", "State", "In_State_Tuition", "Out_State_Tuition", "Avg.Faculty.Salary")
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

sapply(df_2015, class)

# View(df_2015)
# set numeric type for several columns
df_2015[, 20:21] <- as.numeric(unlist(df_2015[, 20:21]), na.rm = TRUE)

cities <- as.vector(unique(df_2015$City))

# Filter null values
df_2015 <- df_2015 %>% filter(Long != "NA", Lat != "NA")

## for server.R
# select state data
getState <- function(states) {
  selected <- df_2015 %>% filter(State.Postcode %in% state.abb[match(states, state.name)])
  selected
}

# Prepare the text for the initial map:
text <- paste("<h4/>", df_2015$Institution.Name, "<br/>", "<br/>",
              "Enrollment Number: ", df_2015$Enrollment, "<br/>", 
              "In-state Tuition: ", " $", df_2015$`In-State.Tuition`, "<br/>",
              "Out-state Tuition: ", " $", df_2015$`Out-State.Tuition`, "<br/>",
              "Admission Rate: ", round(df_2015$Admission.Rate, 2) * 100, "%", "<br/>",
              "Average Age: ", round(df_2015$Avg.Age, 0), "<br/>",
              sep = "") %>% lapply(htmltools::HTML)

# extract the data number for `ui map`
num_2015 <- nrow(df_2015)
num_no_SAT <- df_2015 %>% filter(Avg.SAT == '0') %>% nrow()

summary <- df_2015 %>% 
  summarize(avg.in.tuition = mean(`In-State.Tuition`, na.rm = TRUE),
            avg.out.tuition = mean(`Out-State.Tuition`, na.rm = TRUE),
            avg.age = mean(Avg.Age,  na.rm = TRUE))

# function get the top column info
max_col <- function(var) {
  col <- df_2015 %>% arrange(-var) %>% head(1)
  col
}

# Select the data for diversity
diversity_data <- df_2006_2015 %>% 
  select(Year, Institution.Name, City, State.Postcode, Enrollment, 
         `Percent.1st-generation`, Total.Enrolled.Men, Total.Enrolled.Women) %>% 
  mutate(state = state.name[match(State.Postcode, state.abb)],
         total_men = Enrollment * Total.Enrolled.Men,
         total_women = Enrollment * Total.Enrolled.Women,
         total_first_gen = Enrollment * `Percent.1st-generation`) %>% 
  select(-State.Postcode, -`Percent.1st-generation`, -Total.Enrolled.Men, -Total.Enrolled.Women)

