########################
### Install Packages ###
########################

# install.packages("shinythemes")
# install.packages("ggmap")

############
## SET UP ##
############

library(dplyr)
library(ggmap)

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

# set numeric type for several columns
df_2015[, 20:21] <- as.numeric(unlist(df_2015[, 20:21]), na.rm = TRUE)

cities <- as.vector(unique(df_2015$City))

#### Need these codes for record!
#### Adding latitude and longitude to `null` values
# null <- df_2015 %>% filter(Lat == 0, Long == 0)
# 
# loc <- geocode(as.character(null$Institution.Name))
# 
# df_2015[df_2015$Lat == "0" ,c("Lat")] <- loc$lat
# df_2015[df_2015$Long == "0" ,c("Long")] <- loc$lon
# 
# View(df_2015)
# 
# write.csv(df_2015, "data/MERGED2015.csv", row.names = FALSE)

## for server.R
# select state data
getState <- function(states) {
  selected <- df_2015 %>% filter(State.Postcode %in% state.abb[match(states, state.name)])
  selected
}

# Prepar the text for the itnitial map:
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
