############
## SET UP ##
############

library("tidyr")
library("ggplot2")
library("dplyr")
library("maps")
library("plotly")
library("readr")
library("stringr")

df_2006_2015 <- read.csv("data/MERGED2006-2015.csv", stringsAsFactors = FALSE)
View(df_2006_2015)

# set a var to change the column name
col <- c("Year", "UnitID", "OPEID", "Institution.Name", "City",
                  "State.Postcode", "Zipcode", "Admission.Rate", "Admission.Rate.For.All.Campuses",
                  "Avg.SAT", "Enrollment", "In-State.Tuition", "Out-State.Tuition",
                  "Net.Tuition.Revenue", "Instructional.Expenditures", "Avg.Faculty.Salary",
                  "Percent.1st-generation", "Avg.Age", "Total.Enrolled.Men", "Total.Enrolled.Women",
                  "Open.Admissions.Policy")
colnames(df_2006_2015) <- col

# a function to calculate the total number of school in each year
num_schools <- function(year) {
  df_2006_2015 %>% filter(Year == year) %>% nrow()
}

# admission table for the `admission`
names(df_2006_2015)
# admission <- df_2006_2015 %>% select(1, (4:6), (8:10)) %>% summarize()

# 
