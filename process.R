############
## SET UP ##
############

library("tidyr")
library("ggplot2")
library("dplyr")
library("plotly")

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
df_2006_2015[, 14:18] <- as.numeric(unlist(df_2006_2015[, 14:18]))

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
names(df_2006_2015)
admission <- df_2006_2015 %>% select(1, (4:6), (8:10))
summary_adm <- admission %>% select("Admission.Rate", "Admission.Rate.For.All") %>% summary()


# average age by year in each state
avg_age_year <- df_2006_2015 %>% group_by(Year, State.Postcode) %>% 
  summarize(avg.age_year = mean(Avg.Age,  na.rm = TRUE))

## For map
# extract the data from the raw data set
df_2015 <- read.csv("data/MERGED2015.csv", stringsAsFactors = FALSE)