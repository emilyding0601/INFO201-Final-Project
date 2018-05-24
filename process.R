############
## SET UP ##
############

<<<<<<< HEAD
library("httr")
library("jsonlite")
=======
>>>>>>> dataset
library("tidyr")
library("ggplot2")
library("dplyr")
library("maps")
library("plotly")
library("readr")
library("stringr")

<<<<<<< HEAD
source("api_key.R")
base_uri <- "https://api.data.gov/ed/collegescorecard/v1/schools"
parameter <- list("api_key" = apiKey)
response <- GET(paste0(base_uri, ""), query = parameter)
body <- fromJSON(content(response, "text"))
df <- flatten(body$results)
View(df)
=======
df <- read.csv("data/MERGED2006_V1.csv", stringsAsFactors = FALSE)
View(df)
>>>>>>> dataset
