############
## SET UP ##
############

library("httr")
library("jsonlite")
library("tidyr")
library("ggplot2")
library("dplyr")
library("maps")
library("plotly")
library("readr")
library("stringr")

source("api_key.R")
base_uri <- "https://api.data.gov/ed/collegescorecard/v1/schools"
parameter <- list("api_key" = apiKey)
response <- GET(paste0(base_uri, ""), query = parameter)
body <- fromJSON(content(response, "text"))
df <- flatten(body$results)
View(df)
