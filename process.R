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

df <- read.csv("data/MERGED2006-2015.csv", stringsAsFactors = FALSE)
View(df)

