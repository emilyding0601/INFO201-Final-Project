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

df <- read.csv("data/MERGED2006_V1.csv", stringsAsFactors = FALSE)
View(df)