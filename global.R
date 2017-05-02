library(reshape)
library(networkD3)
library(dplyr)
library(knitr)
library(tidyr)

# Clean data
raw_data <- read.csv("432303311_T_ONTIME.csv")
str(raw_data)
origin_dest <- raw_data[c('ORIGIN_CITY_NAME', 'DEST_CITY_NAME')]

origin_dest$ORIGIN_CITY_NAME <- as.character(origin_dest$ORIGIN_CITY_NAME)
origin_dest$DEST_CITY_NAME <- as.character(origin_dest$DEST_CITY_NAME)

# We are only interested in connection among cities, not flight direction
origin_dest$origin_city <- pmin(origin_dest$ORIGIN_CITY_NAME, origin_dest$DEST_CITY_NAME)
origin_dest$dest_city <- pmax(origin_dest$ORIGIN_CITY_NAME, origin_dest$DEST_CITY_NAME)
origin_dest <- origin_dest[, c("origin_city", "dest_city")]

# Count the number of flights among cities
origin_dest_agg <- origin_dest %>% group_by(origin_city, dest_city) %>% mutate(count = n())
origin_dest_agg <- unique(origin_dest_agg[, 1:3])

# Separate cities and states
origin_dest_agg <- separate(origin_dest_agg, origin_city, into = c("origin_city", "origin_state"), sep = ", ")
origin_dest_agg <- separate(origin_dest_agg, dest_city, into = c("dest_city", "dest_state"), sep = ", ")

raw_origin_dest <- subset(raw_data, ,select=c("ORIGIN_CITY_NAME", "DEST_CITY_NAME"))
raw_origin_dest$ORIGIN_CITY_NAME <- as.character(raw_origin_dest$ORIGIN_CITY_NAME)
origin_separate <- separate(raw_origin_dest, ORIGIN_CITY_NAME, into = c("origin_city", "origin_state"), sep = ", ")
origin_separate <- origin_separate[origin_separate$origin_state %in% c('TX', 'NY', 'CA'), ]
origin_agg <- data.frame(table(origin_dest_agg$origin_city))