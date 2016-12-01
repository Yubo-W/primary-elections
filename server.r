library(dplyr)

primary <- read.csv('./data/primary_results.csv', stringsAsFactors = FALSE)
county <- read.csv('./data/county_facts.csv', stringsAsFactors = FALSE)

joined_data <- left_join(primary, county, by="fips")