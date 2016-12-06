library(dplyr)

data <- read.csv('./data/primary_results.csv', stringsAsFactors = FALSE)
county.facts <- read.csv('./data/county_facts.csv', stringsAsFactors = FALSE)

SortData <- function(results, facts) {
  new_county_names <- facts %>% 
    filter(state_abbreviation) %>% 
    collect() %>% 
    .$area_name %>% 
    gsub(" County", "", .)
}

new.data <- SortData(data, county.facts)