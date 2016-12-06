library(dplyr)

data <- read.csv('./data/primary_results.csv', stringsAsFactors = FALSE)
county.facts <- read.csv('./data/county_facts.csv', stringsAsFactors = FALSE)

SortData <- function(results, facts) {
  new.county <- facts %>% 
                filter(state_abbreviation != "")
  
  new_county_names <- new.county %>% 
    collect() %>% 
    .$area_name %>% 
    gsub(" County", "", .)
  
  new.county <- new.county %>% 
                select(-area_name)
  
  new.county$county <- new_county_names
  return(new.county)
}

new.data <- SortData(data, county.facts)
join_new_data <- left_join(data, new.data, by="county")
