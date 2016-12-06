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
join_new_data <- left_join(data, new.data, by=c("county", "state_abbreviation"))

final_data_yubo <- join_new_data %>%  na.omit() %>%
  select(state, state_abbreviation, county, party, candidate, votes,
         SEX255214, RHI225214, RHI325214, RHI425214, RHI525214, RHI625214,
         RHI725214, RHI825214, EDU635213, EDU685213, INC110213)
colnames(test2) <- c('state', 'abb', 'county', 'party', 'candidate', 'votes',
                          'female', 'black', 'indian', 'asian', 'hawaiian', 'multi', 'hispanic',
                          'white', 'highschool', 'bachelors', 'income')
#test