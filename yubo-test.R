library(dplyr)

data <- read.csv('./data/primary_results.csv', stringsAsFactors = FALSE)
county.facts <- read.csv('./data/county_facts.csv', stringsAsFactors = FALSE)

SortData <- function(facts) {
  new.county <- facts %>% 
                filter(state_abbreviation != "")
  
  new_county_names <- new.county %>% 
    collect() %>% 
    .$area_name %>% 
    gsub(" County", "", .) %>% 
    tolower()
  
  new.county <- new.county %>% 
                select(-area_name)
  
  new.county$county <- new_county_names
  return(new.county)
}

new.data <- SortData(county.facts)
data$county <- tolower(data$county)
join_new_data <- left_join(data, new.data, by = c("county", "state_abbreviation"))

final_data_yubo <- join_new_data %>%  na.omit() %>%
  select(state, state_abbreviation, county, party, candidate, votes,
         SEX255214, RHI225214, RHI325214, RHI425214, RHI525214, RHI625214,
         RHI725214, RHI825214, EDU635213, EDU685213, INC110213)
colnames(final_data_yubo) <- c('state', 'abb', 'county', 'party', 'candidate', 'votes',
                               'female', 'black', 'indian', 'asian', 'hawaiian', 'multi', 'hispanic',
                               'white', 'highschool', 'bachelors', 'income')

#############################################################
# Manually adding in Louisiana and New Hampshire

temp.primary <- data %>%
                filter(state_abbreviation == 'LA' | state_abbreviation == 'NH')

temp.county <- county.facts %>% 
                filter(state_abbreviation == 'LA' | state_abbreviation == 'NH')

new.temp.county <- SortData(temp.county)
temp.primary$county <- tolower(temp.primary$county)
temp_join_LA <- left_join(temp.primary, new.temp.county, by=c("fips")) %>% filter(state_abbreviation.x == 'LA')
temp_join_LA <- temp_join_LA %>% select(-county.y, -state_abbreviation.y, -fips)
names(temp_join_LA)[names(temp_join_LA) == "state_abbreviation.x"] <- "state_abbreviation"
names(temp_join_LA)[names(temp_join_LA) == "county.x"] <- "county"

temp_join_NH <- left_join(temp.primary, new.temp.county, by=c("county", "state_abbreviation")) %>% filter(state_abbreviation == 'NH')
temp_join_NH <- temp_join_NH %>% select(-fips.x, -fips.y)

joined_data <- rbind(temp_join_LA, temp_join_NH) %>% 
                select(state, state_abbreviation, county, party, candidate, votes,
                       SEX255214, RHI225214, RHI325214, RHI425214, RHI525214, RHI625214,
                       RHI725214, RHI825214, EDU635213, EDU685213, INC110213)
colnames(joined_data) <- c('state', 'abb', 'county', 'party', 'candidate', 'votes',
                               'female', 'black', 'indian', 'asian', 'hawaiian', 'multi', 'hispanic',
                               'white', 'highschool', 'bachelors', 'income')
#############################################################
