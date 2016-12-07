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

test_rep <- final_data %>% filter(party == "Republican") %>% select(-candidate, -votes)
test_dem <- final_data %>% filter(party == "Democrat") %>% select(candidate) %>% unique() %>% filter(candidate != "No Preference")

carson <- ByCounty(final_data, "Ben Carson")
trump <- ByCounty(final_data, "Donald Trump")
kasich <- ByCounty(final_data, "John Kasich")
rubio <- ByCounty(final_data, "Marco Rubio")
cruz <- ByCounty(final_data, "Ted Cruz")
fiorina <- ByCounty(final_data, "Carly Fiorina")
christie <- ByCounty(final_data, "Chris Christie")
bush <- ByCounty(final_data, "Jeb Bush")
huckabee <- ByCounty(final_data, "Mike Huckabee")
paul <- ByCounty(final_data, "Rand Paul")
santorum <- ByCounty(final_data, "Rick Santorum")

rep_join <- left_join(test_rep, trump, by = c("county", "abb"))
rep_join <- left_join(rep_join, carson, by = c("county", "abb"))
rep_join <- left_join(rep_join, kasich, by = c("county", "abb"))
rep_join <- left_join(rep_join, rubio, by = c("county", "abb"))
rep_join <- left_join(rep_join, cruz, by = c("county", "abb"))
rep_join <- left_join(rep_join, fiorina, by = c("county", "abb"))
rep_join <- left_join(rep_join, christie, by = c("county", "abb"))
rep_join <- left_join(rep_join, bush, by = c("county", "abb"))
rep_join <- left_join(rep_join, huckabee, by = c("county", "abb"))
rep_join <- left_join(rep_join, paul, by = c("county", "abb"))
rep_join <- left_join(rep_join, santorum, by = c("county", "abb"))

rep_by_county <- rep_join %>% unique() %>% mutate(row = seq(1:nrow(.)))
rep_by_county[is.na(rep_by_county)] <- 0

rep_winners <- rep_by_county %>% 
  select(-state, -female, -county, -abb, -party, -black, -indian, -asian, -hawaiian, -multi, -hispanic,
         -white, -highschool, -bachelors, -income) %>% 
  mutate(row = seq(1:nrow(.)))

rep_winners$row <- seq(1:nrow(rep_winners))

winner <- as.data.frame(cbind(row.names(rep_winners),apply(rep_winners,1,function(x)
  names(rep_winners)[which(x==max(x))])))
winner$row <- seq(1:nrow(winner))

rep_by_county <- left_join(rep_by_county, winner, by = "row")
names(rep_by_county)[names(rep_by_county) == "V1"] <- "remove"
names(rep_by_county)[names(rep_by_county) == "V2"] <- "winner"
rep_by_county <- rep_by_county %>% 
                  select(-remove)


