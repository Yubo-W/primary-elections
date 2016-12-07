library(dplyr)
library(plotly)

# setwd('/Users/iguest/Documents/final-project-I-mWithHer')

primary <- read.csv('./data/primary_results.csv', stringsAsFactors = FALSE)
county <- read.csv('./data/county_facts.csv', stringsAsFactors = FALSE)
#county.key <- read.csv('./data/county_facts_key.csv', stringsAsFactors = FALSE)
#View(county.key)
source("./scripts/functions.R")

new.county <- SortData(primary, county)
primary$county <- tolower(primary$county)
join_new_data <- left_join(primary, new.county, by=c("county", "state_abbreviation"))

final_data <- join_new_data %>%  na.omit() %>%
  select(state, state_abbreviation, county, party, candidate, votes,
         SEX255214, RHI225214, RHI325214, RHI425214, RHI525214, RHI625214,
         RHI725214, RHI825214, EDU635213, EDU685213, INC110213)
colnames(final_data) <- c('state', 'abb', 'county', 'party', 'candidate', 'votes',
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

finalized_data <- rbind(final_data, joined_data)
## We won't have Alaska's data because Alaska does not participate in the primary elections.
## Missing states (not including alaska):
#     - North Dakota: votes by district
#     Massachussetts: manual sorting
#     Connecticut: manual sorting
#     - Kansas: votes by district
#     Maine: manual sorting
#     - Minnesota: missing data
#     New Hampshire: missing fips, manual sorting
#     Rhode Island: manual sorting
#     Vermont: manual sorting
#     - Alaska: They don't conduct presidential primarys

#combining data by state
bernie_by_state <- ByState(final_data, "Bernie Sanders")
hillary_by_state <- ByState(final_data, "Hillary Clinton")
trump_by_state <- ByState(final_data, 'Donald Trump')
kasich_by_state <- ByState(final_data, "John Kasich")
rubio_by_state <- ByState(final_data, "Marco Rubio")
cruz_by_state <- ByState(final_data, "Ted Cruz")
carson_by_state <- ByState(final_data, "Ben Carson")

#democratic data by state
dem_by_state <- left_join(bernie_by_state, hillary_by_state, by=c("state","abb","county")) %>%
  mutate(winner= ifelse(Bernie_Sanders > Hillary_Clinton, "Bernie", "Hillary"),
         z = ifelse(winner == "Bernie", 1, 0))
View(dem_by_state)

#republican data by state
rep_by_state <- left_join(trump_by_state, kasich_by_state, by=c("state","abb","county")) %>% 
  left_join(., rubio_by_state, by=c("state","abb","county")) %>% 
  left_join(., cruz_by_state, by=c("state","abb","county")) %>% 
  left_join(., carson_by_state, by=c("state","abb","county")) 
rep_by_state[is.na(rep_by_state)] <- 0
rep_by_state <- rep_by_state %>% 
  mutate(if(Donald_Trump > John_Kasich && 
            Donald_Trump > Marco_Rubio &&
            Donald_Trump > Ted_Cruz &&
            Donald_Trump > Ben_Carson) {
    winner = "Trump"
  } else if(John_Kasich > Donald_Trump && 
            John_Kasich > Marco_Rubio &&
            John_Kasich > Ted_Cruz &&
            John_Kasich > Ben_Carson) {
    winner = "Kasich"
  } else if(Marco_Rubio > Donald_Trump && 
            Marco_Rubio > John_Kasich &&
            Marco_Rubio > Ted_Cruz &&
            Marco_Rubio > Ben_Carson) {
    winner = "Rubio"
  } else if(Ted_Cruz > Donald_Trump &&
            Ted_Cruz > John_Kasich &&
            Ted_Cruz > Marco_Rubio &&
            Ted_Cruz > Ben_Carson) {
    winner = "Cruz"
  } else {
    winner = "Carson"
  },
  if(winner == "Trump") {
    z = 0
  } else if(winner == "Kasich") {
    z = 1
  } else if(winner == "Rubio") {
    z = 2
  } else if(winner == "Cruz") {
    z = 3
  } else {
    z = 4
  })
colnames(rep_by_state) <- c('state', 'Donald_Trump', 'abb', 'county',
                          'John_Kasich', 'Marco_Rubio', 'Ted_Cruz', 'Ben_Carson',
                          'winner', 'z')
View(rep_by_state)



#stats
nrow(dem_by_state)
# number of states bernie/hillary won
nrow(dem_by_state %>% filter(winner=="Bernie")) #21 states
nrow(dem_by_state %>% filter(winner=="Hillary")) #28 states
# number of overall voters
sum(dem_by_state$bernie_votes) #11959102 votes
sum(dem_by_state$hillary_votes) #15692452 votes


#democratic bar chart
plot_ly(dem_by_state, x = ~abb, y = ~Bernie_Sanders, type = 'bar', name = 'Bernie Sanders', 
        marker = list(color = "#orange", line = list(color = ifelse(dem_by_state$winner == "Bernie", "blue", "orange"), width = 3))) %>%
  add_trace(y = ~Hillary_Clinton, name = 'Hillary Clinton', marker = list(color = "#blue")) %>%
  layout(title = "Primary Elections Democratic Party Votes Dispersion",
         xaxis = list(title = "States"),
         yaxis = list(title = 'Votes', range=c(0, 3500000)), barmode = 'stack')


#republican bar chart
plot_ly(rep_by_state, x = ~abb, y = ~Donald_Trump, type = 'bar', name = 'Donald Trump', 
        marker = list(color = "#red", line = list(color = "#red", width = 3))) %>%
  add_trace(y = ~John_Kasich, name = 'John Kasich', marker = list(color = "#green")) %>%
  add_trace(y = ~Marco_Rubio, name = 'Marco Rubio', marker = list(color = "#yellow")) %>%
  add_trace(y = ~Ted_Cruz, name = 'Ted Cruz', marker = list(color = "#purple")) %>%
  add_trace(y = ~Ben_Carson, name = 'Ben Carson', marker = list(color = "#cyan")) %>%
  layout(title = "Primary Elections Republican Party Votes Dispersion",
         xaxis = list(title = "States"),
         yaxis = list(title = 'Votes', range=c(0, 3000000)), barmode = 'stack')


#democratic choropleth map
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

plot_geo(dem_by_state, locationmode = 'USA-states', showscale = FALSE) %>%
  add_trace(
    z = ~z,
    text = ~winner,
    locations = ~abb,
    color = ~z,
    colors = c('blue', 'orange')
  ) %>%
  layout(
    title = 'Bernie Vs Hillary Map',
    geo = g
  )



#republican choropleth map
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

plot_geo(rep_by_state, locationmode = 'USA-states', showscale = FALSE) %>%
  add_trace(
    z = ~z,
    text = ~winner,
    locations = ~abb,
    color = ~z,
    colors = c('red')
  ) %>%
  layout(
    title = 'Republican Primary Map',
    geo = g
  )



# Create data by county
#select all information except candidate and votes and filter by choosing any candidate
join_with <- final_data  %>% filter(candidate == 'Bernie Sanders') %>%
  select(-candidate, -votes)

bernie_by_county <- ByCounty(final_data, "Bernie Sanders")
hillary_by_county <- ByCounty(final_data, "Hillary Clinton")

#Join data
dem_by_county <- left_join(join_with, bernie_by_county, by=c("abb", "county")) %>%
  left_join(., hillary_by_county, by=c("abb", "county")) %>% 
  mutate(winner= ifelse(Bernie_Sanders > Hillary_Clinton, "Bernie", "Hillary"), z = ifelse(winner == "Bernie", 1, 0))
nrow(dem_by_county) #2798/4205
# View(dem_by_county)

# stats
bernie_countys <- nrow(dem_by_county %>% filter(winner=="Bernie")) #1129counties
hillary_countys <- nrow(dem_by_county %>% filter(winner=="Hillary")) #1669 counties
bernie_votes <- sum(dem_by_county$Bernie_Sanders)
hillary_votes <- sum(dem_by_county$Hillary_Clinton)

#counties won bar chart
plot_ly(x = "Bernie", name = "Bernie", y = bernie_countys, type = "bar", marker = list(color = "#blue")) %>%
  add_trace(x = "Hillary", name = "Hillary", y = hillary_countys, marker = list(color = "#orange")) %>%
  layout(title = "Countys Won for Democratic Candidates",
         xaxis = list(title = "Candidates "),
         yaxis = list(title = 'Countys won', range=c(0, 1800)))

#popular vote bar chart
plot_ly(x = "Bernie", name = "Bernie", y = bernie_votes, type = "bar", marker = list(color = "#blue")) %>%
  add_trace(x = "Hillary", name = "Hillary", y = hillary_votes, marker = list(color = "#orange")) %>%
  layout(title = "Popular vote for Democratic Candidates",
         xaxis = list(title = "Candidates "),
         yaxis = list(title = 'Popular Vote', range=c(0, 16000000)))


# Now group by state2
test <- ByState(dem_by_county, "Bernie Sanders")
View(test)