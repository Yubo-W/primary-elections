library(dplyr)
library(plotly)

# setwd('/Users/iguest/Documents/final-project-I-mWithHer')

primary <- read.csv('./data/primary_results.csv', stringsAsFactors = FALSE)
county <- read.csv('./data/county_facts.csv', stringsAsFactors = FALSE)
#county.key <- read.csv('./data/county_facts_key.csv', stringsAsFactors = FALSE)
#View(county.key)
source("./scripts/functions.R")

#create final data frame
joined_data <- left_join(primary, county, by="fips")
final_data <- joined_data %>% na.omit() %>%
              select(state, state_abbreviation, county, party, candidate, votes,
                     SEX255214, RHI225214, RHI325214, RHI425214, RHI525214, RHI625214,
                     RHI725214, RHI825214, EDU635213, EDU685213)
colnames(final_data) <- c('state', 'abb', 'county', 'party', 'candidate', 'votes',
                          'female', 'black', 'indian', 'asian', 'hawaiian', 'multi', 'hispanic',
                          'white', 'highschool', 'bachelors')
# View(final_data)
nrow(final_data) #17479 / 24611 (over 7000 missing)


#combining data by state
bernie_by_state <- ByState("Bernie Sanders")
hillary_by_state <- ByState("Hillary Clinton")
trump_by_state <- ByState('Donald Trump')
kasich_by_state <- ByState("John Kasich")
rubio_by_state <- ByState("Marco Rubio")
cruz_by_state <- ByState("Ted Cruz")
carson_by_state <- ByState("Ben Carson")

dem_by_state <- left_join(bernie_by_state, hillary_by_state, by=c("state","abb","county")) %>%
  mutate(winner= ifelse(Bernie_Sanders > Hillary_Clinton, "Bernie", "Hillary"),
         z = ifelse(winner == "Bernie", 1, 0))
View(dem_by_state)

<<<<<<< c044230f0e5a17d6cb8472e92f9ca56d2a453731
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
=======


rep_by_state <- left_join(trump_by_state, kasich_by_state, by="state") %>% 
  left_join(., rubio_by_state, by="state") %>% 
  left_join(., cruz_by_state, by="state") %>% 
  left_join(., carson_by_state, by="state") %>% 
  mutate(winner= ifelse(trump_by_state > kasich_by_state && 
                          trump_by_state > rubio_by_state &&
                          trump_by_state > cruz_by_state &&
                          trump_by_state > carson_by_state,
                        "Bernie", "Hillary"),
         z = ifelse(winner == "Bernie", 1, 0))
>>>>>>> fixed by_county



#stats
nrow(dem_by_state)
# number of states bernie/hillary won
nrow(dem_by_state %>% filter(winner=="Bernie")) #21 states
nrow(dem_by_state %>% filter(winner=="Hillary")) #28 states
# number of overall voters
sum(dem_by_state$bernie_votes) #11959102 votes
sum(dem_by_state$hillary_votes) #15692452 votes


#bar chart
plot_ly(dem_by_state, x = ~abb, y = ~Bernie_Sanders, type = 'bar', name = 'Bernie Sanders', 
        marker = list(color = "#orange", line = list(color = ifelse(dem_by_state$winner == "Bernie", "blue", "orange"), width = 3))) %>%
  add_trace(y = ~Hillary_Clinton, name = 'Hillary Clinton', marker = list(color = "#blue")) %>%
  layout(title = "Primary Elections Democratic Party Votes Dispersion",
         xaxis = list(title = "States"),
         yaxis = list(title = 'Votes'), barmode = 'stack')



#choropleth map
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



################################################################################################
# Create data by county
#select all information and filter by choosing any candidate
join_with <- final_data  %>% filter(candidate == 'Bernie Sanders') %>%
  select(-candidate, -votes)
#change colname for joining
colnames(bernie_by_county)[colnames(bernie_by_county) == "votes"] <- "bernie_votes"

bernie_by_county <- final_data  %>% filter(candidate == 'Bernie Sanders') %>%
  select(county = county, abb = abb, bernie_votes = votes)

hillary_by_county <- final_data  %>% filter(candidate == 'Hillary Clinton') %>% 
  select(county = county, abb = abb, hillary_votes = votes)

# nrow(bernie_by_county)
# nrow(hillary_by_county)
# nrow(dem_by_county)

#Join data
dem_by_county <- left_join(join_with, bernie_by_county, by=c("abb", "county")) %>%
  left_join(., hillary_by_county, by=c("abb", "county")) %>% 
  mutate(winner= ifelse(bernie_votes > hillary_votes, "Bernie", "Hillary"), z = ifelse(winner == "Bernie", 1, 0))
nrow(dem_by_county) #2798/4205
View(dem_by_county)

# stats
bernie_countys <- nrow(dem_by_county %>% filter(winner=="Bernie")) #1129counties
hillary_countys <- nrow(dem_by_county %>% filter(winner=="Hillary")) #1669 counties
bernie_votes <- sum(dem_by_county$bernie_votes)
hillary_votes <- sum(dem_by_county$hillary_votes)

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
