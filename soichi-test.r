library(dplyr)
library(plotly)

primary <- read.csv('./data/primary_results.csv', stringsAsFactors = FALSE)
county <- read.csv('./data/county_facts.csv', stringsAsFactors = FALSE)
#county.key <- read.csv('./data/county_facts_key.csv', stringsAsFactors = FALSE)
#View(county.key)
source("./scripts/functions.R")

#create final data frame
joined_data <- left_join(primary, county, by="fips")
final_data <- joined_data %>%
              select(state, state_abbreviation.x, county, party, candidate, votes,
                     SEX255214, RHI225214, RHI325214, RHI425214, RHI525214, RHI625214,
                     RHI725214, RHI825214, EDU635213, EDU685213)
colnames(final_data) <- c('state', 'abb', 'county', 'party', 'candidate', 'votes',
                          'female', 'black', 'indian', 'asian', 'hawaiian', 'multi', 'hispanic',
                          'white', 'highschool', 'bachelors')
# View(final_data)



#combining data by state
ByState <- function(person) {
  temp <- final_data %>% 
          filter(candidate == person) %>% 
          group_by(state) %>% 
          summarize(votes = sum(votes), abb = first(abb), county = n())
  return(temp)
}

bernie_by_state <- ByState("Bernie Sanders")
hillary_by_state <- ByState("Hillary Clinton")
trump_by_state <- ByState('Donald Trump')
john_kasich <- ByState("John Kasich")
marco_rubio <- ByState("Marco Rubio")
ted_cruz <- ByState("Ted Cruz")
ben_carson <- ByState("Ben Carson")

dem_by_state <- left_join(bernie_by_state, hillary_by_state, by=c("state","abb","county")) %>%
  mutate(winner= ifelse(bernie_votes > hillary_votes, "Bernie", "Hillary"),
         z = ifelse(winner == "Bernie", 1, 0))
# View(dem_by_state)

rep_by_state <- left_join(trump_by_state, john_kasich, by="state") %>% 
  left_join(., marco_rubio, by="state") %>% 
  left_join(., ted_cruz, by="state") %>% 
  left_join(., ben_carson, by="state") %>% 
  mutate(winner= ifelse(trump_by_state > john_kasich && 
                        trump_by_state > marco_rubio,
                        "Bernie", "Hillary"),
         z = ifelse(winner == "Bernie", 1, 0))



#stats
nrow(dem_by_state)
# number of states bernie/hillary won
nrow(dem_by_state %>% filter(winner=="Bernie")) #21 states
nrow(dem_by_state %>% filter(winner=="Hillary")) #28 states
# number of overall voters
sum(dem_by_state$bernie_votes) #11959102 votes
sum(dem_by_state$hillary_votes) #15692452 votes


#bar chart
plot_ly(dem_by_state, x = ~abb, y = ~bernie_votes, type = 'bar', name = 'Bernie Sanders', 
        marker = list(color = "#orange", line = list(color = ifelse(dem_by_state$winner == "Bernie", "blue", "orange"), width = 3))) %>%
  add_trace(y = ~hillary_votes, name = 'Hillary Clinton', marker = list(color = "#blue")) %>%
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




# Create data by county
bernie_by_county <- ByCounty('Bernie Sanders')
hillary_by_county <- ByCounty('Hillary Clinton')
dem_by_county <- left_join(bernie_by_county, hillary_by_county, by="county") %>%
  mutate(winner= ifelse(bernie_votes > hillary_votes, "Bernie", "Hillary"),
         z = ifelse(winner == "Bernie", 1, 0)) %>%
        na.omit()
#don't turn na's to 0's
#dem_by_county[is.na(dem_by_county)] <- 0
View(dem_by_county)

#filter
filtered.df <- dem_by_county %>% filter(black > 25)

# stats
nrow(dem_by_county)
bernie_countys <- nrow(dem_by_county %>% filter(winner=="Bernie")) #600/1375 countys
hillary_countys <- nrow(dem_by_county %>% filter(winner=="Hillary")) #889/1247 countys
bernie_votes <- sum(dem_by_county$bernie_votes) #7682064/11959102 votes
hillary_votes <- sum(dem_by_county$hillary_votes) #10392931/15692452 votes
nrow(dem_by_county) #1489/2622


#counties won bar chart
plot_ly(x = "Bernie", name = "Bernie", y = bernie_countys, type = "bar", marker = list(color = "#blue")) %>%
  add_trace(x = "Hillary", name = "Hillary", y = hillary_countys, marker = list(color = "#orange")) %>%
  layout(title = "Countys Won for Democratic Candidates",
         xaxis = list(title = "Candidates "),
         yaxis = list(title = 'Countys won', range=c(0, 1000)))

#popular vote bar chart
plot_ly(x = "Bernie", name = "Bernie", y = bernie_votes, type = "bar", marker = list(color = "#blue")) %>%
  add_trace(x = "Hillary", name = "Hillary", y = hillary_votes, marker = list(color = "#orange")) %>%
  layout(title = "Popular vote for Democratic Candidates",
         xaxis = list(title = "Candidates "),
         yaxis = list(title = 'Popular Vote', range=c(0, 12000000)))

