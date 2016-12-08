
# setwd('/Users/iguest/Documents/final-project-I-mWithHer')
primary <- read.csv('./data/primary_results.csv', stringsAsFactors = FALSE)
county <- read.csv('./data/county_facts.csv', stringsAsFactors = FALSE)
source("./scripts/functions.R")



#create final data frame
new.county <- SortData(county)
primary$county <- tolower(primary$county)
joined_data <- left_join(primary, new.county, by=c("county", "state_abbreviation"))
final_data <- joined_data %>% na.omit() %>% 
  select(state, state_abbreviation, county, party, candidate, votes,
         SEX255214, RHI225214, RHI325214, RHI425214, RHI525214, RHI625214,
         RHI725214, RHI825214, EDU635213, EDU685213, INC110213)
colnames(final_data) <- c('state', 'abb', 'county', 'party', 'candidate', 'votes',
                          'female', 'black', 'indian', 'asian', 'hawaiian', 'multi', 'hispanic',
                          'white', 'highschool', 'bachelors', 'income')

#############################################################
# Manually adding in Louisiana and New Hampshire

temp.primary <- primary %>%
  filter(state_abbreviation == 'LA' | state_abbreviation == 'NH')

temp.county <- county %>% 
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

final_data <- rbind(final_data, joined_data)

# Create data by county
#select all information except candidate and votes and filter by choosing any candidate
join_with <- final_data  %>% filter(candidate == 'Bernie Sanders') %>%
  select(-candidate, -votes)

bernie_by_county <- ByCounty(final_data, "Bernie Sanders")
hillary_by_county <- ByCounty(final_data, "Hillary Clinton")

#Join data
dem_by_county <- left_join(join_with, bernie_by_county, by=c("abb", "county")) %>%
  left_join(., hillary_by_county, by=c("abb", "county")) %>% 
  mutate(winner = ifelse(Bernie_Sanders > Hillary_Clinton, "Bernie", "Hillary"), z = ifelse(winner == "Bernie", 1, 0))
nrow(dem_by_county)

bernie_counties <- nrow(dem_by_county %>% filter(winner=="Bernie"))
hillary_counties <- nrow(dem_by_county %>% filter(winner=="Hillary"))

plot_ly(x = "Bernie", name = "Bernie", y = bernie_counties, type = "bar", marker = list(color = "#blue")) %>%
  add_trace(x = "Hillary", name = "Hillary", y = hillary_counties, marker = list(color = "#orange")) %>%
  layout(title = "Number of Counties Won",
         xaxis = list(title = "Candidates "),
         yaxis = list(title = 'Counties won', range=c(0, 1800)))


#################################################################
# pie chart
View(dem_by_county)

bernie_counties <- nrow(dem_by_county %>% filter(winner=="Bernie"))
hillary_counties <- nrow(dem_by_county %>% filter(winner=="Hillary"))

names <- c("Bernie Sanders", "Hillary Clinton")
county_percent <- c(bernie_counties, hillary_counties)

colors <- c('#FF7F0E', '#1F77B4')
plot_ly(labels = names, values = county_percent, type = 'pie',
        marker = list(colors = colors)) %>%
  layout(title = 'Percentage of Counties Won',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



# pie chart 2
bernie_votes <- sum(final_data$Bernie_Sanders)
hillary_votes <- sum(final_data$Hillary_Clinton)

names <- c("Bernie Sanders", "Hillary Clinton")
votes_percent <- c(bernie_votes, hillary_votes)

plot_ly(labels = names, values = votes_percent, type = 'pie') %>%
  layout(title = 'Percentage of Overall Popular Vote',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



#################################################################
View(dem_by_county)
bernie_by_state <- ByState(dem_by_county, "Bernie Sanders")
hillary_by_state <- ByState(dem_by_county, "Hillary Clinton")
dem_by_state <- left_join(bernie_by_state, hillary_by_state, by=c("state","abb","county")) %>%
  mutate(winner= ifelse(Bernie_Sanders > Hillary_Clinton, "Bernie", "Hillary"),
         z = ifelse(winner == "Bernie", 1, 0))
View(dem_by_state)



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
    colors = c('#1F77B4', '#FF7F0E'),
    showlegend = TRUE
  ) %>%
  
  layout(
    title = 'States Won Visualization Map',
    geo = g,
    legend = list(x = 1, y = 0.5)
  )

#################################################################

smoke <- matrix(c(51,43,22,92,28,21,68,22,9),ncol=3,byrow=TRUE)
colnames(o) <- c("High","Low","Middle")
rownames(o) <- c("current","former","never")
smoke <- as.table(smoke)
smoke



