
primary <- read.csv('./data/primary_results.csv', stringsAsFactors = FALSE)
county <- read.csv('./data/county_facts.csv', stringsAsFactors = FALSE)
source("./scripts/functions.R")

# Creating the finalized data frame, joining the county and voting data together.
new.county <- CountyData(county)
primary$county <- tolower(primary$county)
joined_data <- left_join(primary, new.county, by=c("county", "state_abbreviation"))
final_data <- joined_data %>% na.omit() %>% 
  select(state, state_abbreviation, county, party, candidate, votes,
         SEX255214, RHI225214, RHI325214, RHI425214, RHI525214, RHI625214,
         RHI725214, RHI825214, EDU635213, EDU685213, INC110213)
colnames(final_data) <- c('state', 'abb', 'county', 'party', 'candidate', 'votes',
                          'female', 'black', 'indian', 'asian', 'hawaiian', 'multi', 'hispanic',
                          'white', 'highschool', 'bachelors', 'income')
# Data frame for data in New Hampshire and Louisiana.
temp.primary <- primary %>%
  filter(state_abbreviation == 'LA' | state_abbreviation == 'NH')

temp.county <- county %>% 
  filter(state_abbreviation == 'LA' | state_abbreviation == 'NH')

new.temp.county <- CountyData(temp.county)
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

# Making the final data, joining previous final data with manually created dataframe for NH and LA.
final_data <- rbind(final_data, joined_data)

# Create data by county
# Select all information except candidate and votes and filter by choosing any candidate
join_with <- final_data  %>% filter(candidate == 'Bernie Sanders') %>%
  select(-candidate, -votes)

# Candidates by county.
bernie_by_county <- ByCounty(final_data, "Bernie Sanders")
hillary_by_county <- ByCounty(final_data, "Hillary Clinton")

# Join candidates data.
dem_by_county <- left_join(join_with, bernie_by_county, by=c("abb", "county")) %>%
  left_join(., hillary_by_county, by=c("abb", "county")) %>% 
  mutate(winner = ifelse(Bernie_Sanders > Hillary_Clinton, "Bernie", "Hillary"), z = ifelse(winner == "Bernie", 1, 0))
nrow(dem_by_county)




######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################


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
# View(dem_by_county)
bernie_by_state <- ByState(dem_by_county, "Bernie Sanders")
hillary_by_state <- ByState(dem_by_county, "Hillary Clinton")
dem_by_state <- left_join(bernie_by_state, hillary_by_state, by=c("state","abb","county")) %>%
  mutate(winner= ifelse(Bernie_Sanders > Hillary_Clinton, "Bernie", "Hillary"),
         z = ifelse(winner == "Bernie", 1, 0))
View(dem_by_state)


dem_by_state$hover <- with(dem_by_state, paste( 
        'Winner:', winner, '<br>', 'Hillary votes:', Hillary_Clinton, '<br>', 'Bernie votes:', Bernie_Sanders,
         state, '<br>', '# of counties:', county))


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
    text = ~hover,
    hoverinfo = "text",
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
# tables
bernie_electoral <- nrow(dem_by_state %>% filter(winner == "Bernie"))
hillary_electoral <- nrow(dem_by_state %>% filter(winner == "Hillary"))
total_states <- bernie_electoral + hillary_electoral
winner <- ifelse(bernie_electoral > hillary_electoral, "Bernie", "Hillary")

table <- data.frame(bernie_electoral, hillary_electoral, total_states, winner)
colnames(table) <- c("Bernie","Hillary", "States with data", "Winner")
table

