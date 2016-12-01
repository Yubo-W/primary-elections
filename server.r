library(dplyr)
library(plotly)

primary <- read.csv('./data/primary_results.csv', stringsAsFactors = FALSE)
county <- read.csv('./data/county_facts.csv', stringsAsFactors = FALSE)

joined_data <- left_join(primary, county, by="fips")
joined_data$state_abbreviation.y <- NULL
joined_data$area_name <- NULL

bernie_by_state <- joined_data  %>% 
                  select(state, candidate, votes) %>% 
                  filter(candidate == 'Bernie Sanders') %>% 
                  group_by(state) %>% 
                  summarise(bernie_votes = sum(votes))
hillary_by_state <- joined_data  %>% 
                  select(state, candidate, votes) %>% 
                  filter(candidate == 'Hillary Clinton') %>% 
                  group_by(state) %>% 
                  summarise(hillary_votes = sum(votes))
joined_by_state_dem <- left_join(bernie_by_state, hillary_by_state, by="state")


p <- plot_ly(joined_by_state_dem, x = ~state, y = ~bernie_votes, type = 'bar', name = 'Bernie Sanders') %>%
  add_trace(y = ~hillary_votes, name = 'Hillary Clinton') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')


# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

map <- plot_geo(joined_by_state_dem, locationmode = 'USA-states') %>%
  add_trace(
    z1 = ~bernie_votes, text = "Hillary Clinton", locations = ~state,
    color = ~bernie_votes, colors = 'Purples'
  ) %>%
  colorbar(title = "Millions USD") %>%
  layout(
    title = '2011 US Agriculture Exports by State<br>(Hover for breakdown)',
    geo = g
  )
