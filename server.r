library(dplyr)
library(plotly)


# read in data
shinyServer(function(input, output) {
  
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
  
  
  # bar plot1: democrat counties won
  output$plot1 <- renderPlotly({
    #filter based on user input
    filtered.df <- FilterByUserInput(dem_by_county, input$race1, input$race2, input$race3, input$race4, input$education1, input$education2, input$income1)
    
    # stats
    bernie_counties <- nrow(filtered.df %>% filter(winner=="Bernie"))
    hillary_counties <- nrow(filtered.df %>% filter(winner=="Hillary"))

    
    #county bar chart
    p <- plot_ly(x = "Bernie", name = "Bernie", y = bernie_counties, type = "bar", marker = list(color = "#blue")) %>%
      add_trace(x = "Hillary", name = "Hillary", y = hillary_counties, marker = list(color = "#orange")) %>%
      layout(title = "Number of Counties Won",
             xaxis = list(title = "Candidates "),
             yaxis = list(title = 'Counties won', range=c(0, 1800)))
    return (p)
  })
  
  # bar plot2: democrat popular vote
  output$plot2 <- renderPlotly({
    #filter based on user input
    filtered.df <- FilterByUserInput(dem_by_county, input$race1, input$race2, input$race3, input$race4, input$education1, input$education2, input$income1)
    
    # stats
    bernie_votes <- sum(filtered.df$Bernie_Sanders)
    hillary_votes <- sum(filtered.df$Hillary_Clinton)
    
    #county bar chart
    p <- plot_ly(x = "Bernie", name = "Bernie", y = bernie_votes, type = "bar", marker = list(color = "#blue")) %>%
      add_trace(x = "Hillary", name = "Hillary", y = hillary_votes, marker = list(color = "#orange")) %>%
      layout(title = "Overall Popular Vote",
             xaxis = list(title = "Candidates"),
             yaxis = list(title = 'Popular vote', range=c(0, 16000000)))
    return (p)
  })
  
  
  # chart #3
  output$plot3 <- renderPlotly({
    filtered.df <- FilterByUserInput(dem_by_county, input$race1, input$race2, input$race3, input$race4, input$education1, input$education2, input$income1)
    
    bernie_by_state <- ByState(filtered.df, "Bernie Sanders")
    hillary_by_state <- ByState(filtered.df, "Hillary Clinton")
    dem_by_state <- left_join(bernie_by_state, hillary_by_state, by=c("state","abb","county")) %>%
      mutate(winner= ifelse(Bernie_Sanders > Hillary_Clinton, "Bernie", "Hillary"),
             z = ifelse(winner == "Bernie", 1, 0))
    
    p <- plot_ly(dem_by_state, x = ~abb, y = ~Bernie_Sanders, type = 'bar', name = 'Bernie Sanders', 
            marker = list(color = "#orange", line = list(color = ifelse(dem_by_state$winner == "Bernie", "blue", "orange"), width = 3))) %>%
      add_trace(y = ~Hillary_Clinton, name = 'Hillary Clinton', marker = list(color = "#blue")) %>%
      layout(title = "Vote Dispersion for each State",
             xaxis = list(title = "States"),
             yaxis = list(title = 'Votes', range=c(0, 3500000)), barmode = 'stack')
    return (p)
  })
  
  #  chart 4
  output$plot4 <- renderPlotly({
    filtered.df <- FilterByUserInput(dem_by_county, input$race1, input$race2, input$race3, input$race4, input$education1, input$education2, input$income1)
    
    bernie_by_state <- ByState(filtered.df, "Bernie Sanders")
    hillary_by_state <- ByState(filtered.df, "Hillary Clinton")
    dem_by_state <- left_join(bernie_by_state, hillary_by_state, by=c("state","abb","county")) %>%
      mutate(winner= ifelse(Bernie_Sanders > Hillary_Clinton, "Bernie", "Hillary"),
             z = ifelse(winner == "Bernie", 1, 0))
  
    l <- list(color = toRGB("white"), width = 2)
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    p <- plot_geo(dem_by_state, locationmode = 'USA-states', showscale = FALSE) %>%
      add_trace(
        z = ~z,
        text = ~winner,
        locations = ~abb,
        color = ~z,
        colors = c('orange', 'blue')
      ) %>%
      layout(
        title = 'States Won Visualization Map',
        geo = g
      )
    return (p)
  })
})
  