library(dplyr)


# read in data
shinyServer(function(input, output) {
  
  primary <- read.csv('./data/primary_results.csv', stringsAsFactors = FALSE)
  county <- read.csv('./data/county_facts.csv', stringsAsFactors = FALSE)
  source("./scripts/functions.R")
  
  #create final data frame
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
  # View(final_data)
  
  
  
  # Create data by county
  #select all information except candidate and votes and filter by choosing any candidate
  join_with <- final_data  %>% filter(candidate == 'Donald Trump') %>%
    select(-candidate, -votes)
  
  trump_by_county <- ByCounty(final_data, "Donald Trump")
  kasich_by_county <- ByCounty(final_data, "John Kasich")
  rubio_by_county <- ByCounty(final_data, "Marco Rubio")
  cruz_by_county <- ByCounty(final_data, "Ted Cruz")
  carson_by_county <- ByCounty(final_data, "Ben Carson")
  
  #Join data
  dem_by_county <- left_join(join_with, bernie_by_county, by=c("abb", "county")) %>%
    left_join(., hillary_by_county, by=c("abb", "county")) %>% 
    mutate(winner = ifelse(Bernie_Sanders > Hillary_Clinton, "Bernie", "Hillary"), z = ifelse(winner == "Bernie", 1, 0))
  nrow(dem_by_county)
  
  rep_by_county <- left_join(join_with, trump_by_county, by=c("abb", "county")) %>%
    left_join(., kasich_by_county, by=c("abb", "county")) %>% 
    left_join(., rubio_by_county, by=c("abb", "county")) %>% 
    left_join(., cruz_by_county, by=c("abb", "county")) %>% 
    left_join(., carson_by_county, by=c("abb", "county")) %>% 
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
  colnames(rep_by_county) <- c('state', 'abb', 'county', 'party', 'female', 'black', 'indian', 'asian', 'hawaiian',
                              'multi', 'hispanic', 'white', 'highschool', 'bachelors', 'income',
                              'Donald_Trump', 'John_Kasich', 'Marco_Rubio', 'Ted_Cruz', 'Ben_Carson',
                              'winner', 'z')
  
  
  # bar plot1: democrat counties won
  output$plot1 <- renderPlotly({
    #filter based on user input
    filtered.df <- FilterByUserInput(rep_by_county, input$race1, input$race2, input$race3, input$race4, input$education1, input$education2, input$income1)
    
    # stats
    trump_counties <- nrow(filtered.df %>% filter(winner=="Trump"))
    kasich_counties <- nrow(filtered.df %>% filter(winner=="Kasich"))
    rubio_counties <- nrow(filtered.df %>% filter(winner=="Rubio"))
    cruz_counties <- nrow(filtered.df %>% filter(winner=="Cruz"))
    carson_counties <- nrow(filtered.df %>% filter(winner=="Carson"))
        
    
    #county bar chart
    p <- plot_ly(x = "Trump", name = "Trump", y = trump_counties, type = "bar", marker = list(color = "#red")) %>%
      add_trace(x = "Kasich", name = "Kasich", y = kasich_counties, marker = list(color = "#yellow")) %>%
      add_trace(x = "Rubio", name = "Rubio", y = rubio_counties, marker = list(color = "#green")) %>%
      add_trace(x = "Cruz", name = "Cruz", y = cruz_counties, marker = list(color = "#purple")) %>%
      add_trace(x = "Carson", name = "Carson", y = carson_counties, marker = list(color = "#blue")) %>%
      layout(title = "Number of Counties Won",
             xaxis = list(title = "Candidates "),
             yaxis = list(title = 'Counties won', range=c(0, 1800)))
    return (p)
  })
  
  # bar plot2: democrat popular vote
  output$plot2 <- renderPlotly({
    #filter based on user input
    filtered.df <- FilterByUserInput(rep_by_county, input$race1, input$race2, input$race3, input$race4, input$education1, input$education2, input$income1)

    # stats
    trump_votes <- sum(filtered.df$Donald_Trump)
    kasich_votes <- sum(filtered.df$John_Kasich)
    rubio_votes <- sum(filtered.df$Marco_Rubio)
    cruz_votes <- sum(filtered.df$Ted_Cruz)
    carson_votes <- sum(filtered.df$Ben_Carson)
    
    trump_votes <- sum(rep_by_county$Donald_Trump)
    kasich_votes <- sum(rep_by_county$John_Kasich)
    rubio_votes <- sum(rep_by_county$Marco_Rubio)
    cruz_votes <- sum(rep_by_county$Ted_Cruz)
    carson_votes <- sum(rep_by_county$Ben_Carson)
    
    
    #county bar chart
    p <- plot_ly(x = "Trump", name = "Trump", y = trump_votes, type = "bar", marker = list(color = "#red")) %>%
      add_trace(x = "Kasich", name = "Kasich", y = kasich_votes, marker = list(color = "#yellow")) %>%
      add_trace(x = "Rubio", name = "Rubio", y = rubio_votes, marker = list(color = "#green")) %>%
      add_trace(x = "Cruz", name = "Cruz", y = cruz_votes, marker = list(color = "#purple")) %>%
      add_trace(x = "Carson", name = "Carson", y = carson_votes, marker = list(color = "#blue")) %>%
      layout(title = "Overall Popular Vote",
             xaxis = list(title = "Candidates"),
             yaxis = list(title = 'Popular vote', range=c(0, 16000000)))
    return (p)
  })


  # # chart #3
  # output$plot3 <- renderPlotly({
  #   filtered.df <- FilterByUserInput(dem_by_county, input$race1, input$race2, input$race3, input$race4, input$education1, input$education2, input$income1)
  #   
  #   bernie_by_state <- ByState2(filtered.df, "Bernie Sanders")
  #   hillary_by_state <- ByState2(filtered.df, "Hillary Clinton")
  #   dem_by_state <- left_join(bernie_by_state, hillary_by_state, by=c("state","abb","county")) %>%
  #     mutate(winner= ifelse(Bernie_Sanders > Hillary_Clinton, "Bernie", "Hillary"),
  #            z = ifelse(winner == "Bernie", 1, 0))
  #   
  #   p <- plot_ly(dem_by_state, x = ~abb, y = ~Bernie_Sanders, type = 'bar', name = 'Bernie Sanders', 
  #                marker = list(color = "#orange", line = list(color = ifelse(dem_by_state$winner == "Bernie", "blue", "orange"), width = 3))) %>%
  #     add_trace(y = ~Hillary_Clinton, name = 'Hillary Clinton', marker = list(color = "#blue")) %>%
  #     layout(title = "Vote Dispersion for each State",
  #            xaxis = list(title = "States"),
  #            yaxis = list(title = 'Votes', range=c(0, 3500000)), barmode = 'stack')
  #   return (p)
  # })
  # 
  # #  chart 4
  # output$plot4 <- renderPlotly({
  #   filtered.df <- FilterByUserInput(dem_by_county, input$race1, input$race2, input$race3, input$race4, input$education1, input$education2, input$income1)
  #   
  #   bernie_by_state <- ByState2(filtered.df, "Bernie Sanders")
  #   hillary_by_state <- ByState2(filtered.df, "Hillary Clinton")
  #   dem_by_state <- left_join(bernie_by_state, hillary_by_state, by=c("state","abb","county")) %>%
  #     mutate(winner= ifelse(Bernie_Sanders > Hillary_Clinton, "Bernie", "Hillary"),
  #            z = ifelse(winner == "Bernie", 1, 0))
  #   
  #   l <- list(color = toRGB("white"), width = 2)
  #   # specify some map projection/options
  #   g <- list(
  #     scope = 'usa',
  #     projection = list(type = 'albers usa'),
  #     showlakes = TRUE,
  #     lakecolor = toRGB('white')
  #   )
  #   p <- plot_geo(dem_by_state, locationmode = 'USA-states', showscale = FALSE) %>%
  #     add_trace(
  #       z = ~z,
  #       text = ~winner,
  #       locations = ~abb,
  #       color = ~z,
  #       colors = c('orange', 'blue')
  #     ) %>%
  #     layout(
  #       title = 'States Won Visualization Map',
  #       geo = g
  #     )
  #   
  #   return (p)
  # })
  
  
})
