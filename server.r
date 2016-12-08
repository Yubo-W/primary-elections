# library the required packages.
library(dplyr)
library(plotly)
library(shiny)

# Shiny server
shinyServer(function(input, output) {
  
  # Reading in data files.
  primary <- read.csv('./data/primary_results.csv', stringsAsFactors = FALSE)
  county <- read.csv('./data/county_facts.csv', stringsAsFactors = FALSE)
  source("./scripts/functions.R")
  
  # Creating the finalized data frame, joining the county and voting data together.
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
  
  ######################################################################################
  # Data frame for dat ain New Hampshire and Louisiana.
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
  ######################################################################################
  
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


  # Democratic Party plots.
  # Bar plot1: democrat counties won
  output$plot1 <- renderPlotly({
    #filter based on user input
    filtered.df <- FilterByUserInput(dem_by_county, input$race1, input$race2, input$race3, input$race4, input$education1, input$education2, input$income1)
    
    # stats
    bernie_counties <- filtered.df %>% filter(winner=="Bernie") %>% nrow()
    hillary_counties <- filtered.df %>% filter(winner=="Hillary") %>% nrow()

    # county bar chart
    p <- plot_ly(x = "Bernie", name = "Bernie", y = bernie_counties, type = "bar", marker = list(color = "#FF7F0E")) %>%
      add_trace(x = "Hillary", name = "Hillary", y = hillary_counties, marker = list(color = "#1F77B4")) %>%
      layout(title = "Number of Counties Won",
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
    p <- plot_ly(x = "Bernie", name = "Bernie", y = bernie_votes, type = "bar", marker = list(color = "#FF7F0E")) %>%
      add_trace(x = "Hillary", name = "Hillary", y = hillary_votes, marker = list(color = "#1F77B4")) %>%
      layout(title = "Overall Popular Vote",
             yaxis = list(title = 'Popular vote', range=c(0, 16000000)))
    return (p)
  })
  
  # pie chart 1: democratic county wins by percent
  output$plot3 <- renderPlotly({
    filtered.df <- FilterByUserInput(dem_by_county, input$race1, input$race2, input$race3, input$race4, input$education1, input$education2, input$income1)
    
    bernie_counties <- nrow(filtered.df %>% filter(winner=="Bernie"))
    hillary_counties <- nrow(filtered.df %>% filter(winner=="Hillary"))
    
    names <- c("Bernie Sanders", "Hillary Clinton")
    county_percent <- c(bernie_counties, hillary_counties)
    
    plot_ly(labels = names, values = county_percent, type = 'pie') %>%
      layout(title = 'Percentage of Counties Won',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  # pie chart 2: Overall democratic popular vote by percent.
  output$plot4 <- renderPlotly({
    filtered.df <- FilterByUserInput(dem_by_county, input$race1, input$race2, input$race3, input$race4, input$education1, input$education2, input$income1)
    
    bernie_votes <- sum(filtered.df$Bernie_Sanders)
    hillary_votes <- sum(filtered.df$Hillary_Clinton)
    
    names <- c("Bernie Sanders", "Hillary Clinton")
    votes_percent <- c(bernie_votes, hillary_votes)
    
    plot_ly(labels = names, values = votes_percent, type = 'pie') %>%
      layout(title = 'Percentage of Overall Popular Vote',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  
  # By State Stacked Bar chart
  output$plot5 <- renderPlotly({
    filtered.df <- FilterByUserInput(dem_by_county, input$race1, input$race2, input$race3, input$race4, input$education1, input$education2, input$income1)
    
    bernie_by_state <- ByState(filtered.df, "Bernie Sanders")
    hillary_by_state <- ByState(filtered.df, "Hillary Clinton")
    dem_by_state <- left_join(bernie_by_state, hillary_by_state, by=c("state","abb","county")) %>%
      mutate(winner= ifelse(Bernie_Sanders > Hillary_Clinton, "Bernie", "Hillary"),
             z = ifelse(winner == "Bernie", 1, 0))
    
    p <- plot_ly(dem_by_state, x = ~abb, y = ~Bernie_Sanders, type = 'bar', name = 'Bernie Sanders', 
            marker = list(color = "#FF7F0E", line = list(color = ifelse(dem_by_state$winner == "Bernie", "orange", "blue"), width = 3))) %>%
      add_trace(y = ~Hillary_Clinton, name = 'Hillary Clinton', marker = list(color = "#1F77B4")) %>%
      layout(title = "Vote Dispersion for each State",
             xaxis = list(title = "States"),
             yaxis = list(title = 'Votes', range=c(0, 3500000)), barmode = 'stack')
    return (p)
  })
  
  # By State Map
  output$plot6 <- renderPlotly({
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
        colors = c('#1F77B4', '#FF7F0E')
      ) %>%
      layout(
        title = 'States Won Visualization Map',
        geo = g
      )
    return (p)
  })

  ######################################################################################
  # Republican by County data.
  mod_rep <- final_data %>% filter(party == "Republican") %>% select(-candidate, -votes)
  
  # data on individual candidate.
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
  
  # joining the data of all the republican candidates.
  rep_join <- left_join(mod_rep, trump, by = c("county", "abb"))
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
  
  # making "row" column for republican data and replacing NA with 0.
  rep_by_county <- rep_join %>% unique() %>% mutate(row = seq(1:nrow(.)))
  rep_by_county[is.na(rep_by_county)] <- 0
  
  # dataframe of potential winners for each county.
  rep_winners <- rep_by_county %>% 
    select(-state, -female, -county, -abb, -party, -black, -indian, -asian, -hawaiian, -multi, -hispanic,
           -white, -highschool, -bachelors, -income, -row)
  
  # Finding the candidate with the most votes by row and assigning that candidate as the winner.
  winner <- as.data.frame(cbind(row.names(rep_winners),apply(rep_winners,1,function(x)
    names(rep_winners)[which(x==max(x))])))
  
  # Making a "row" column for the winner.
  winner$row <- seq(1:nrow(winner))
  
  # making the final republican by county dataframe.
  rep_by_county <- left_join(rep_by_county, winner, by = "row")
  names(rep_by_county)[names(rep_by_county) == "V1"] <- "remove"
  names(rep_by_county)[names(rep_by_county) == "V2"] <- "winner"
  rep_by_county <- rep_by_county %>% 
    select(-remove, -row)
  ######################################################################################
  
  # Republican Party plots.
  
  # bar plot1: republican counties won
  output$rep_plot1 <- renderPlotly({
    #filter based on user input
    filtered.df <- FilterByUserInput(rep_by_county, input$rep_race1, input$rep_race2, input$rep_race3, input$rep_race4,
                                     input$rep_education1, input$rep_education2, input$rep_income1)
    
    # stats
    ben_counties <- nrow(filtered.df %>% filter(winner=="Ben_Carson"))
    donald_counties <- nrow(filtered.df %>% filter(winner=="Donald_Trump"))
    john_counties <- nrow(filtered.df %>% filter(winner=="John_Kasich"))
    marco_counties <- nrow(filtered.df %>% filter(winner=="Marco_Rubio"))
    ted_counties <- nrow(filtered.df %>% filter(winner=="Ted_Cruz"))
    carly_counties <- nrow(filtered.df %>% filter(winner=="Carly_Fiorina"))
    chris_counties <- nrow(filtered.df %>% filter(winner=="Chris_Christie"))
    jeb_counties <- nrow(filtered.df %>% filter(winner=="Jeb_Bush"))
    mike_counties <- nrow(filtered.df %>% filter(winner=="Mike_Huckabee"))
    rand_counties <- nrow(filtered.df %>% filter(winner=="Rand_Paul"))
    rick_counties <- nrow(filtered.df %>% filter(winner=="Rick_Santorum"))
    
    #county bar chart
    p <- plot_ly(x = "Ben", name = "Ben", y = ben_counties, type = "bar", marker = list(color = "#FF7F0E")) %>%
      add_trace(x = "Donald", name = "Donald", y = donald_counties, marker = list(color = "#1F77B4")) %>%
      add_trace(x = "John", name = "John", y = john_counties, marker = list(color = "#36dde2")) %>%
      add_trace(x = "Marco", name = "Marco", y = marco_counties, marker = list(color = "#f9f61b")) %>%
      add_trace(x = "Ted", name = "Ted", y = ted_counties, marker = list(color = "#e59f14")) %>%
      add_trace(x = "Carly", name = "Carly", y = carly_counties, marker = list(color = "#e54514")) %>%
      add_trace(x = "Chris", name = "Chris", y = chris_counties, marker = list(color = "#14e518")) %>%
      add_trace(x = "Jeb", name = "Jeb", y = jeb_counties, marker = list(color = "#7214e5")) %>%
      add_trace(x = "Mike", name = "Mike", y = mike_counties, marker = list(color = "#b814e5")) %>%
      add_trace(x = "Rand", name = "Rand", y = rand_counties, marker = list(color = "#000000")) %>%
      add_trace(x = "Rick", name = "Rick", y = rick_counties, marker = list(color = "#5b585b")) %>%
      layout(title = "Number of Counties Won",
             yaxis = list(title = 'Counties won', range=c(0, 2000)))
    return (p)
  })
  
  # bar plot2: republican popular vote
  output$rep_plot2 <- renderPlotly({
    #filter based on user input
    filtered.df <- FilterByUserInput(rep_by_county, input$rep_race1, input$rep_race2, input$rep_race3, input$rep_race4,
                                     input$rep_education1, input$rep_education2, input$rep_income1)
    
    # stats
    carson_votes <- sum(filtered.df$Ben_Carson)
    trump_votes <- sum(filtered.df$Donald_Trump)
    kasich_votes <- sum(filtered.df$John_Kasich)
    rubio_votes <- sum(filtered.df$Marco_Rubio)
    cruz_votes <- sum(filtered.df$Ted_Cruz)
    fiorina_votes <- sum(filtered.df$Carly_Fiorina)
    christie_votes <- sum(filtered.df$Chris_Christie)
    bush_votes <- sum(filtered.df$Jeb_Bush)
    huckabee_votes <- sum(filtered.df$Mike_Huckabee)
    paul_votes <- sum(filtered.df$Rand_Paul)
    santorum_votes <- sum(filtered.df$Rick_Santorum)
    
    #county bar chart
    p <- plot_ly(x = "Ben", name = "Ben", y = carson_votes, type = "bar", marker = list(color = "#FF7F0E")) %>%
      add_trace(x = "Donald", name = "Donald", y = trump_votes, marker = list(color = "#1F77B4")) %>%
      add_trace(x = "John", name = "John", y = kasich_votes, marker = list(color = "#36dde2")) %>%
      add_trace(x = "Marco", name = "Marco", y = rubio_votes, marker = list(color = "#f9f61b")) %>%
      add_trace(x = "Ted", name = "Ted", y = cruz_votes, marker = list(color = "#e59f14")) %>%
      add_trace(x = "Carly", name = "Carly", y = fiorina_votes, marker = list(color = "#e54514")) %>%
      add_trace(x = "Chris", name = "Chris", y = christie_votes, marker = list(color = "#14e518")) %>%
      add_trace(x = "Jeb", name = "Jeb", y = bush_votes, marker = list(color = "#7214e5")) %>%
      add_trace(x = "Mike", name = "Mike", y = huckabee_votes, marker = list(color = "#b814e5")) %>%
      add_trace(x = "Rand", name = "Rand", y = paul_votes, marker = list(color = "#000000")) %>%
      add_trace(x = "Rick", name = "Rick", y = santorum_votes, marker = list(color = "#5b585b")) %>%
      layout(title = "Overall Popular Vote",
             yaxis = list(title = 'Counties won', range=c(0, 14000000)))
    return (p)
  })
  
  # Republican pie chart 1: republican candidates with counties won.
  output$rep_plot3 <- renderPlotly({
    filtered.df <- FilterByUserInput(rep_by_county, input$rep_race1, input$rep_race2, input$rep_race3,
                                     input$rep_race4, input$rep_education1, input$rep_education2, input$rep_income1)
    
    # stats.
    ben_counties <- nrow(filtered.df %>% filter(winner=="Ben_Carson"))
    donald_counties <- nrow(filtered.df %>% filter(winner=="Donald_Trump"))
    john_counties <- nrow(filtered.df %>% filter(winner=="John_Kasich"))
    marco_counties <- nrow(filtered.df %>% filter(winner=="Marco_Rubio"))
    ted_counties <- nrow(filtered.df %>% filter(winner=="Ted_Cruz"))
    carly_counties <- nrow(filtered.df %>% filter(winner=="Carly_Fiorina"))
    chris_counties <- nrow(filtered.df %>% filter(winner=="Chris_Christie"))
    jeb_counties <- nrow(filtered.df %>% filter(winner=="Jeb_Bush"))
    mike_counties <- nrow(filtered.df %>% filter(winner=="Mike_Huckabee"))
    rand_counties <- nrow(filtered.df %>% filter(winner=="Rand_Paul"))
    rick_counties <- nrow(filtered.df %>% filter(winner=="Rick_Santorum"))
    
    # data for the pie chart.
    names <- c("Ben Carson", "Donald Trump", "John Kasich", "Marco Rubio", "Ted Cruz",
               "Carly Fiorina", "Chris Christie", "Jeb Bush", "Mike Huckabee", "Rand Paul",
               "Rick Santorum")
    county_percent <- c(ben_counties, donald_counties, john_counties, marco_counties,
                        ted_counties, carly_counties, chris_counties, jeb_counties, mike_counties,
                        rand_counties, rick_counties)
    
    # making pie chart.
    plot_ly(labels = names, values = county_percent, type = 'pie') %>%
      layout(title = 'Percentage of Counties Won',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  # Republican pie chart 2: republican overall state vote data.
  output$rep_plot4 <- renderPlotly({
    filtered.df <- FilterByUserInput(rep_by_county, input$rep_race1, input$rep_race2, input$rep_race3,
                                     input$rep_race4, input$rep_education1, input$rep_education2, input$rep_income1)
    
    # stats
    carson_votes <- sum(filtered.df$Ben_Carson)
    trump_votes <- sum(filtered.df$Donald_Trump)
    kasich_votes <- sum(filtered.df$John_Kasich)
    rubio_votes <- sum(filtered.df$Marco_Rubio)
    cruz_votes <- sum(filtered.df$Ted_Cruz)
    fiorina_votes <- sum(filtered.df$Carly_Fiorina)
    christie_votes <- sum(filtered.df$Chris_Christie)
    bush_votes <- sum(filtered.df$Jeb_Bush)
    huckabee_votes <- sum(filtered.df$Mike_Huckabee)
    paul_votes <- sum(filtered.df$Rand_Paul)
    santorum_votes <- sum(filtered.df$Rick_Santorum)
    
    # data for pie chart.
    names <- c("Ben Carson", "Donald Trump", "John Kasich", "Marco Rubio", "Ted Cruz",
               "Carly Fiorina", "Chris Christie", "Jeb Bush", "Mike Huckabee", "Rand Paul",
               "Rick Santorum")
    county_percent <- c(carson_votes, trump_votes, kasich_votes, rubio_votes,
                        cruz_votes, fiorina_votes, christie_votes, bush_votes, huckabee_votes,
                        paul_votes, santorum_votes)
    
    # making the pie chart.
    plot_ly(labels = names, values = county_percent, type = 'pie') %>%
      layout(title = 'Percentage of Overall Popular Vote',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  #Republican Stack Bar
  output$rep_plot5 <- renderPlotly({
    filtered.df <- FilterByUserInput(rep_by_county, input$race1, input$race2, input$race3, input$race4, input$education1, input$education2, input$income1)
    
    # stats
    carson_by_state <- ByState(filtered.df, "Ben Carson")
    trump_by_state <- ByState(filtered.df, "Donald Trump")
    kasich_by_state <- ByState(filtered.df, "John Kasich")
    rubio_by_state <- ByState(filtered.df, "Marco Rubio")
    cruz_by_state <- ByState(filtered.df, "Ted Cruz")
    fiorina_by_state <- ByState(filtered.df, "Carly Fiorina")
    christie_by_state <- ByState(filtered.df, "Chris Christie")
    bush_by_state <- ByState(filtered.df, "Jeb Bush")
    huckabee_by_state <- ByState(filtered.df, "Mike Huckabee")
    paul_by_state <- ByState(filtered.df, "Rand Paul")
    santorum_by_state <- ByState(filtered.df, "Rick Santorum")
    
    
    rep_by_state <- left_join(carson_by_state, trump_by_state, by=c("state","abb","county")) %>% 
      left_join(., kasich_by_state, by=c("state","abb","county")) %>% 
      left_join(., rubio_by_state, by=c("state","abb","county")) %>% 
      left_join(., cruz_by_state, by=c("state","abb","county")) %>% 
      left_join(., fiorina_by_state, by=c("state","abb","county")) %>% 
      left_join(., christie_by_state, by=c("state","abb","county")) %>% 
      left_join(., bush_by_state, by=c("state","abb","county")) %>% 
      left_join(., huckabee_by_state, by=c("state","abb","county")) %>% 
      left_join(., paul_by_state, by=c("state","abb","county")) %>% 
      left_join(., santorum_by_state, by=c("state","abb","county"))
    rep_by_state <- rep_by_state[, c('state', 'abb', 'county', 'Ben_Carson', 'Donald_Trump', 'John_Kasich',
                                     'Marco_Rubio', 'Ted_Cruz', 'Carly_Fiorina', 'Chris_Christie', 'Jeb_Bush',
                                     'Mike_Huckabee', 'Rand_Paul', 'Rick_Santorum')]
    
    rep_by_state <- rep_by_state %>% unique() %>% mutate(row = seq(1:nrow(.)))
    rep_state_winners <- rep_by_state %>% 
      select(-state, -abb, -county) %>% 
      mutate(row = seq(1:nrow(.)))
    
    rep_state_winners$row <- seq(1:nrow(rep_state_winners))
    
    state_winner <- as.data.frame(cbind(row.names(rep_state_winners),apply(rep_state_winners,1,function(x)
      names(rep_state_winners)[which(x==max(x))])))
    state_winner$row <- seq(1:nrow(state_winner))
    
    rep_by_state <- left_join(rep_by_state, state_winner, by = "row")
    names(rep_by_state)[names(rep_by_state) == "V1"] <- "remove"
    names(rep_by_state)[names(rep_by_state) == "V2"] <- "winner"
    rep_by_state <- rep_by_state %>% 
      select(-remove)
    
    
    p <- plot_ly(rep_by_state, x = ~abb, y = ~Ben_Carson, type = 'bar', name = 'Ben Carson', 
                 marker = list(color = "##FF7F0E", 
                               line = list(color = '#000000'
                                           #   if(rep_by_state$winner=="Donald_Trump") {
                                           #   color = "#1F77B4"
                                           # } else if (rep_by_state$winner=="Ted_Cruz") {
                                           #   color = "#e59f14"
                                           # } else {
                                           #   color = "#36dde2"
                                           # }
                                           , width = 1))) %>%
      add_trace(y = ~Donald_Trump, name = 'Donald Trump', marker = list(color = "#1F77B4")) %>%
      add_trace(y = ~John_Kasich, name = 'John Kasich', marker = list(color = "#36dde2")) %>%
      add_trace(y = ~Marco_Rubio, name = 'Marco Rubio', marker = list(color = "#f9f61b")) %>%
      add_trace(y = ~Ted_Cruz, name = 'Ted Cruz', marker = list(color = "#e59f14")) %>%
      add_trace(y = ~Carly_Fiorina, name = 'Carly Fiorina', marker = list(color = "#e54514")) %>%
      add_trace(y = ~Chris_Christie, name = 'Chris Christie', marker = list(color = "#14e518")) %>%
      add_trace(y = ~Jeb_Bush, name = 'Jeb Bush', marker = list(color = "#7214e5")) %>%
      add_trace(y = ~Mike_Huckabee, name = 'Mike Huckabee', marker = list(color = "#b814e5")) %>%
      add_trace(y = ~Rand_Paul, name = 'Rand Paul', marker = list(color = "#5e3100")) %>%
      add_trace(y = ~Rick_Santorum, name = 'Rick Santorum', marker = list(color = "#5b585b")) %>%
      layout(title = "Vote Dispersion for each State",
             xaxis = list(title = "States"),
             yaxis = list(title = 'Votes', range=c(0, 3000000)), barmode = 'stack')
    return (p)
  })
})