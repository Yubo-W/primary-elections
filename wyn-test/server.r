library(dplyr)
library(plotly)


# read in data
shinyServer(function(input, output) {
  
  output$plot5 <- renderPlotly({
    filtered.df <- FilterByUserInput(rep_by_county, input$race1, input$race2, input$race3, input$race4, input$education1, input$education2, input$income1)
    
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
                               , width = 3))) %>%
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
             yaxis = list(title = 'Votes', range=c(0, 3500000)), barmode = 'stack')
    return (p)
  })
  
  output$plot6 <- renderPlotly({
    filtered.df <- FilterByUserInput(rep_by_county, input$race1, input$race2, input$race3, input$race4, input$education1, input$education2, input$income1)
    
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
    
    
    # for(i in rep_by_state$state) {
    #   rep_by_state_z <- rep_by_state %>% 
    #     mutate(if(winner=="Donald_Trump") {
    #       z = 0
    #     } else if (winner=="Ted_Cruz") {
    #       z = 1
    #     } else {
    #       z = 2
    #     })
    # }
          
    l <- list(color = toRGB("white"), width = 2)
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    p <- plot_geo(rep_by_state, locationmode = 'USA-states', showscale = FALSE) %>%
      add_trace(
        z = ~z,
        text = ~winner,
        locations = ~abb,
        color = ~z,
        colors = c('#1F77B4', '#e59f14', '36dde2')
      ) %>%
      layout(
        title = 'States Won Visualization Map',
        geo = g
      )
    return (p)
  })
  
})
