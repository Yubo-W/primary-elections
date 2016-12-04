library(dplyr)
library(plotly)


# read in data
shinyServer(function(input, output) {
  
  primary <- read.csv('./data/primary_results.csv', stringsAsFactors = FALSE)
  county <- read.csv('./data/county_facts.csv', stringsAsFactors = FALSE)
  source("./scripts/functions.R")
  
  #create final data frame
  joined_data <- left_join(primary, county, by="fips")
  final_data <- joined_data %>%
    select(state, state_abbreviation.x, county, party, candidate, votes,
           SEX255214, RHI225214, RHI325214, RHI425214, RHI525214, RHI625214,
           RHI725214, RHI825214, EDU635213, EDU685213, INC110213)
  colnames(final_data) <- c('state', 'abb', 'county', 'party', 'candidate', 'votes',
                            'female', 'black', 'indian', 'asian', 'hawaiian', 'multi', 'hispanic',
                            'white', 'highschool', 'bachelors', 'income')
  # View(final_data)
  
  
  
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
    filtered.df <- FilterByUserInput(dem_by_county, input$slider1, input$slider2, input$slider3)
    
    # stats
    bernie_counties <- nrow(filtered.df %>% filter(winner=="Bernie"))
    hillary_counties <- nrow(filtered.df %>% filter(winner=="Hillary"))

    
    #county bar chart
    p <- plot_ly(x = "Bernie", name = "Bernie", y = bernie_counties, type = "bar", marker = list(color = "#blue")) %>%
      add_trace(x = "Hillary", name = "Hillary", y = hillary_counties, marker = list(color = "#orange")) %>%
      layout(title = "Counties Won for Democratic Candidates",
             xaxis = list(title = "Candidates "),
             yaxis = list(title = 'Counties won', range=c(0, 1800)))
    return (p)
  })
  
  # bar plot2: democrat popular vote
  output$plot2 <- renderPlotly({
    #filter based on user input
    filtered.df <- FilterByUserInput(dem_by_county, input$slider1, input$slider2, input$slider3)
    
    # stats
    bernie_votes <- sum(filtered.df$Bernie_Sanders)
    hillary_votes <- sum(filtered.df$Hillary_Clinton)
    
    #county bar chart
    p <- plot_ly(x = "Bernie", name = "Bernie", y = bernie_votes, type = "bar", marker = list(color = "#blue")) %>%
      add_trace(x = "Hillary", name = "Hillary", y = hillary_votes, marker = list(color = "#orange")) %>%
      layout(title = "Popular Vote for Democratic Candidates",
             xaxis = list(title = "Candidates"),
             yaxis = list(title = 'Popular vote', range=c(0, 16000000)))
    return (p)
  })
  
  
  # chart #3
  # output$plot3 <- renderPlotly({
  #   return (p)
  # })
  
  
})
  