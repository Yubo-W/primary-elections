library(dplyr)
library(plotly)


# read in data
shinyServer(function(input, output) {
  
  primary <- read.csv('./data/primary_results.csv', stringsAsFactors = FALSE)
  county <- read.csv('./data/county_facts.csv', stringsAsFactors = FALSE)
  
  
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
  bernie_by_county <- final_data  %>% 
    filter(candidate == 'Bernie Sanders') %>% 
    group_by(county) %>% 
    summarise(bernie_votes = sum(votes), abb = first(abb), black = mean(black), 
              asian = mean(asian), hispanic = mean(hispanic), white = mean(white),
              highschool = mean(highschool), bachelors = mean(bachelors), income = mean(income))
  hillary_by_county <- final_data  %>% 
    filter(candidate == 'Hillary Clinton') %>% 
    group_by(county) %>% 
    summarise(hillary_votes = sum(votes))
  dem_by_county <- left_join(bernie_by_county, hillary_by_county, by="county") %>%
    mutate(winner= ifelse(bernie_votes > hillary_votes, "Bernie", "Hillary"),
           z = ifelse(winner == "Bernie", 1, 0))  %>%
           na.omit()
  # View(dem_by_county)
  
  
  # bar plot1: democrat counties won
  output$plot1 <- renderPlotly({
    #filter based on user input
    filtered.blacks <- dem_by_county %>% filter(black >= input$slider1)
    filtered.bachelors <- filtered.blacks %>% filter(bachelors >= input$slider2)
    filtered.income <- filtered.bachelors %>% filter(income >= input$slider3)
    filtered.df <- filtered.income
    
    # stats
    nrow(dem_by_county)
    bernie_counties <- nrow(filtered.df %>% filter(winner=="Bernie"))
    hillary_counties <- nrow(filtered.df %>% filter(winner=="Hillary"))
    # sum(filtered.df$bernie_votes)
    # sum(filtered.df$hillary_votes)
    
    #county bar chart
    p <- plot_ly(x = "Bernie", name = "Bernie", y = bernie_counties, type = "bar", marker = list(color = "#blue")) %>%
      add_trace(x = "Hillary", name = "Hillary", y = hillary_counties, marker = list(color = "#orange")) %>%
      layout(title = "Counties Won for Democratic Candidates",
             xaxis = list(title = "Candidates "),
             yaxis = list(title = 'Counties won', range=c(0, 1000)))
    return (p)
  })
  
  # bar plot2: democrat popular vote
  output$plot2 <- renderPlotly({
    #filter based on user input
    filtered.blacks <- dem_by_county %>% filter(black >= input$slider1)
    filtered.bachelors <- filtered.blacks %>% filter(bachelors >= input$slider2)
    filtered.income <- filtered.bachelors %>% filter(income >= input$slider3)
    filtered.df <- filtered.income
    
    # stats
    nrow(dem_by_county)
    bernie_counties <- nrow(filtered.df %>% filter(winner=="Bernie"))
    hillary_counties <- nrow(filtered.df %>% filter(winner=="Hillary"))
    bernie_votes <- sum(filtered.df$bernie_votes)
    hillary_votes <- sum(filtered.df$hillary_votes)
    
    #county bar chart
    p <- plot_ly(x = "Bernie", name = "Bernie", y = bernie_votes, type = "bar", marker = list(color = "#blue")) %>%
      add_trace(x = "Hillary", name = "Hillary", y = hillary_votes, marker = list(color = "#orange")) %>%
      layout(title = "Popular Vote for Democratic Candidates",
             xaxis = list(title = "Candidates "),
             yaxis = list(title = 'Popular vote', range=c(0, 12000000)))
    return (p)
  })
  
  
})
  