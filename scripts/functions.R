# Gets votes of desired candidate by state.
ByState <- function(final_data, person) {
  temp <- final_data %>% 
    filter(candidate == person) %>% 
    group_by(state) %>% 
    summarize(votes = sum(votes), abb = first(abb), county = n())
  colnames(temp)[colnames(temp) == "votes"] <- gsub(" ", "_", person)
  return(temp)
}

ByState2 <- function(filtered.df, person) {
  temp.name <- gsub(" ", "_", person)
  colnames(filtered.df)[colnames(filtered.df) == temp.name] <- 'votes'
  temp <- filtered.df %>%
    group_by(state) %>% 
    summarize(votes = sum(votes), abb = first(abb), county = n())
  colnames(temp)[colnames(temp) == "votes"] <- temp.name
  return(temp)
}

ByCounty <- function(final_data, person) {
  temp <- final_data %>% 
    filter(candidate == person) %>% 
    select(county = county, abb = abb, votes = votes)
    colnames(temp)[colnames(temp) == "votes"] <- gsub(" ", "_", person)
    return(temp)
}

FilterByUserInput <- function(dem_by_county, input1, input2, input3) {
  filtered.blacks <- dem_by_county %>% filter(black >= input1)
  filtered.bachelors <- filtered.blacks %>% filter(bachelors >= input2)
  filtered.income <- filtered.bachelors %>% filter(income >= input3)
  return (filtered.income)
}