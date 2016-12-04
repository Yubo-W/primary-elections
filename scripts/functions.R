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
  filtered.blacks <- dem_by_county %>% filter(black >= head(input1, n=1) & black < tail(input1, n=1))
  filtered.bachelors <- filtered.blacks %>% filter(bachelors >= head(input2, n=1) & bachelors < tail(input2, n=1))
  filtered.income <- filtered.bachelors %>% filter(income >= head(input3, n=1) & income < tail(input3, n=1))
  return (filtered.income)
}