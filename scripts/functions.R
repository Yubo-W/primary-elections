# Gets votes of desired candidate by state.
ByState <- function(person) {
  temp <- final_data %>% 
    filter(candidate == person) %>% 
    group_by(state) %>% 
    summarize(votes = sum(votes), abb = first(abb), county = n())
  colnames(temp)[colnames(temp) == "votes"] <- gsub(" ", "_", person)
  return(temp)
}

ByCounty <- function(person) {
  temp <- final_data %>% 
    filter(candidate == person) %>% 
    select(county = county, abb = abb, votes = votes)
    colnames(temp)[colnames(temp) == "votes"] <- gsub(" ", "_", person)
    return(temp)
}
