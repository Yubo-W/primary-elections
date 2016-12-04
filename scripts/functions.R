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
  return(final_data %>% 
    filter(candidate == person) %>% 
    group_by(county) %>% 
    summarise(votes = sum(votes), abb = first(abb), black = mean(black), 
              asian = mean(asian), hispanic = mean(hispanic), white = mean(white),
              highschool = mean(highschool), bachelors = mean(bachelors)))
}