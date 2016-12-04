# Gets votes of desired candidate by state.
ByState <- function(person) {
  temp <- final_data %>% 
    filter(candidate == person) %>% 
    group_by(state) %>% 
    summarize(votes = sum(quote(votes)))
  return(temp)
}