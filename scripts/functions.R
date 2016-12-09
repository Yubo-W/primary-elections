# Gets votes of desired candidate by state by the filtered data.
ByState <- function(filtered.df, person) {
  temp.name <- gsub(" ", "_", person)
  colnames(filtered.df)[colnames(filtered.df) == temp.name] <- 'votes'
  temp <- filtered.df %>%
    group_by(state) %>% 
    summarize(votes = sum(votes), abb = first(abb), county = n())
  colnames(temp)[colnames(temp) == "votes"] <- temp.name
  return(temp)
}

# Gets votes of Desired candidates by county.
ByCounty <- function(final_data, person) {
  temp <- final_data %>% 
    filter(candidate == person) %>% 
    select(county = county, abb = abb, votes = votes)
    colnames(temp)[colnames(temp) == "votes"] <- gsub(" ", "_", person)
    return(temp)
}

# Creates a data frame through user inputs.
FilterByUserInput <- function(df, race1, race2, race3, race4, education1, education2, income1) {
  filtered.blacks <- df %>% filter(black >= head(race1, n=1) & black < tail(race1, n=1))
  filtered.whites <- filtered.blacks %>% filter(white >= head(race2, n=1) & white < tail(race2, n=1))
  filtered.asians <- filtered.whites %>% filter(asian >= head(race3, n=1) & asian < tail(race3, n=1))
  filtered.hispanics <- filtered.asians %>% filter(hispanic >= head(race4, n=1) & hispanic < tail(race4, n=1))
  
  filtered.highschool <- filtered.hispanics %>% filter(highschool >= head(education1, n=1) & highschool < tail(education1, n=1))
  filtered.bachelors <- filtered.highschool %>% filter(bachelors >= head(education2, n=1) & bachelors < tail(education2, n=1))
  filtered.income <- filtered.bachelors %>% filter(income >= head(income1, n=1) & income < tail(income1, n=1))
  return (filtered.income)
}

# Modifies the county facts data so the column area_name is county.
CountyData <- function(facts) {
  new.county <- facts %>% 
    filter(state_abbreviation != "")
  
  new_county_names <- new.county %>% 
    collect() %>% 
    .$area_name %>% 
    gsub(" County", "", .) %>% 
    tolower()
  
  new.county <- new.county %>% 
    select(-area_name)
  
  new.county$county <- new_county_names
  return(new.county)
}

# Returns the dataframe for the Republican party by state.
RepublicanStateData <- function(df) {
  carson_by_state <- ByState(df, "Ben Carson")
  trump_by_state <- ByState(df, "Donald Trump")
  kasich_by_state <- ByState(df, "John Kasich")
  rubio_by_state <- ByState(df, "Marco Rubio")
  cruz_by_state <- ByState(df, "Ted Cruz")
  fiorina_by_state <- ByState(df, "Carly Fiorina")
  christie_by_state <- ByState(df, "Chris Christie")
  bush_by_state <- ByState(df, "Jeb Bush")
  huckabee_by_state <- ByState(df, "Mike Huckabee")
  paul_by_state <- ByState(df, "Rand Paul")
  santorum_by_state <- ByState(df, "Rick Santorum")
  
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
  return(rep_by_state)
}