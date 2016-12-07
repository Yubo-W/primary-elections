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
SortData <- function(facts) {
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