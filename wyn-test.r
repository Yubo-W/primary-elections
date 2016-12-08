library(dplyr)
library(plotly)

#summary data
wyn_rep_by_state <- rep_by_state %>% 
  mutate(total = Ben_Carson + Donald_Trump + John_Kasich + Marco_Rubio + Ted_Cruz
         + Carly_Fiorina + Chris_Christie + Jeb_Bush + Mike_Huckabee + Rand_Paul + Rick_Santorum)

#each candidate percent
for(i in wyn_rep_by_state$state) {
  wyn_rep_by_state <- wyn_rep_by_state %>% 
    mutate(Ben_Carson_percent = round((Ben_Carson / total)*10000)/100)
}
for(i in wyn_rep_by_state$state) {
  wyn_rep_by_state <- wyn_rep_by_state %>% 
    mutate(Donald_Trump_percent = round((Donald_Trump / total)*10000)/100)
}
for(i in wyn_rep_by_state$state) {
  wyn_rep_by_state <- wyn_rep_by_state %>% 
    mutate(John_Kasich_percent = round((John_Kasich / total)*10000)/100)
}
for(i in wyn_rep_by_state$state) {
  wyn_rep_by_state <- wyn_rep_by_state %>% 
    mutate(Marco_Rubio_percent = round((Marco_Rubio / total)*10000)/100)
}
for(i in wyn_rep_by_state$state) {
  wyn_rep_by_state <- wyn_rep_by_state %>% 
    mutate(Ted_Cruz_percent = round((Ted_Cruz / total)*10000)/100)
}
for(i in wyn_rep_by_state$state) {
  wyn_rep_by_state <- wyn_rep_by_state %>% 
    mutate(Carly_Fiorina_percent = round((Carly_Fiorina / total)*10000)/100)
}
for(i in wyn_rep_by_state$state) {
  wyn_rep_by_state <- wyn_rep_by_state %>% 
    mutate(Chris_Christie_percent = round((Chris_Christie / total)*10000)/100)
}
for(i in wyn_rep_by_state$state) {
  wyn_rep_by_state <- wyn_rep_by_state %>% 
    mutate(Jeb_Bush_percent = round((Jeb_Bush / total)*10000)/100)
}
for(i in wyn_rep_by_state$state) {
  wyn_rep_by_state <- wyn_rep_by_state %>% 
    mutate(Mike_Huckabee_percent = round((Mike_Huckabee / total)*10000)/100)
}
for(i in wyn_rep_by_state$state) {
  wyn_rep_by_state <- wyn_rep_by_state %>% 
    mutate(Rand_Paul_percent = round((Rand_Paul / total)*10000)/100)
}
for(i in wyn_rep_by_state$state) {
  wyn_rep_by_state <- wyn_rep_by_state %>% 
    mutate(Rick_Santorum_percent = round((Rick_Santorum / total)*10000)/100)
}

wyn_rep_by_state_test <- wyn_rep_by_state %>% 
  mutate(total_percent = Ben_Carson_percent + Donald_Trump_percent + John_Kasich_percent + Marco_Rubio_percent
         + Ted_Cruz_percent + Carly_Fiorina_percent + Chris_Christie_percent + Jeb_Bush_percent
         + Mike_Huckabee_percent + Rand_Paul_percent + Rick_Santorum_percent)

#generate vectors
y <- vector()
for(i in wyn_rep_by_state$abb) {
  y <- append(y, i)}
x1 <- vector()
for(i in wyn_rep_by_state$Donald_Trump_percent) {
  x1 <-c(x1, i)}
x2 <- vector()
for(i in wyn_rep_by_state$John_Kasich_percent) {
  x2 <-c(x2, i)}
x3 <- vector()
for(i in wyn_rep_by_state$Marco_Rubio_percent) {
  x3 <-c(x3, i)}
x4 <- vector()
for(i in wyn_rep_by_state$Ted_Cruz_percent) {
  x4 <-c(x4, i)}
x5 <- vector()
for(i in wyn_rep_by_state$Ben_Carson_percent) {
  x5 <-c(x5, i)}
x6 <- vector()
for(i in wyn_rep_by_state$Carly_Fiorina_percent) {
  x6 <-c(x6, i)}
x7 <- vector()
for(i in wyn_rep_by_state$Chris_Christie_percent) {
  x7 <-c(x7, i)}
x8 <- vector()
for(i in wyn_rep_by_state$Jeb_Bush_percent) {
  x8 <-c(x8, i)}
x9 <- vector()
for(i in wyn_rep_by_state$Mike_Huckabee_percent) {
  x9 <-c(x9, i)}
x10 <- vector()
for(i in wyn_rep_by_state$Rand_Paul_percent) {
  x10 <-c(x10, i)}
x11 <- vector()
for(i in wyn_rep_by_state$Rick_Santorum_percent) {
  x11 <-c(x11, i)}


df <- data.frame(y, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)


#horizontal bar chart
plot_ly(df, x = ~x1, y = ~y, type = 'bar', orientation = 'h',
              marker = list(color = '#1F77B4',
                            line = list(color = '#000000', width = 1)),
              name = 'Donald Trump') %>%
  add_trace(x = ~x2, marker = list(color = '#36dde2'), name = 'John Kasich') %>%
  add_trace(x = ~x3, marker = list(color = '#f9f61b'), name = 'Marco Rubio') %>%
  add_trace(x = ~x4, marker = list(color = '#e59f14'), name = 'Ted Cruz') %>%
  add_trace(x = ~x5, marker = list(color = '#FF7F0E'), name = 'Ben Carson') %>%
  add_trace(x = ~x6, marker = list(color = '#e54514'), name = 'Carly Fiorina') %>%
  add_trace(x = ~x7, marker = list(color = '#14e518'), name = 'Chris Christie') %>%
  add_trace(x = ~x8, marker = list(color = '#7214e5'), name = 'Jeb Bush') %>% 
  add_trace(x = ~x9, marker = list(color = '#b814e5'), name = 'Mike Huckabee') %>%
  add_trace(x = ~x10, marker = list(color = '#5e3100'), name = 'Rand Paul') %>%
  add_trace(x = ~x11, marker = list(color = '#5b585b'), name = 'Rick Santorum') %>%
  layout(xaxis = list(title = "Primary Elections Republican Party Votes Dispersion",
                      showgrid = FALSE,
                      showline = FALSE,
                      showticklabels = FALSE,
                      zeroline = FALSE,
                      domain = c(0.15, 1)),
         yaxis = list(title = "",
                      showgrid = FALSE,
                      showline = FALSE,
                      showticklabels = FALSE,
                      zeroline = FALSE),
         barmode = 'stack',
         paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
         margin = list(l = 120, r = 10, t = 140, b = 80),
         showlegend = TRUE) %>%
  # labeling the y-axis
  add_annotations(xref = 'paper', yref = 'y', x = 0.14, y = y,
                  xanchor = 'right',
                  text = y,
                  font = list(family = 'Arial', size = 12,
                              color = 'rgb(67, 67, 67)'),
                  showarrow = FALSE, align = 'right') %>%
  # labeling the percentages of each bar (x_axis)
  add_annotations(xref = 'x', yref = 'y',
                  x = x1 / 2, y = y,
                  text = "",
                  font = list(family = 'Arial', size = 12,
                              color = 'rgb(248, 248, 255)'),
                  showarrow = FALSE) %>%
  add_annotations(xref = 'x', yref = 'y',
                  x = x1 + x2 / 2, y = y,
                  text = "",
                  font = list(family = 'Arial', size = 12,
                              color = 'rgb(248, 248, 255)'),
                  showarrow = FALSE) %>%
  add_annotations(xref = 'x', yref = 'y',
                  x = x1 + x2 + x3 / 2, y = y,
                  text = "",
                  font = list(family = 'Arial', size = 12,
                              color = 'rgb(248, 248, 255)'),
                  showarrow = FALSE) %>%
  add_annotations(xref = 'x', yref = 'y',
                  x = x1 + x2 + x3 + x4 / 2, y = y,
                  text = "",
                  font = list(family = 'Arial', size = 12,
                              color = 'rgb(248, 248, 255)'),
                  showarrow = FALSE) %>%
  add_annotations(xref = 'x', yref = 'y',
                  x = x1 + x2 + x3 + x4 + x5 / 2, y = y,
                  text = "",
                  font = list(family = 'Arial', size = 12,
                              color = 'rgb(248, 248, 255)'),
                  showarrow = FALSE) %>%
  add_annotations(xref = 'x', yref = 'y',
                  x = x1 + x2 + x3 + x4 + x5 + x6 / 2, y = y,
                  text = "",
                  font = list(family = 'Arial', size = 12,
                              color = 'rgb(248, 248, 255)'),
                  showarrow = FALSE) %>%
  add_annotations(xref = 'x', yref = 'y',
                  x = x1 + x2 + x3 + x4 + x5 + x6 + x7 / 2, y = y,
                  text = "",
                  font = list(family = 'Arial', size = 12,
                              color = 'rgb(248, 248, 255)'),
                  showarrow = FALSE) %>%
  add_annotations(xref = 'x', yref = 'y',
                  x = x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 / 2, y = y,
                  text = "",
                  font = list(family = 'Arial', size = 12,
                              color = 'rgb(248, 248, 255)'),
                  showarrow = FALSE) %>%
  add_annotations(xref = 'x', yref = 'y',
                  x = x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 / 2, y = y,
                  text = "",
                  font = list(family = 'Arial', size = 12,
                              color = 'rgb(248, 248, 255)'),
                  showarrow = FALSE) %>%
  add_annotations(xref = 'x', yref = 'y',
                  x = x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 / 2, y = y,
                  text = "",
                  font = list(family = 'Arial', size = 12,
                              color = 'rgb(248, 248, 255)'),
                  showarrow = FALSE) %>%
  add_annotations(xref = 'x', yref = 'y',
                  x = x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 / 2, y = y,
                  text = "",
                  font = list(family = 'Arial', size = 12,
                              color = 'rgb(248, 248, 255)'),
                  showarrow = FALSE)

#############################################3
carson_by_state <- ByState(rep_by_county, "Ben Carson")
trump_by_state <- ByState(rep_by_county, "Donald Trump")
kasich_by_state <- ByState(rep_by_county, "John Kasich")
rubio_by_state <- ByState(rep_by_county, "Marco Rubio")
cruz_by_state <- ByState(rep_by_county, "Ted Cruz")
fiorina_by_state <- ByState(rep_by_county, "Carly Fiorina")
christie_by_state <- ByState(rep_by_county, "Chris Christie")
bush_by_state <- ByState(rep_by_county, "Jeb Bush")
huckabee_by_state <- ByState(rep_by_county, "Mike Huckabee")
paul_by_state <- ByState(rep_by_county, "Rand Paul")
santorum_by_state <- ByState(rep_by_county, "Rick Santorum")


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
                           line = list(if(rep_by_state$winner=="Donald_Trump") {
                                          color = "#1F77B4"
                                        } else if (rep_by_state$winner=="Ted_Cruz") {
                                          color = "#e59f14"
                                        } else {
                                          color = "#36dde2"
                                        }, width = 3))) %>%
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


#####TEST
rep_by_state <- rep_by_state %>% mutate(winner = apply(rep_by_state[,4:14], 1, max))
for(i in rep_by_state$winner) {
  if(rep_by_state[i, "winner"] == rep_by_state[i, "Donald_Trump"]) {
    rep_by_state[i, "winner"] <- "Donald"
  }
}
%>% 
  mutate(if(Donald_Trump > John_Kasich && 
            Donald_Trump > Marco_Rubio &&
            Donald_Trump > Ted_Cruz &&
            Donald_Trump > Ben_Carson &&
            Donald_Trump > Carly_Fiorina &&
            Donald_Trump > Chris_Christie &&
            Donald_Trump > Jeb_Bush &&
            Donald_Trump > Mike_Huckabee &&
            Donald_Trump > Rand_Paul &&
            Donald_Trump > Rick_Santorum) {
    winner = "Trump"
  } else if(John_Kasich > Donald_Trump && 
            John_Kasich > Marco_Rubio &&
            John_Kasich > Ted_Cruz &&
            John_Kasich > Ben_Carson &&
            John_Kasich > Carly_Fiorina &&
            John_Kasich > Chris_Christie &&
            John_Kasich > Jeb_Bush &&
            John_Kasich > Mike_Huckabee &&
            John_Kasich > Rand_Paul &&
            John_Kasich > Rick_Santorum) {
    winner = "Kasich"
  } else if(Marco_Rubio > Donald_Trump && 
            Marco_Rubio > John_Kasich &&
            Marco_Rubio > Ted_Cruz &&
            Marco_Rubio > Ben_Carson &&
            Marco_Rubio > Carly_Fiorina &&
            Marco_Rubio > Chris_Christie &&
            Marco_Rubio > Jeb_Bush &&
            Marco_Rubio > Mike_Huckabee &&
            Marco_Rubio > Rand_Paul &&
            Marco_Rubio > Rick_Santorum) {
    winner = "Rubio"
  } else if(Ted_Cruz > Donald_Trump &&
            Ted_Cruz > John_Kasich &&
            Ted_Cruz > Marco_Rubio &&
            Ted_Cruz > Ben_Carson &&
            Ted_Cruz > Carly_Fiorina &&
            Ted_Cruz > Chris_Christie &&
            Ted_Cruz > Jeb_Bush &&
            Ted_Cruz > Mike_Huckabee &&
            Ted_Cruz > Rand_Paul &&
            Ted_Cruz > Rick_Santorum) {
    winner = "Cruz"
  } else if(Ben_Carson > Donald_Trump && 
            Ben_Carson > John_Kasich &&
            Ben_Carson > Ted_Cruz &&
            Ben_Carson > Marco_Rubio &&
            Ben_Carson > Carly_Fiorina &&
            Ben_Carson > Chris_Christie &&
            Ben_Carson > Jeb_Bush &&
            Ben_Carson > Mike_Huckabee &&
            Ben_Carson > Rand_Paul &&
            Ben_Carson > Rick_Santorum) {
    winner = "Carson"
  } else if(Carly_Fiorina > Donald_Trump && 
            Carly_Fiorina > John_Kasich &&
            Carly_Fiorina > Ted_Cruz &&
            Carly_Fiorina > Marco_Rubio &&
            Carly_Fiorina > Ben_Carson &&
            Carly_Fiorina > Chris_Christie &&
            Carly_Fiorina > Jeb_Bush &&
            Carly_Fiorina > Mike_Huckabee &&
            Carly_Fiorina > Rand_Paul &&
            Carly_Fiorina > Rick_Santorum) {
    winner = "Carly"
  } else if(Chris_Christie > Donald_Trump && 
            Chris_Christie > John_Kasich &&
            Chris_Christie > Ted_Cruz &&
            Chris_Christie > Marco_Rubio &&
            Chris_Christie > Ben_Carson &&
            Chris_Christie > Carly_Fiorina &&
            Chris_Christie > Jeb_Bush &&
            Chris_Christie > Mike_Huckabee &&
            Chris_Christie > Rand_Paul &&
            Chris_Christie > Rick_Santorum) {
    winner = "Chris"
  } else if(Jeb_Bush > Donald_Trump && 
            Jeb_Bush > John_Kasich &&
            Jeb_Bush > Ted_Cruz &&
            Jeb_Bush > Marco_Rubio &&
            Jeb_Bush > Ben_Carson &&
            Jeb_Bush > Carly_Fiorina &&
            Jeb_Bush > Chris_Christie &&
            Jeb_Bush > Mike_Huckabee &&
            Jeb_Bush > Rand_Paul &&
            Jeb_Bush > Rick_Santorum) {
    winner = "Jeb"
  } else if(Mike_Huckabee > Donald_Trump && 
            Mike_Huckabee > John_Kasich &&
            Mike_Huckabee > Ted_Cruz &&
            Mike_Huckabee > Marco_Rubio &&
            Mike_Huckabee > Ben_Carson &&
            Mike_Huckabee > Carly_Fiorina &&
            Mike_Huckabee > Chris_Christie &&
            Mike_Huckabee > Jeb_Bush &&
            Mike_Huckabee > Rand_Paul &&
            Mike_Huckabee > Rick_Santorum) {
    winner = "Mike"
  } else if(Rand_Paul > Donald_Trump && 
            Rand_Paul > John_Kasich &&
            Rand_Paul > Ted_Cruz &&
            Rand_Paul > Marco_Rubio &&
            Rand_Paul > Ben_Carson &&
            Rand_Paul > Carly_Fiorina &&
            Rand_Paul > Chris_Christie &&
            Rand_Paul > Jeb_Bush &&
            Rand_Paul > Mike_Huckabee &&
            Rand_Paul > Rick_Santorum) {
    winner = "Rand"
  } else {
    winner = "Rick"
  })
,
  if(winner == "Trump") {
    z = 0
  } else if(winner == "Kasich") {
    z = 1
  } else if(winner == "Rubio") {
    z = 2
  } else if(winner == "Cruz") {
    z = 3
  } else {
    z = 4
  })
colnames(rep_by_state) <- c('state', 'Donald_Trump', 'abb', 'county',
                            'John_Kasich', 'Marco_Rubio', 'Ted_Cruz', 'Ben_Carson',
                            'winner', 'z')

bernie_by_state <- ByState(dem_by_county, "Bernie Sanders")
hillary_by_state <- ByState(dem_by_county, "Hillary Clinton")
dem_by_state <- left_join(bernie_by_state, hillary_by_state, by=c("state","abb","county")) %>%
  mutate(winner= ifelse(Bernie_Sanders > Hillary_Clinton, "Bernie", "Hillary"),
         z = ifelse(winner == "Bernie", 1, 0))