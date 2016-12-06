library(dplyr)
library(plotly)

#summary data
wyn_rep_by_state <- rep_by_state %>% 
  mutate(total = Donald_Trump + John_Kasich + Marco_Rubio + Ted_Cruz + Ben_Carson)

#each candidate percent
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
    mutate(Ben_Carson_percent = round((Ben_Carson / total)*10000)/100)
}

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

df <- data.frame(y, x1, x2, x3, x4, x5)


#horizontal bar chart
plot_ly(df, x = ~x1, y = ~y, type = 'bar', orientation = 'h',
              marker = list(color = '#red',
                            line = list(color = 'rgb(248, 248, 249)', width = 1)),
              name = 'Donald Trump') %>%
  add_trace(x = ~x2, marker = list(color = '#orange'), name = 'John Kasich') %>%
  add_trace(x = ~x3, marker = list(color = '#green'), name = 'Marco Rubio') %>%
  add_trace(x = ~x4, marker = list(color = '#blue'), name = 'Ted Cruz') %>%
  add_trace(x = ~x5, marker = list(color = '#purple'), name = 'Ben Carson') %>%
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
                  showarrow = FALSE)