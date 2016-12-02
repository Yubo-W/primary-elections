library(plotly)


shinyUI(fluidPage(
  mainPanel(
    br(),
    br(),
    br(),
    h2("Look at just the counties with a certain minimum percentage of the values you choose."),
    column(4, sliderInput("slider1", label=h3("African American Minimum Percentage"), min = 0, max = 50, value = 0)
    ),
    column(4, sliderInput("slider2", label=h3("Bachelors Degree Minimum Percentage"), min = 0, max = 60, value = 0)
    ),
    column(4, sliderInput("slider3", label=h3("Minimum Household Income"), min = 0, max = 100000, value = 0)
    ),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    plotlyOutput('plot2'),
    br(),
    br(),
    br(),
    plotlyOutput('plot1')
  )
))