library(plotly)


shinyUI(navbarPage('Primary Election',
                   tabPanel('Democratic',
                            titlePanel('Electoral College Votes'),
                            br(),
                            br(),
                            # Create sidebar layout
                            sidebarLayout(
                              
                              # Side panel for controls
                              sidebarPanel(
                                h2("Look at just the counties with a certain minimum percentage of the values you choose."),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                # Input to select variable to map
                                sliderInput("slider1", label=h3("African American Minimum Percentage"), min = 0, max = 50, value = 0),
                                sliderInput("slider2", label=h3("Bachelors Degree Minimum Percentage"), min = 0, max = 60, value = 0),
                                sliderInput("slider3", label=h3("Minimum Household Income"), min = 0, max = 100000, value = 0)
                              ),
                              
  mainPanel(
    br(),
    br(),
    plotlyOutput('plot1'),
    br(),
    br(),
    br(),
    plotlyOutput('plot2'),
    br(),
    br(),
    br(),
    plotlyOutput('plot3'),
    br(),
    br(),
    br(),
    plotlyOutput('plot4')
  )
))
))