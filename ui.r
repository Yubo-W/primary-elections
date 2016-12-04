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
                                h2("Select the counties within the range you are interested for."),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                # Input to select variable to map
                                sliderInput("slider1", label=h3("African American Percentage"), min = 0, max = 100, value = c(0, 100)),
                                sliderInput("slider2", label=h3("Bachelors Degree Percentage"), min = 0, max = 100,  value = c(0, 100)),
                                sliderInput("slider3", label=h3("Household Income"), min = 0, max = 100000,  value = c(0, 100000))
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