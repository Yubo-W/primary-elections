# Library the required packages.
library(plotly)
library(shiny)

# Shiny UI
shinyUI(fluidPage(theme = "bootstrap.css",
                      navbarPage('Primary Election',
                             
                             # Tab for the introduction to this web application.
                             tabPanel('Introduction',
                                      titlePanel('Background Information on Data'),
                                      br(),
                                      p("This data is made by Ben Hamner."),
                                      p("Source: https://www.kaggle.com/benhamner/2016-us-election"),
                                      br(),
                                      p("Project is on GitHub."),
                                      p("Source: https://github.com/wynhsu/final-project-I-mWithHer"),
                                      br(),
                                      p("This data conatins files from two different CSV files. The first CSV file contains data on the results of the primary election
                                        in 2016. We display data for the Democratic and Republican party across 46 states. The other 4 states (Alaska, Kansas, Minnesota, & North Dakota) are not included in the visualization for
                                        various reasons. The reasons are all documented in the GitHub repo's README. The link to the repo is provided above.
                                        Our data originally comes from reliable sources such as CNN and the US Census. The data
                                        we used allowed us to draw bar graphs, pie charts, and maps to visualize the data. The data wrangling was done with R, the charts were creating using the Plotly library, and
                                        the interactivity was done with the Shiny framework."),
                                      p("Further documentation can be found on the GitHub repo. The link is provided above and it goes into much more detail."),
                                      br(),
                                      h4('Collaboration project by Soichi Tanabe, Yubo Wang, and Wynston Hsu')
                                      ),
                             
                             # Democratic party statistics tab.
                             tabPanel('Democratic',
                                      titlePanel('Democratic Party Statistics'),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      # Create sidebar layout
                                      sidebarLayout(
                                        
                                        # Side panel for controls
                                        sidebarPanel(
                                          h2("Select only the counties within the range you are interested for."),
                                          br(),
                                          br(),
                                          br(),
                                          br(),
                                          br(),
                                          # Input to select variable to map
                                          h3("Ethnicities"),
                                          sliderInput("race1", label=h4("African American Percentage"), min = 0, max = 100, value = c(0, 100)),
                                          sliderInput("race2", label=h4("Caucasian Percentage"), min = 0, max = 100, value = c(0, 100)),
                                          sliderInput("race3", label=h4("Asian Percentage"), min = 0, max = 100, value = c(0, 100)),
                                          sliderInput("race4", label=h4("Hispanic Percentage"), min = 0, max = 100, value = c(0, 100)),
                                          br(),
                                          br(),
                                          h3("Education"),
                                          sliderInput("education1", label=h4("High School Graduate Percentage"), min = 0, max = 100,  value = c(0, 100)),
                                          sliderInput("education2", label=h4("College Graduate Percentage"), min = 0, max = 100,  value = c(0, 100)),
                                          br(),
                                          br(),
                                          h3("Income"),
                                          sliderInput("income1", label=h4("Household Income"), min = 0, max = 100000,  value = c(0, 100000))
                                        ),
                                        
                                        # Tab for the display of plots.
                                        mainPanel(
                                          fluidRow(
                                            splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("plot1"), plotlyOutput("plot2"))
                                          ),
                                          br(),
                                          br(),
                                          fluidRow(
                                            splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("plot3"), plotlyOutput("plot4"))
                                          ),
                                          br(),
                                          br(),
                                          plotlyOutput('plot5'),
                                          br(),
                                          br(),
                                          fluidRow(
                                            column(12, align="center",
                                                   h4("States Won Summary Table"),
                                                   tableOutput('table')
                                            )
                                          ),
                                          br(),
                                          br(),
                                          plotlyOutput('plot6'),
                                          br(),
                                          br()
                                        )
                                      )
                             ),
                             
                             # Tab for Republican party statistics.
                             tabPanel('Republican',
                                      titlePanel('Republican Party Statistics'),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      # Create sidebar layout
                                      sidebarLayout(
                                        
                                        # Side panel for controls
                                        sidebarPanel(
                                          h2("Select only the counties within the range you are interested for."),
                                          br(),
                                          br(),
                                          br(),
                                          br(),
                                          br(),
                                          # Input to select variable to map
                                          h3("Ethnicities"),
                                          sliderInput("rep_race1", label=h4("African American Percentage"), min = 0, max = 100, value = c(0, 100)),
                                          sliderInput("rep_race2", label=h4("Caucasian Percentage"), min = 0, max = 100, value = c(0, 100)),
                                          sliderInput("rep_race3", label=h4("Asian Percentage"), min = 0, max = 100, value = c(0, 100)),
                                          sliderInput("rep_race4", label=h4("Hispanic Percentage"), min = 0, max = 100, value = c(0, 100)),
                                          br(),
                                          br(),
                                          h3("Education"),
                                          sliderInput("rep_education1", label=h4("High School Graduate Percentage"), min = 0, max = 100,  value = c(0, 100)),
                                          sliderInput("rep_education2", label=h4("College Graduate Percentage"), min = 0, max = 100,  value = c(0, 100)),
                                          br(),
                                          br(),
                                          h3("Income"),
                                          sliderInput("rep_income1", label=h4("Household Income"), min = 0, max = 100000,  value = c(0, 100000))
                                        ),
                                      
                                      # Tab for the display of plots.
                                      mainPanel(
                                        fluidRow(
                                          splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("rep_plot1"), plotlyOutput("rep_plot2"))
                                        ),
                                        br(),
                                        br(),
                                        fluidRow(
                                          splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("rep_plot3"), plotlyOutput("rep_plot4"))
                                        ),
                                        br(),
                                        br(),
                                        plotlyOutput('rep_plot5'),
                                        br(),
                                        br(),
                                        plotlyOutput('rep_plot6')
                                      )
                                    )
                                  )
)))