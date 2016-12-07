library(plotly)


shinyUI(fluidPage(navbarPage('Primary Election',
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
                                        in 2016. There are, in total, data that is displayed from 46 states. The other 4 states are not included in the visualization for
                                        various reasons. The reasons are all documented in the GitHub repo's README. The link to the repo is provided above.
                                        Our data also comes reliable sources, such as CNN and the US Census, which is where the data we used originally came from. The data
                                        we used allowed us to draw bar graphs, pie charts, and maps to visualize the data. All the data drawn is done using Plotly, and
                                        visual is provided for the two largest parties involved United States Politics, the Democratic and Republican party."),
                                      p("Further documentation can be found on the GitHub repo. The link is provided above and it goes much more into detail of the aspects
                                        of this Shiny web application. Including the reasoning for why data is not displayed in the visualizations and the 
                                        user input functions."),
                                      br(),
                                      h4('Collaboration project by Soichi Tanabe, Yubo Wang, and Wynston Hsu')
                                      ),
                             
                             tabPanel('Democratic',
                                      titlePanel('Primary Election Democratic Party Statistics'),
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
                                        
                                        mainPanel(
                                          br(),
                                          h2("Data Visualization"),
                                          br(),
                                          br(),
                                          br(),
                                          br(),
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
                                          plotlyOutput('plot6')
                                        )
                                      )
                             ),
                             
                             tabPanel('Republican',
                                      titlePanel('Primary Election Republican Party Statistics'),
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
                                      
                                      mainPanel(
                                        
                                      )
                                    )
                                  )
)))