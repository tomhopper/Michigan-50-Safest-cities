
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("spacelab"),
                  
                  # Application title
                  titlePanel("Michigan Cities Crime Data"),
                  
                  # Sidebar with a slider input for number of bins
                  tabsetPanel(
                    tabPanel("Graph", 
                             sidebarLayout(
                               sidebarPanel(
                                 selectInput("crime_var",
                                             "Crime statistic to graph",
                                             choices = c("Violent Crime Rate", "Property Crime Rate"),
                                             selected = "Violent Crime Rate"),
                                 selectInput("best_worst",
                                             "Plot best or worst?",
                                             choices = c("Best","Worst"),
                                             selected = "Best"),
                                 sliderInput("top_n",
                                             "Number of cities to plot:",
                                             min = 5,
                                             max = 50,
                                             value = 10,
                                             step = 1),
                                 numericInput("min_pop_graph",
                                              "Population cutoff:",
                                              min = 0,
                                              max = NA,
                                              value = 10000,
                                              step = 1)
                               ),
                               mainPanel(
                                 uiOutput("citiesPlotUI"),
                                 htmlOutput("noteGraph")
                               )
                             )
                    ),
                    tabPanel("Table",
                             verticalLayout(
                               numericInput("min_pop",
                                            "Population cutoff:",
                                            min = 0,
                                            max = NA,
                                            value = 10000,
                                            step = 1),
                               
                               # Show a plot of the generated distribution
                               
                               dataTableOutput("citiesTable")
                               #tableOutput("citiesTable")
                             )
                    ),
                    tabPanel("Relationships",
                             sidebarLayout(
                               sidebarPanel(
                                 numericInput("min_pop_rel",
                                              "Population cutoff:",
                                              min = 0,
                                              max = NA,
                                              value = 10000,
                                              step = 1),
                                 selectInput(inputId = "xy_var_x",
                                             label = "Variable to plot horizontally (x axis):",
                                             choices = c("City", "Population", "violent_crime_rate", "violent_rate_lower", "violent_rate_upper",
                                                         "property_crime_rate", "property_rate_lower", "property_rate_upper",
                                                         "murder_rate", "robbery_rate", "assault_rate", "burglary_rate", "auto_theft_rate", "arson_rate"),
                                             selected = "Population"),
                                 selectInput(inputId = "xy_var_y",
                                             label = "Variable to plot vertically (y axis):",
                                             choices = c("City", "Population", "violent_crime_rate", "violent_rate_lower", "violent_rate_upper",
                                                         "property_crime_rate", "property_rate_lower", "property_rate_upper",
                                                         "murder_rate", "robbery_rate", "assault_rate", "burglary_rate", "auto_theft_rate", "arson_rate"),
                                             selected = "violent_crime_rate")
                               ),
                               mainPanel(
                                 plotOutput("relPlot")
                               )
                             )
                    )
                  ),
                  htmlOutput("noteData")
))
