
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rvest)
library(readxl)
library(PropCIs)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggvis)

shinyServer(function(input, output) {
  
  getCitiesData <- function(scrape_page = FALSE) {
    if(scrape_page) {
      # Data source, FBI UCR 2014
      page_uri <- "https://www.fbi.gov/about-us/cjis/ucr/crime-in-the-u.s/2014/crime-in-the-u.s.-2014/tables/table-8/table-8-by-state/Table_8_Offenses_Known_to_Law_Enforcement_by_Michigan_by_City_2014.xls"
      
      # Scrape the data into a data frame
      cities_df <- page_uri %>%
        read_html() %>%
        html_nodes("table")
      cities_df <- cities_df[[2]] %>% #'//*[(@id = "table-data-container")]',
      html_table()
      
      # Fix column names (spaces, dashes)
      colnames(cities_df) <- make.names(colnames(cities_df))
      
      # Remove commas from numbers and convert to numeric
      for (i in c(2,3,7:12))
        cities_df[[i]] <- as.numeric(gsub(',', '', cities_df[[i]]))
      
    } else {
      # Data source, FBI UCR 2014, downloaded 2016-03-25 20:00
      cities_df <- read_excel("data-raw/Table_8_Offenses_Known_to_Law_Enforcement_by_Michigan_by_City_2014.xls", skip = 4, sheet = 1)
      
      # Fix column names (spaces, dashes)
      colnames(cities_df) <- make.names(colnames(cities_df))
      
      # Strip the notes from the last three lines
      cities_df <- cities_df[1:(NROW(cities_df) - 3),]
    }
    
    # Add in GR reported motor theft, which the FBI determined was under-reported in 2014
    cities_df["Grand Rapids3" == cities_df$City,]$Motor.vehicle.theft <- 327 # \uri{http://www.grand-rapids.mi.us/police-department/Documents/Crime%20In%20Grand%20Rapids%202014.pdf}
    cities_df["Grand Rapids3" == cities_df$City,]$Property.crime <- with(cities_df["Grand Rapids3" == cities_df$City,], Motor.vehicle.theft + Larceny..theft + Burglary)
    
    # Calculate crime rates per 100,000 residents
    cities_df <- cities_df %>% 
      mutate(City = as.factor(City),
             violent_crime_rate = round(Violent.crime / Population * 100000, 0),
             property_crime_rate = round(Property.crime / Population * 100000, 0))
    
    # Calculate confidence intervals on the crime rates. May be used later for error bars
    cities_df$violent_rate_upper = apply(cities_df, 1, function(x) round(exactci(as.numeric(x[3]), as.numeric(x[2]), conf.level = 0.95)$conf.int[2] * 100000, 0))
    cities_df$violent_rate_lower = apply(cities_df, 1, function(x) round(exactci(as.numeric(x[3]), as.numeric(x[2]), conf.level = 0.95)$conf.int[1] * 100000, 0))
    cities_df$property_rate_upper = apply(cities_df, 1, function(x) round(exactci(as.numeric(x[9]), as.numeric(x[2]), conf.level = 0.95)$conf.int[2] * 100000, 0))
    cities_df$property_rate_lower = apply(cities_df, 1, function(x) round(exactci(as.numeric(x[9]), as.numeric(x[2]), conf.level = 0.95)$conf.int[1] * 100000, 0))
    
    # Select only the columns that we're interested in
    cities_df <- cities_df %>% 
      select(City, Population, Violent.crime, violent_crime_rate, violent_rate_lower, violent_rate_upper, Property.crime, property_crime_rate, property_rate_lower, property_rate_upper, murder = Murder.and.nonnegligent.manslaughter, rape = Rape..revised.definition.1, robbery = Robbery, aggravated_assault = Aggravated.assault, burglary = Burglary, theft = Larceny..theft, auto_theft = Motor.vehicle.theft, arson = Arson) %>% 
      mutate(murder_rate = round(murder / Population * 100000, 0),
             robbery_rate = round(robbery / Population * 100000, 0),
             assault_rate = round(aggravated_assault / Population * 100000, 0),
             burglary_rate = round(burglary / Population * 100000, 0),
             auto_theft_rate = round(auto_theft / Population * 100000, 0),
             arson_rate = round(arson / Population * 100000, 0)) %>% 
      select(City, Population, violent_crime_rate, violent_rate_lower, violent_rate_upper,
             property_crime_rate, property_rate_lower, property_rate_upper,
             murder_rate, robbery_rate, assault_rate, burglary_rate, auto_theft_rate, arson_rate)
    
    return(cities_df)
  }
  
  # Get the data
  cities_df <- getCitiesData()
  
  # Filter the data frame based on population selected in min_pop
  # cities_display <- reactive({
  #   cities_df %>% 
  #     filter(Population >= as.numeric(input$min_pop)) %>% 
  #     mutate(rank = rank(violent_crime_rate, ties.method = "first"),
  #            rank_upper = rank(violent_upper, ties.method = "first"))
  # })
  
  # create a DataTable output
  output$citiesTable <- renderDataTable({
    cities_df %>%
      filter(Population >= as.numeric(input$min_pop)) %>%
      mutate(crime_rank = rank(violent_crime_rate, ties.method = "first"),
             rank_worst_case = rank(violent_rate_upper, ties.method = "first")) %>%
      #select(City, Population, crime_rank, violent_crime_rate, property_crime_rate, violent_crimes = Violent.crime, property_crimes = Property.crime) %>% 
      arrange(crime_rank)
  })
  
  # create the plot
  output$citiesPlot <- renderPlot({
    if(input$crime_var == "Violent Crime Rate") {
      # User wants to plot violent crime rate
      
      # Set up data frame filtered by population
      cities_plot_df <- cities_df %>% 
        filter(Population >= input$min_pop_graph) %>% 
        mutate(crime_rank = rank(violent_crime_rate, ties.method = "first")) %>% 
        arrange(crime_rank)
      if(input$best_worst == "Best") {
        # User wants to plot the best n
        # Filter plotting data frame for selected "top" n.
        cities_plot_df <- cities_plot_df %>% 
          filter(min_rank(crime_rank) <= as.numeric(input$top_n))
        sort_order = TRUE # Sort the graph from low to high (decreasing = TRUE)
      } else {
        # User wants to plot the worst n
        # Filter plotting data frame for selecte "bottom" n.
        cities_plot_df <- cities_plot_df %>% 
          filter(min_rank(desc(violent_crime_rate)) <= as.numeric(input$top_n))
        sort_order <- FALSE # Sort the graph from high to low (decreasing = FALSE)
      }
      # Sort the City names so that the top of the graph will hold the best or worst, depending
      # on user selection.
      levels_order <- cities_plot_df[order(cities_plot_df$crime_rank, decreasing = sort_order),]$City
      cities_plot_df$City <- factor(cities_plot_df$City, levels = levels_order)
      
      # Build up the plot
      cities_plot <- cities_plot_df %>% 
        ggplot(aes(x = City)) +
        geom_segment(aes(y = violent_rate_lower, yend = violent_rate_upper, xend = City), colour = "grey50", alpha = 0.5) +
        geom_point(aes(y = violent_crime_rate)) +
        ylab("Violent crime rate, per 100,000") +
        coord_flip() +
        theme_bw() +
        theme(axis.title.y = element_blank(), 
              axis.title.x = element_text(size = rel(4)),
              text = element_text(size = rel(5)))
    }
    else {
      # User wants to plot property crime data
      # Repeat all steps for violent crime, but using property crime variables
      if(input$crime_var == "Property Crime Rate") {
        cities_plot_df <- cities_df %>% 
          filter(Population >= input$min_pop_graph) %>% 
          mutate(crime_rank = rank(property_crime_rate, ties.method = "first")) %>% 
          arrange(crime_rank)
        if(input$best_worst == "Best") {
          cities_plot_df <- cities_plot_df %>% 
            filter(min_rank(crime_rank) <= as.numeric(input$top_n))
          sort_order = TRUE
        } else {
          cities_plot_df <- cities_plot_df %>% 
            filter(min_rank(desc(property_crime_rate)) <= as.numeric(input$top_n))
          sort_order <- FALSE
        }
        levels_order <- cities_plot_df[order(cities_plot_df$crime_rank, decreasing = sort_order),]$City
        cities_plot_df$City <- factor(cities_plot_df$City, levels = levels_order)
        cities_plot <- cities_plot_df %>% ggplot(aes(x = City)) +
          geom_segment(aes(y = property_rate_lower, yend = property_rate_upper, xend = City), colour = "grey50", alpha = 0.5) +
          geom_point(aes(y = property_crime_rate)) +
          ylab("Property crime rate, per 100,000") +
          coord_flip() +
          theme_bw() +
          theme(axis.title.y = element_blank(), 
                axis.title.x = element_text(size = rel(4)),
                text = element_text(size = rel(5)))
      }
    }
    # return the plot
    cities_plot
  })
  
  # Set up a reactive variable to dynamically calculate the height of the plot depending
  # on the user-selected value of n (number of cities to show)
  values <- reactiveValues()
  plot_height <- function() {
    values$facetCount <- as.numeric(input$top_n) * 30 # 30 seems to be a nice balance of compact graph and readable spacing
    return(values$facetCount)
  }
  
  # Set output variable to the plot with variable height.
  # Narrow the plot a little to give some extra whitespace to the edge of the tab pane
  output$citiesPlotUI <- renderUI({
    plotOutput(outputId = "citiesPlot", height = plot_height(), width = "90%")
  })
  
  tooltip_label <- reactive({
    function(x) {
      if(is.null(x)) return(NULL)
      else return(paste0(x$city, ": </br>x:", x$x, "</br>y: ", x$y))
    }
  })
  
  rel_plot_df <- reactive({
    df <- cities_df %>% 
      filter(Population >= input$min_pop_rel) %>% 
      mutate_(x = input$xy_var_x, y = input$xy_var_y)
  })
  
  output$relPlot <- renderPlot({
    rel_p <- rel_plot_df() %>% 
      ggplot(aes(x = x, y = y)) +
      #geom_smooth(method = "lm") +
      geom_point() +
      ylab(input$xy_var_y) +
      xlab(input$xy_var_x) +
      theme_bw() +
      theme(text = element_text(size = rel(5)),
            axis.title.x = element_text(size = rel(4)),
            axis.title.y = element_text(size = rel(4)))
    if(is.numeric(rel_plot_df()$x) & is.numeric(rel_plot_df()$y)) {
      rel_p <- rel_p + geom_smooth(method = "lm")
    }
    return(rel_p)
  })
  # cities_plot_df <- reactive({
  #   df <- cities_df %>% 
  #     filter(Population >= input$min_pop_rel) %>% 
  #     mutate_(x = input$xy_var_x, y = input$xy_var_y)
  # }) 
  # ggvis(cities_plot_df, ~x, ~y) %>% 
  #   layer_points() %>% 
  #   add_tooltip(html = tooltip_label()) %>% 
  #   bind_shiny("relPlot")
  # output$relPlot_ui <- renderUI({
  #   ggvisOutput("relPlot")
  # })
  
  output$correlation_text <- renderText({
    if(is.numeric(rel_plot_df()$x) && is.numeric(rel_plot_df()$y)){
      #rel_cor <- cor(rel_plot_df()$x, rel_plot_df()$y)
      rel_lm <- lm(data = rel_plot_df(), y ~ x)
      rel_cor <- coef(rel_lm)[[2]]
      rel_rsq <- summary(rel_lm)$r.squared
      cor_text <- ifelse(!is.null(rel_cor), paste0("</br>", input$xy_var_x ," explains at most ", round(rel_rsq * 100, 0), "% of the variation in ", input$xy_var_y,".", ""))
      HTML(paste0("For every increase of 1 in ", input$xy_var_x,", ", input$xy_var_y," increases by about ", format(rel_cor, digits = 2), ".", ifelse(!is.null(cor_text),cor_text," ")))
    } else
      HTML("")
  })
  
  output$relVarXui <- renderUI({
    selectInput(inputId = "xy_var_x",
                label = "Variable to plot horizontally (x axis):",
                choices = colnames(cities_df),
                selected = colnames(cities_df[2]))
  })
  
  output$relVarYui <- renderUI({
    selectInput(inputId = "xy_var_y",
                label = "Variable to plot vertically (y axis):",
                choices = colnames(cities_df),
                selected = colnames(cities_df[3]))
    
  })
  
  # Set up explanatory notes
  output$noteGraph <- renderText({
    HTML("<div style = \"font-size: smaller\">Light gray bars indicate the range within which the \"true\" crime rate falls.<br/>Where dots of one city overlap with bars of another city, the crime rates of the two cities are not meaningfully different.</div>")
  })
  output$noteData <- renderText({
    HTML("<div style = \"font-size: smaller\"></br></br>Data from FBI Uniform Crime Report <em>2014 Crime in the United States.</em></div>")
  })
})
