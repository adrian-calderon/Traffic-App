library(shiny)          # For building interactive web applications in R
library(ggplot2)        # For creating static data visualizations
library(data.table)     # For fast data manipulation and aggregation
library(plotly)         # For creating interactive graphs
library(dplyr)          # For efficient data manipulation using the pipe operator (%>%)
library(cowplot)        # For combining and aligning ggplot2 plots into complex layouts
library(shinythemes)    # For adding themes to Shiny applications
library(ggiraph)        # For creating interactive ggplot2 visualizations
library(tidyverse)      # A collection of packages for data science, including ggplot2, dplyr, tidyr, etc.
library(mapdata)        # For accessing additional map data for spatial visualizations
library(stringr)        # For string manipulation and regular expressions
library(RColorBrewer)   # For creating color palettes for visualizations
library(paletteer)      # For accessing a wide range of color palettes
library(viridis)        # For colorblind-friendly and perceptually uniform color scales
library(DT)             # For creating interactive, JavaScript-based data tables
library(shinydashboard) # For creating dashboards in Shiny applications

# Load your data
accidents <- fread("accident.csv") 
weather <- fread("weather.csv")
distract <- fread("distract.csv")
drimpair <- fread("drimpair.csv")
drugs <- fread("drugs.csv")
person <- fread("person.csv")
safetyeq <- fread("safetyeq.csv")

accident1 <- read_csv("accident1.csv")
accident2 <- read_csv("accident2.csv")

state_choices <- setNames(unique(accident2$STATENAME),
                          str_to_title(unique(accident2$STATENAME)))

fill_choice <- setNames(c("fatalcase", "prop", "count"),
                        c("Fatalities Per Case", "% of Fatalities", "Total Cases"))

fill_choice2 <- setNames(c("fatalcase", "prop", "count"),
                         c("Fatalities", "Fatalities (%)", "Cases"))
drimpair <- drimpair %>%
  mutate(DRIMPAIRNAME = ifelse(DRIMPAIRNAME %in% c("Not Reported", "Reported as Unknown if Impaired", "No Driver Present/Unknown if Driver Present"), "Unknown/Not Reported", DRIMPAIRNAME))

accidents <- accidents %>%
  mutate(WEATHERNAME = ifelse(WEATHERNAME %in% c("Not Reported", "Reported as Unknown", "Other"), "Unknown/Other", WEATHERNAME))



# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("slate"),
  tags$head(
    tags$style(HTML("
      .dataTable th { color: #fff; background-color: #333; }
      .dataTable th, .dataTable td { padding: 8px; text-align: center; }
      .dataTable .odd { background-color: #f0f0f0; color: #333333; }
      .dataTable .even { background-color: #ffffff; color: #333333; }
      .dataTables_wrapper .dataTables_paginate .paginate_button {
        color: #333333 !important; background-color: #ffffff;
        border-color: #dddddd;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
        background-color: #eeeeee; border-color: #dddddd;
      }
      .dataTables_wrapper { background-color: #ffffff; color: #333333; }
      table.dataTable thead th { background-color: #f9f9f9; color: #333333; }
      table.dataTable thead th, table.dataTable tbody td {
        border-bottom: 1px solid #ddd;
      }
      .dataTables_wrapper .dataTables_length select,
      .dataTables_wrapper .dataTables_filter input {
        background-color: #fff; color: #333; border: 1px solid #aaa;
        border-radius: 4px; padding: 5px;
      }
    "))
  ),
  titlePanel("Traffic Incident Analysis"),
  navbarPage("Navigation",
             tabPanel("Summary Table", DTOutput('summaryTable')),
             tabPanel("General Analysis", sidebarLayout(
               sidebarPanel(
                 selectInput("analysisType", "Choose type of analysis:", 
                             choices = c("Weather", "Impairments", "Drug Influence", "Safety Equipment", "Distractions", "Severity Analysis")),
                 uiOutput("topNControl"),
                 actionButton("update", "Update Analysis")
               ),
               mainPanel(plotlyOutput("plot", height = "600px"))
             )),
             tabPanel("Time and Weather Impact", sidebarLayout(
               sidebarPanel(actionButton("updateTimeWeather", "Update Graph")),
               mainPanel(plotlyOutput("timeWeatherPlot", height = "600px"))
             )),
             tabPanel("Demographic Analysis", sidebarLayout(
               sidebarPanel(actionButton("updateDemographic", "Update Graph")),
               mainPanel(plotlyOutput("demographicPlot", height = "600px"))
             )),
             tabPanel("Map", sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "Location1", label = "Location",
                             choices = c("United States", state_choices)),
                 radioButtons(inputId = "Fill", label = "Info", choices = fill_choice)
               ),
               mainPanel(
                 tags$div(
                   style = "border: 1px solid #D3D3D3; padding: 15px; border-radius: 5px; background-color: #FFFFFF;",
                   girafeOutput("location", width = "100%")
                 )
               )
             )),
             tabPanel("Data by State",
                      fluidRow(
                        column(4,
                               selectInput("stateSelection", "Choose a State:", choices = unique(accident2$STATENAME))
                               
                        ),
                        column(4,
                               valueBoxOutput("totalAccidents"),
                               valueBoxOutput("totalFatalities"),
                               valueBoxOutput("drugsInvolved")
                        )
                      )
             )
  )
  
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Map rendering logic
  output$location <- renderGirafe({
    
    if (input$Location1 == "United States"){
      
      us_map <- ggplot(data=accident1, 
                       mapping=aes(x=long,
                                   y=lat,
                                   group=group,
                                   data_id = STATENAME,
                                   tooltip = paste("State:", 
                                                   str_to_title(STATENAME),
                                                   "\n", 
                                                   names(fill_choice[which(fill_choice == input$Fill)]),
                                                   ":",
                                                   round(.data[[input$Fill]], 2)))) + 
        coord_fixed(1.3) + 
        geom_polygon_interactive(color="black", aes(fill = .data[[input$Fill]])) + 
        scale_fill_distiller_interactive(palette = "RdYlBu")+
        geom_polygon_interactive(data=accident1, fill = NA, color="white") + 
        geom_polygon_interactive(color="black", fill=NA) + 
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              panel.background = element_rect(fill = "#FFFFFF"),
              panel.grid = element_blank())+
        labs(title = paste("Traffic Accident", 
                           names(fill_choice[which(fill_choice == input$Fill)]),
                           "by State"),
             fill = names(fill_choice2[which(fill_choice2 == input$Fill)]))
      
      
      girafe(ggobj = us_map, 
             options = list(opts_hover(css = "fill:green;stroke:black")))
      
    } else {
      
      full_counties <- accident2 %>%
        filter(STATENAME == input$Location1)
      
      county_map <- ggplot(data=full_counties, 
                           mapping=aes(x=long,
                                       y=lat,
                                       group=group,
                                       data_id = COUNTYNAME,
                                       tooltip = paste("County:", 
                                                       str_to_title(COUNTYNAME),
                                                       "\n", 
                                                       names(fill_choice[which(fill_choice == input$Fill)]),
                                                       ":",
                                                       round(.data[[input$Fill]], 2)))) + 
        coord_fixed(1.3) + 
        geom_polygon_interactive(color="black", aes(fill = .data[[input$Fill]])) +
        scale_fill_distiller_interactive(palette = "RdYlBu")+
        geom_polygon_interactive(data=full_counties, fill = NA, color="white") + 
        geom_polygon_interactive(color="black", fill=NA) + 
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              panel.background = element_rect(fill = "#FFFFFF"),
              panel.grid = element_blank())+
        labs(title = paste(str_to_title(input$Location1),
                           "Traffic Accident", 
                           names(fill_choice[which(fill_choice == input$Fill)])),
             fill = names(fill_choice2[which(fill_choice2 == input$Fill)]))
      
      girafe(ggobj = county_map,
             options = list(opts_hover(css = "fill:green;stroke:black")))
      
      
    }
    
  })
  
  output$topNControl <- renderUI({
    if (input$analysisType %in% c("Weather", "Impairments", "Drug Influence", "Safety Equipment", "Distractions", "Severity Analysis")) {
      sliderInput("topN", "Number of Top Instances:", min = 1, max = 50, value = 5)
    }
  })
  
  analysis_data <- reactive({
    req(input$update)  # Ensure that the update button is pressed
    topN <- input$topN  # Get the number of top instances to display
    
    # Determine which dataset to use
    data <- switch(input$analysisType,
                   "Weather" = weather,
                   "Impairments" = drimpair,
                   "Drug Influence" = drugs,
                   "Safety Equipment" = safetyeq,
                   "Distractions" = distract,
                   "Severity Analysis" = person %>% filter(SEX %in% c(1, 2)),
                   NULL)
    
    if (!is.null(data)) {
      # Create a column name for grouping based on the analysis type
      group_var <- switch(input$analysisType,
                          "Weather" = "WEATHERNAME",
                          "Impairments" = "DRIMPAIRNAME",
                          "Drug Influence" = "DRUGRESNAME",
                          "Safety Equipment" = "NMHELMETNAME",
                          "Distractions" = "DRDISTRACTNAME",
                          "Severity Analysis" = "INJ_SEV")
      
      # Perform counting and filter top N instances
      data <- data %>%
        count(!!sym(group_var)) %>%
        top_n(topN, n) %>%
        arrange(desc(n))
      
      # Create the plot
      ggplot(data, aes_string(x = group_var, y = "n")) +
        geom_col() +
        labs(title = paste("Top", topN, "Instances in", input$analysisType), x = NULL, y = "Count") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      ggplot() + labs(title = "No data available")
    }
  })
  
  # Plot output
  output$plot <- renderPlotly({
    p <- analysis_data()
    if (!is.null(p)) {
      ggplotly(p) %>% layout(autosize = TRUE)
    } else {
      plot_ly() %>% add_annotations(text = "Data not available for the selected analysis", x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 20))
    }
  })
  output$timeWeatherPlot <- renderPlotly({
    req(input$updateTimeWeather)
    # Ensure the histogram is correctly created
    valid_accidents <- accidents %>% filter(HOUR != 99)
    p <- ggplot(valid_accidents, aes(x = as.factor(HOUR), fill = WEATHERNAME)) +  # Added fill aesthetic
      geom_histogram(stat = "count", bins = 24, position = "dodge") +  # Use 'position = "dodge"' if you want separate bars for each weather condition
      facet_wrap(~WEATHERNAME, scales = "free_y") +
      labs(title = "Accidents by Time of Day and Weather", x = "Hour of Day", y = "Count") +
      theme_minimal()+
      guides(fill = FALSE)
    
    # Convert to Plotly after creating the ggplot object
    ggplotly(p) %>% layout(autosize = TRUE)
  })
  
  
  output$demographicPlot <- renderPlotly({
    req(input$updateDemographic)
    valid_person <- person %>% filter(SEX %in% c(1, 2))
    p <- ggplot(valid_person, aes(x = factor(SEX), fill = factor(SEX))) +
      geom_bar(stat = "count") +  # Ensure this is a count plot
      labs(title = "Gender Distribution in Accidents", x = "Gender", y = "Count") +
      scale_fill_manual(values = c("1" = "blue", "2" = "pink"), labels = c("1" = "Male", "2" = "Female")) +
      scale_x_discrete(labels = c("1" = "Male", "2" = "Female"))+
      guides(fill = FALSE)
    
    # Convert to Plotly after creating the ggplot object
    ggplotly(p) %>% layout(autosize = TRUE)
  })
  
  output$summaryTable <- renderDT({
    # Define the datatable with explicit column names
    datatable(accidents[, .(ST_CASE, HOUR, DAY_WEEKNAME, MAN_COLLNAME, FATALS)],
              options = list(pageLength = 5, autoWidth = TRUE),
              colnames = c("Case Number", "Hour", "Day of Week", "Collision Type", "Fatalities"),  # Set custom column names
              filter = 'top',  # Adds filters on top of each column
              rownames = FALSE)  # Hide row names for a cleaner look
  })
  
  filtered_data1 <- reactive({
    req(input$stateSelection)
    data <- accident2 %>% filter(STATENAME == input$stateSelection)
    print(paste("Filtering data for state:", input$stateSelection))
    print(paste("Rows found:", nrow(data)))
    return(data)
  })
  
  filtered_data2 <- reactive({
    req(input$stateSelection)
    # Convert the state names to uppercase and filter DRUGSPEC <= 1
    data <- drugs %>% 
      mutate(STATENAME = toupper(STATENAME)) %>% 
      filter(STATENAME == toupper(input$stateSelection), DRUGSPEC <= 1)
    print(paste("Filtered drug data rows for state:", input$stateSelection, "with DRUGSPEC <= 1:", nrow(data)))
    return(data)
  })
  
  
  
  output$totalAccidents <- renderValueBox({
    data <- filtered_data1()
    valueBox(
      value = formatC(nrow(data), format="d", big.mark=','),
      subtitle = "Total Accidents",
      icon = icon("car-crash"),
      color = "blue"  # Use basic color names known to work
    )
  })
  
  output$totalFatalities <- renderValueBox({
    data <- filtered_data1()
    valueBox(
      value = formatC(sum(data$fatalcase, na.rm = TRUE), format="d", big.mark=','),
      subtitle = "Total Fatalities",
      icon = icon("heartbeat"),
      color = "red"
    )
  })
  
  output$drugsInvolved <- renderValueBox({
    data <- filtered_data2()
    valueBox(
      value = sum(data$DRUGSPEC, na.rm = "1"),
      subtitle = "Drugs Involved",
      icon = icon("cannabis"),
      color = "green"
    )
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)