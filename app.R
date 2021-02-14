library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)
library(tidyverse)
library(zoo)

# Load dataset
nyc_art_programs <- read.csv("nyc_art_programs.csv")

# Mutate Year.Month (e.g 10.2018) for later visualization
nyc_art_programs <- nyc_art_programs %>%
  mutate(Year.Month = paste(Install.Year, Install.Month))

nyc_art_programs$Year.Month <- str_replace_all(nyc_art_programs$Year.Month, " ", "-")

nyc_art_programs$Year.Month <- as.yearmon(nyc_art_programs$Year.Month)

# Data source: https://catalog.data.gov/dataset/temporary-art-program

# Define UI for application that plots features of programs ---------
ui <- fluidPage(

  # Application title -----------------------------------------------
  titlePanel("Temporary Art Programs in New York City (2008-2020)"),
  
  # One functioning downloadButton()
  downloadButton("downloadData", "Download"),
  
  # Sidebar layout with a input and output definitions --------------
  sidebarLayout(
    
    # Inputs: Select variables to plot ------------------------------
    sidebarPanel(
      
      # Select variable for X-axis for bar charts -------------------
      selectInput(inputId = "x", 
                  label = "X-axis (Bar chart):",
                  choices = c("Program Initiative" = "Program.Initiative", 
                              "Site Type" = "Site.Type",
                              "Project Type" = "Project.Type"), 
                  selected = "Program Initiative"),
      
      # Select variable for X-axis for boxplots ---------------------
      selectInput(inputId = "x2", 
                  label = "X-axis (Boxplot):",
                  choices = c("Program Initiative" = "Program.Initiative", 
                              "Site Type" = "Site.Type",
                              "Project Type" = "Project.Type"), 
                  selected = "Program Initiative"),
      
      # Set color
      selectInput(inputId = "z", 
                  label = "Color by:",
                  choices = c("Program Initiative" = "Program.Initiative", 
                              "Site Type" = "Site.Type",
                              "Project Type" = "Project.Type"),
                  selected = "Program Initiative"),
      
      # Set alpha level ---------------------------------------------
      sliderInput(inputId = "alpha", 
                  label = "Alpha:", 
                  min = 0, max = 1, 
                  value = 0.5),
      
      # Horizontal line for visual separation -----------------------
      hr(),
      
      # Enter text for trend line plot title ------------------------
      textInput(inputId = "plot.title", 
                label = "Plot title (Trend line)", 
                placeholder = "Enter text to be used as plot title"),
       
      # Enter text for bar chart title ------------------------------
      textInput(inputId = "plot.title2", 
                label = "Plot title (Bar chart)", 
                placeholder = "Enter text to be used as plot title"),
      
      # Enter text for bar chart title ------------------------------
      textInput(inputId = "plot.title3", 
                label = "Plot title (Boxplot)", 
                placeholder = "Enter text to be used as plot title"),
 
      # Horizontal line for visual separation -----------------------
      hr(),
      
      fluidPage(
        column(6,
               
        # Select which types of project to plot ---------------------
               checkboxGroupInput(inputId = "selected.type",
                                  label = "Select Program Type(s):",
                                  choices = c("Intervention", "Mural", 
                                              "Sculpture"),
                                  selected = "Intervention"),
               
        # Select which years to plot --------------------------------
               checkboxGroupInput(inputId = "selected.year",
                                  label = "Select Program Year(s):",
                                  choices = c(2008:2020),
                                  selected = c(2008:2020))
        ),
        
        column(6,
        # Select sample size ------------------------------------------
               numericInput(inputId = "n.samp", 
                            label = "Sample size:", 
                            min = 1, max = nrow(nyc_art_programs), 
                            value = 20),
        
        # Show data table --------------------------------------------
               checkboxInput(inputId = "show.data",
                             label = "Show data table",
                             value = TRUE)
        )
      )
    ),
    
    # Output: --------------------------------------------------------
    mainPanel(
      
      # Guide users to select program years for plottig trend line ---
      uiOutput(outputId = "n.0"),
      br(), br(),    # a little bit of visual separation
      
      # Show trend line ----------------------------------------------
      plotOutput(outputId = "line"),
      br(),        # a little bit of visual separation
      
      # Guide users to sample observations for bar chart and boxplot -
      uiOutput(outputId = "n.1"),
      br(), br(),    # a little bit of visual separation
      
      # Show bar charts ----------------------------------------------
      plotOutput(outputId = "barchart"),
      br(),        # a little bit of visual separation
      
      # Show boxplots ------------------------------------------------
      plotOutput(outputId = "boxplot"),
      br(),        # a little bit of visual separation
      
      # Print number of obs for bar chart and boxplot  ---------------
      uiOutput(outputId = "n.2"),
      br(), br(),    # a little bit of visual separation
      
      # Show data table ----------------------------------------------
      DT::dataTableOutput(outputId = "programstable")
    )
  )
)


# Define server function required to create the scatterplot ----------
server <- function(input, output, session) {
  
  # Create a subset of data filtering for selected title types --------
  programs.subset <- reactive({
    req(input$selected.type) # ensure availablity of value before proceeding
    filter(nyc_art_programs, Project.Type %in% input$selected.type)
  })
  
  # Create a subset of data filtering for selected title types --------
  years.subset <- reactive({
    req(input$selected.year) # ensure availablity of value before proceeding
    filter(nyc_art_programs, Year %in% input$selected.year)
  })
  
  # Update the maximum allowed n.samp for selected type programs ------
  observe({
    updateNumericInput(session, 
                       inputId = "n.samp",
                       value = min(20, nrow(programs.subset())),
                       max = nrow(programs.subset())
    )
  })
  
  # Create new df that is n.samp obs from selected type programs ------
  programs.sample <- reactive({ 
    req(input$n.samp) # ensure availablity of value before proceeding
    sample_n(programs.subset(), input$n.samp)
  })
  
  # Convert plot.title toTitleCase for trend line --------------------
  pretty.plot.title <- reactive({ toTitleCase(input$plot.title) })
  
  # Convert plot.title2 toTitleCase for bar charts -------------------
  pretty.plot.title2 <- reactive({ toTitleCase(input$plot.title2) })
  
  # Convert plot.title3 toTitleCase for boxplot ----------------------
  pretty.plot.title3 <- reactive({ toTitleCase(input$plot.title3) })
  
  # Guide users to select years for the trend line -----------
  output$n.0 <- renderUI({
    
    HTML("Select program years for plotting the number of programs
          in each month bewteen 2008 and 2020. <br>")
  })

  # Create line object the plotOutput function is expecting --
  output$line <- renderPlot({
    ggplot(data = years.subset(), aes_string(x = "Year.Month")) +
      geom_line(stat = 'count', alpha = input$alpha) +
      labs(x = toTitleCase("Month and Year"),
           title = pretty.plot.title()
      )
  })
  
  # Guide users to sample observations for bar chart and boxplot -----
  output$n.1 <- renderUI({
    
    HTML("Type in the number of programs to be sampled for
         plotting the bar chart/boxplot displayed below. <br>
         
        Select types of programs that will be sampled.")
  })
  
  # Create bar chart object the plotOutput function is expecting -----
  output$barchart <- renderPlot({
    ggplot(data = programs.sample(), aes_string(x = input$x,
                                                fill = input$z)) +
      geom_bar(aes(y = (..count..)), alpha = input$alpha) +
      labs(x = toTitleCase(str_replace_all(input$x, "\\.", " ")),
           y = toTitleCase("Count"),
           title = pretty.plot.title2()
      )
  })
  
  # Create boxplot object the plotOutput function is expecting -------
  output$boxplot <- renderPlot({
    ggplot(data = programs.sample(), 
           aes_string(x = input$x2, y = "Length.Days")) +
      geom_boxplot(alpha = input$alpha) +
      labs(x = toTitleCase(str_replace_all(input$x, "\\.", " ")),
           y = toTitleCase("Program Length (Days)"),
           title = pretty.plot.title3()
      )
  })

  # Print number of programs plotted in bar chart and boxplot -------
  output$n.2 <- renderUI({
    types <- programs.sample()$Project.Type %>% 
      factor(levels = input$selected.type) 
    counts <- table(types)
    
    HTML(paste("There are", counts, input$selected.type, 
               "temporary NYC art programs in this sample. <br>"))
  })
  
  # Print data table if checked -------------------------------------
  output$programstable <- DT::renderDataTable(
    if(input$show.data){
      DT::datatable(data = programs.sample()[, 1:7], 
                    extensions = 'Scroller', 
                    options = list(pageLength = 10,
                                   scrollX = TRUE),
                    rownames = FALSE)
    })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("nyc_art_programs-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(nyc_art_programs, file)
    }
  )
}

# Run the application -----------------------------------------------
shinyApp(ui = ui, server = server)
