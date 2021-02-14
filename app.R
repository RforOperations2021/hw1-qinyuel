library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)
library(tidyverse)
load("programs_2.Rdata")

##Data source: https://catalog.data.gov/dataset/temporary-art-program

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
                  choices = c("Program Initiative" = "Program_Initiative", 
                              "Site Type" = "Site_Type",
                              "Project Type" = "Project_Type"), 
                  selected = "Program Initiative"),
      
      # Select variable for X-axis for boxplots ---------------------
      selectInput(inputId = "x2", 
                  label = "X-axis (Boxplot):",
                  choices = c("Program Initiative" = "Program_Initiative", 
                              "Site Type" = "Site_Type",
                              "Project Type" = "Project_Type"), 
                  selected = "Program Initiative"),
      
      # Set color
      selectInput(inputId = "z", 
                  label = "Color by:",
                  choices = c("Program Initiative" = "Program_Initiative", 
                              "Site Type" = "Site_Type",
                              "Project Type" = "Project_Type"),
                  selected = "Program Initiative"),
      
      # Set alpha level ---------------------------------------------
      sliderInput(inputId = "alpha", 
                  label = "Alpha:", 
                  min = 0, max = 1, 
                  value = 0.5),
      
      # Horizontal line for visual separation -----------------------
      hr(),
      
      # Enter text for trend line plot title ------------------------
      textInput(inputId = "plot_title", 
                label = "Plot title (Trend line)", 
                placeholder = "Enter text to be used as plot title"),
       
      # Enter text for bar chart title ------------------------------
      textInput(inputId = "plot_title2", 
                label = "Plot title (Bar chart)", 
                placeholder = "Enter text to be used as plot title"),
      
      # Enter text for bar chart title ------------------------------
      textInput(inputId = "plot_title3", 
                label = "Plot title (Boxplot)", 
                placeholder = "Enter text to be used as plot title"),
 
      # Horizontal line for visual separation -----------------------
      hr(),
      
      fluidPage(
        column(6,
               
        # Select which types of project to plot ---------------------
               checkboxGroupInput(inputId = "selected_type",
                                  label = "Select Project Type(s):",
                                  choices = c("Intervention", "Mural", 
                                              "Sculpture"),
                                  selected = "Intervention"),
               
        # Select which years to plot --------------------------------
               checkboxGroupInput(inputId = "selected_year",
                                  label = "Select Project Year(s):",
                                  choices = c(2008:2020),
                                  selected = c(2008:2020))
        ),
        
        column(6,
        # Select sample size ------------------------------------------
               numericInput(inputId = "n_samp", 
                            label = "Sample size:", 
                            min = 1, max = nrow(programs_2), 
                            value = 20),
        
        # Show data table --------------------------------------------
               checkboxInput(inputId = "show_data",
                             label = "Show data table",
                             value = TRUE)
        )
      )
    ),
    
    # Output: --------------------------------------------------------
    mainPanel(
      
      # Show trend line ----------------------------------------------
      plotOutput(outputId = "line"),
      br(),        # a little bit of visual separation
      
      # Show bar charts ----------------------------------------------
      plotOutput(outputId = "barchart"),
      br(),        # a little bit of visual separation
      
      # Show boxplots ------------------------------------------------
      plotOutput(outputId = "boxplot"),
      br(),        # a little bit of visual separation
      
      # Print number of obs plotted ----------------------------------
      uiOutput(outputId = "n"),
      br(), br(),    # a little bit of visual separation
      
      # Show data table ----------------------------------------------
      DT::dataTableOutput(outputId = "programstable")
    )
  )
)


# Define server function required to create the scatterplot ----------
server <- function(input, output, session) {
  
  # Create a subset of data filtering for selected title types --------
  programs_subset <- reactive({
    req(input$selected_type) # ensure availablity of value before proceeding
    filter(programs_2, Project_Type %in% input$selected_type)
  })
  
  # Create a subset of data filtering for selected title types --------
  years_subset <- reactive({
    req(input$selected_year) # ensure availablity of value before proceeding
    filter(programs_2, Year %in% input$selected_year)
  })
  
  # Update the maximum allowed n_samp for selected type programs ------
  observe({
    updateNumericInput(session, 
                       inputId = "n_samp",
                       value = min(20, nrow(programs_subset())),
                       max = nrow(programs_subset())
    )
  })
  
  # Create new df that is n_samp obs from selected type programs ------
  programs_sample <- reactive({ 
    req(input$n_samp) # ensure availablity of value before proceeding
    sample_n(programs_subset(), input$n_samp)
  })
  
  # Convert plot_title toTitleCase for trend line --------------------
  pretty_plot_title <- reactive({ toTitleCase(input$plot_title) })
  
  # Convert plot_title2 toTitleCase for bar charts -------------------
  pretty_plot_title2 <- reactive({ toTitleCase(input$plot_title2) })
  
  # Convert plot_title3 toTitleCase for boxplot ----------------------
  pretty_plot_title3 <- reactive({ toTitleCase(input$plot_title3) })
  
  # Create line object the plotOutput function is expecting --
  output$line <- renderPlot({
    ggplot(data = years_subset(), aes_string(x = "Year_Month")) +
      geom_line(stat = 'count', alpha = input$alpha) +
      labs(x = toTitleCase(str_replace_all(input$x, "_", " ")),
           title = pretty_plot_title()
      )
  })
  
  # Create bar chart object the plotOutput function is expecting -----
  output$barchart <- renderPlot({
    ggplot(data = programs_sample(), aes_string(x = input$x,
                                                fill = input$z)) +
      geom_bar(aes(y = (..count..)), alpha = input$alpha) +
      labs(x = toTitleCase(str_replace_all(input$x, "_", " ")),
           title = pretty_plot_title2()
      )
  })
  
  # Create boxplot object the plotOutput function is expecting -------
  output$boxplot <- renderPlot({
    ggplot(data = programs_sample(), 
           aes_string(x = input$x2, y = "Length_Days")) +
      geom_boxplot(alpha = input$alpha) +
      labs(x = toTitleCase(str_replace_all(input$x, "_", " ")),
           y = toTitleCase(str_replace_all(input$y, "_", " ")),
           title = pretty_plot_title3()
      )
  })

  # Print number of programs plotted ---------------------------------
  output$n <- renderUI({
    types <- programs_sample()$Project_Type %>% 
      factor(levels = input$selected_type) 
    counts <- table(types)
    
    HTML(paste("There are", counts, input$selected_type, 
               "temporary NYC art programs in this dataset. <br>"))
  })
  
  # Print data table if checked -------------------------------------
  output$programstable <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = programs_sample()[, 1:7], 
                    extensions = 'Scroller', 
                    options = list(pageLength = 10,
                                   scrollX = TRUE),
                    rownames = FALSE)
    })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("programs_2-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(programs_2, file)
    }
  )
}

# Run the application -----------------------------------------------
shinyApp(ui = ui, server = server)
