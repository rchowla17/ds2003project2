library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(dplyr)

# Define UI for application
ui <- page_navbar(
  title = "Final Project",
  underline=T,
      # Tab selection

        tabPanel("Introduction",
           value = "intro",
           fluidPage(h3("Introduction to Dataset")),
           mainPanel(
             # Output
             uiOutput("output")
           )
        ),
        tabPanel("Question 1", value = "q1",
         fluidPage(
           titlePanel("Risk Factors and Lung Cancer Visualization"),
           sidebarLayout(
             sidebarPanel(
               selectInput("xaxis", "Choose a variable for the X-axis:", 
                           choices = c("Smoking", "Air.Pollution", "Alcohol.use", "Obesity", "Genetic.Risk")),
               selectInput("color", "Choose a variable for color coding:",
                           choices = c("Age", "Gender", "Smoking", "Air.Pollution", "Alcohol.use", "Obesity", "Genetic.Risk"))
             ),
             mainPanel(
               plotlyOutput("plot")
             )
           )
         )
        ),
        tabPanel("Question 2", value = "q2",
         fluidPage(h3("Question 2")),
         mainPanel(
           # Output
           uiOutput("output")
         )
        ),
        tabPanel("Question 3", value = "q3",
         fluidPage(h3("Question 3")),
         mainPanel(
           # Output
           uiOutput("output")
         )
        )
    # Main panel for displaying outputs
)

# Define server logic
server <- function(input, output) {
  
  # Function to render output based on selected tab
  output$output <- renderUI({
    selected_tab <- input$tabsetPanel
    if (is.null(selected_tab)) {
      return(NULL)
    } else if (selected_tab == "intro") {
      # Render output for Intro
      fluidRow(
        h3("Introduction to dataset"),
        # Add your output elements for Question 1 here
      )
    } else if (selected_tab == "q1") {
      # Render output for Question 1
      cancer_data <- read.csv("cancer patient data sets.csv")
      cancer_data <- cancer_data %>%
        mutate(across(c(Smoking, Air.Pollution, Alcohol.use, Obesity, Genetic.Risk, Age, Gender), as.factor))
      output$plot <- renderPlotly({
        if (input$xaxis != input$color) {
          gg <- ggplot(cancer_data, aes_string(x = input$xaxis, fill = input$color, group = input$color)) +
            geom_bar(stat = "count", position = position_dodge()) +
            labs(x = input$xaxis, fill = input$color) +
            theme_minimal()
          ggplotly(gg)
        } else {
          gg <- ggplot() +
            labs(title = "Please select different variables for X-axis and color.")
          ggplotly(gg)
        }
      })
    } else if (selected_tab == "q2") {
      # Render output for Question 2
      fluidRow(
        column(12,
               h3("Answer to Question 2"),
               # Add your output elements for Question 2 here
        )
      )
    } else if (selected_tab == "q3") {
      # Render output for Question 3
      fluidRow(
        column(12,
               h3("Answer to Question 3"),
               # Add your output elements for Question 3 here
        )
      )
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)