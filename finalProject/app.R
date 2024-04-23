library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(dplyr)

# reading in the data
df <- read.csv('cancer patient data sets.csv')

# filtering the data to only have high level lung cancer patients
high <- subset(df, Level == "High")

# i want to focus on dry cough, clubbed finger nails, and age
high_subset <- subset(high, select = c("Clubbing.of.Finger.Nails", "Age", "Dry.Cough"))

# i want the slider bar to only have the minimum and maximum ages, so i'm finding that
minimum_age <- min(high_subset$Age)
maximum_age <- max(high_subset$Age)

# Define UI for application
ui <- page_navbar(
  title = "Final Project",
  theme= bs_theme(
    bootswatch = "lux"
  ),
  underline=T,
      # Tab selection

        tabPanel("Introduction",
           value = "intro",
           fluidPage(h3("Introduction to Dataset")),
           mainPanel()
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
               plotlyOutput("plot1")
             )
           )
         )
        ),
        tabPanel("Question 2", value = "q2",
         fluidPage(
           titlePanel("Analysis of High Level Lung Cancer Symptoms"),
           sidebarLayout(
             sidebarPanel(
               sliderInput("age_range", "Choose Age Range:",
                           min = minimum_age, max = 65, value = c(minimum_age, maximum_age), step = 1)
             ),
             mainPanel(
               plotlyOutput("plot2")
             )
           )
         )
        ),
        tabPanel("Question 3", value = "q3",
         fluidPage(h3("Question 3")),
         mainPanel()
        )
    # Main panel for displaying outputs
)

# Define server logic
server <- function(input, output) {

  cancer_data <- df
  cancer_data <- cancer_data %>%
    mutate(across(c(Smoking, Air.Pollution, Alcohol.use, Obesity, Genetic.Risk, Age, Gender), as.factor))
  output$plot1 <- renderPlotly({
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
  
  
  # Render output for Question 2
  output$plot2 <- renderPlotly({
    # making the data filtered based on the age range in the df using indexing
    filtered <- high_subset %>%
      filter(Age >= input$age_range[1], Age <= input$age_range[2])
    
    p <- ggplot(filtered, aes(x = Age, y = Dry.Cough, color = Clubbing.of.Finger.Nails, text = paste("Age: ", Age, "<br>Dry Cough: ", Dry.Cough, "<br>Clubbing of Finger Nails: ", Clubbing.of.Finger.Nails))) +
      geom_point() +
      labs(title = "Dry Cough vs Age with Clubbing of Finger Nails",
           x = "Age", y = "Dry Cough", color = "Clubbing of Finger Nails") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
}

# Run the application
shinyApp(ui = ui, server = server)