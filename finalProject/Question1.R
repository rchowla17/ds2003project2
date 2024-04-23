file = "cancer patient data sets.csv"

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)


ui <- fluidPage(
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

server <- function(input, output) {
  cancer_data <- read.csv("C:/Users/elija/Documents/ds2003project2/cancer patient data sets.csv")
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
}

shinyApp(ui = ui, server = server)
