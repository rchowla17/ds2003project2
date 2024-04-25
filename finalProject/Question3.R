#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)

dataset <- read.csv("cancer patient data sets.csv")

mini_dataset <- dataset %>%
  select(Age, Gender, Chest.Pain, Fatigue, Shortness.of.Breath, Wheezing, Frequent.Cold, Dry.Cough, Level)


# Define the UI
ui <- fluidPage(
  titlePanel("Comparing Specific Symptoms and Lung Cancer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("symptom", "Select Symptom:",
                  choices = c("Chest Pain", "Fatigue", "Shortness of Breath", "Wheezing", 
                              "Frequent Cold", "Dry Cough"),
                  selected = "Chest Pain"),
      
      uiOutput("slider")
    ),
    
    mainPanel(
      plotlyOutput("plot")
    )
  )
)




server <- function(input, output) {
  categories <- c('Low', 'Medium', 'High')
  
  counts <- reactive({
    
     filtered_data <- switch(input$symptom,
                             "Chest Pain" = mini_dataset %>% filter(Chest.Pain <= input$chestpain),
                             "Fatigue" = mini_dataset %>% filter(Fatigue <= input$fatigue),
                             "Shortness of Breath" = mini_dataset %>% filter(Shortness.of.Breath <= input$shortbreath),
                             "Wheezing" = mini_dataset %>% filter(Wheezing <= input$wheeze),
                             "Frequent Cold" = mini_dataset %>% filter(Frequent.Cold <= input$frequentcold),
                             "Dry Cough" = mini_dataset %>% filter(Dry.Cough <= input$drycough))
     
  
    counts <- filtered_data %>%
      group_by(Level) %>%
      summarise(count = n())
    
    counts <- merge(data.frame(Level = categories), counts, by = "Level", all.x = TRUE)
    
    counts$count[is.na(counts$count)] <- 0
    
    counts$count
    
  })
  
  output$plot <- renderPlotly({
    graph <- plot_ly(x = categories, y = counts(), type = "bar") %>%
      layout(
        yaxis = list(range = c(0, 1000), tickmode = "linear", tick0 = 0, dtick = 50),
        xaxis = list(categoryorder = "array", categoryarray = c("Low", "Medium", "High")))
  })
  
  output$slider <- renderUI({
    slider <- switch(input$symptom,
    "Chest Pain" = sliderInput("chestpain", label = "Chest Pain",
                               min = 1, max = 9, value = 4, step = 1),
    "Fatigue" = sliderInput("fatigue", label = "Fatigue",
                            min = 1, max = 9, value = 4, step = 1),
    "Shortness of Breath" = sliderInput("shortbreath", label = "Shortness of Breath",
                                        min = 1, max = 9, value = 4, step = 1),
    "Wheezing" = sliderInput("wheeze", label = "Wheezing",
                             min = 1, max = 8, value = 4, step = 1), 
    "Frequent Cold" = sliderInput("frequentcold", label = "Frequent Cold",
                                  min = 1, max = 9, value = 4, step = 1),
    "Dry Cough" =  sliderInput("drycough", label = "Dry Cough",
                               min = 1, max = 7, value = 4), step = 1)
  })
}


shinyApp(ui = ui, server = server)
