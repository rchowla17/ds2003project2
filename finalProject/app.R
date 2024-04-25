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
  
  tabPanel("Introduction", value = "intro",
           fluidPage(h3("Introduction to Dataset and Analysis")),
           
           p("This is a comprehensive data set comprised of lung cancer patients and their associated risk factors."),
           a("Kaggle data set link",
             href = "https://www.kaggle.com/datasets/thedevastator/cancer-patients-and-air-pollution-a-new-link",
             target = "_blank",
             tags$head(tags$style(HTML("
               a { color: #007bff; } /* blue */
               a:hover { color: #ffa700; } /* orange */
             ")))
            ),
            tags$hr(),
            p("Each entry in the data set is classified by a unique 'Patient ID' which is followed by demographic information like 'Gender' and 'Age'. The rest of the data set is comprised of variables known as risk factors, which are associated with an increased liklihood of developing lung cancer."),
            p("Risk factor variables are scored on a scale of 1-8 with 8 repersenting the highest severity."),
             selectInput("variable_selector", "Select a Risk Factor:",
                         choices = c( "Smoking", "Obesity", "Air Pollution", "Alcohol Use", "OccuPational Hazards", "Genetic Risk", "Passive Smoker", "Clubbing of Finger Nails", "Wheezing", "Balanced Diet" )),
             verbatimTextOutput("variable_details"),
             tags$hr(),
             h4("Research Questions and Data Analysis"),
             p("Lung Cancer is one of the most prevalent and deadly types of cancer. For a long time, it was believed that smoking was the leading contributor to lung cancer; however, new studies have identified other risk factors that increase the likelihood of cancer. Through analysis of this dataset, we hope to identify these risk factors and find key correlations. Below are our targeted research questions: "),
             tags$ul(
               tags$li("Are there any interactions between variables that are most likely to result in a patient contracting lung cancer/which behaviors are most preventative?"),
               tags$li("Which age group with high level lung cancer is most affected by dry cough and clubbing of finger nails?"),
               tags$li("How does the severity of symptoms relate to different levels of lung cancer?")
             ),
                div(style = "display: flex; justify-content: flex-end; align-items: center;",
      tags$img(src = "dataset-cover.jpeg", style = "width: 300px; margin-right: 30px;"),  # First image
      tags$img(src = "facts-mortality-world.jpg", style = "width: 500px;")  # Second image
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
         fluidPage(
           titlePanel("Comparing Specific Symptoms to Levels of Lung Cancer"),
            sidebarLayout(
              sidebarPanel(
                selectInput("symptom", "Select Symptom:",
                                   choices = c("Chest Pain", "Fatigue", "Shortness of Breath", "Wheezing", 
                                               "Frequent Cold", "Dry Cough"),
                                   selected = "Chest Pain"),
                       
                       uiOutput("slider")
                     ),
                     
                     mainPanel(
                       plotlyOutput("graph3"),
                       textOutput("caption3")
                     )
              )
           )
        ), 
        tabPanel("Results", value = 'results',
           fluidPage(
             titlePanel("Results and Takeaways"),
             br(), br(), br(),
             fluidRow(
               column(4,
                      h3("Question 1: "), p("Are there any interactions between variables that are most likely to result in a patient contracting lung cancer?")   
               ),
               column(4,
                      h3("Question 2: "), p(" Which age group with high level lung cancer is most affected by dry cough and clubbing of finger nails?")    
               ),
               column(4,
                      h3("Question 3:  "), p("How does the severity of symptoms relate to different levels of lung cancer?"),    
               )
             ) ,
             fluidRow(
               column(4, h4("Takeaway: "), p("Lung cancer risk isn't solely contingent on the factors provided, like smoking or genetics alone, but is influenced by a many interconnected elements. However, we can identify the trend that those with higher levels of risk typically are at a greater risk of developing lung cancer. ")),
               column(4, h4("Takeaway: "), p("There is seemingly no correlation between age groups with dry cough and clubbing of fingernails of those with high level lung cancer. There is very little data for the selected patients above age 45, likely because of the fatality of high level lung cancer.")),
               column(4, h4("Takeaway:"), p("The majority of patients with high level lung cancer rated most of their symptoms lower while patients with low level lung cancer had overall a higher magnitude of their symptoms. This could be due to the fact that patients with a higher level of cancer may be more use to those symptoms."))
             )
           ) 
  )
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
  output$variable_details <- renderText({
    req(input$variable_selector)  # Ensure a variable is selected
    if (input$variable_selector == "Smoking") {
      return("Smoking: The level of smoking of the patient. (Categorical, Mean ± S): 3.99 ± 2.64)")
    } else if (input$variable_selector == "Obesity") {
      return("Obesity: The level of obesity of the patient. (Categorical, Mean ± S): 4.47 ± 2.16)")
    }
    else if (input$variable_selector == "OccuPational Hazards") {
      return("Occupational Hazards: The patient's exposure to occupational hazards. (Categorical, Mean ± SD: 4.15 ± 2.44)")
    }
    else if (input$variable_selector == "Air Pollution") {
      return("Air Pollution: The level of air pollution exposure of the patient. (Categorical, Mean ± S):  3.84 ± 2.30)")
    }else if (input$variable_selector == "Air Pollution") {
      return("Air Pollution: The level of air pollution exposure of the patient. (Categorical, Mean ± SD: 3.84 ± 2.30)")
    } else if (input$variable_selector == "Alcohol Use") {
      return("Alcohol Use: The level of alcohol consumption of the patient. (Categorical, Mean ± SD: 3.76 ± 2.60)")
    } else if (input$variable_selector == "Occupational Hazards") {
      return("Occupational Hazards: The patient's exposure to occupational hazards. (Categorical, Mean ± SD: 4.15 ± 2.44)")
    } else if (input$variable_selector == "Genetic Risk") {
      return("Genetic Risk: The genetic risk factors of the patient for lung cancer. (Categorical, Mean ± SD: 4.51 ± 2.38)")
    } else if (input$variable_selector == "Passive Smoker") {
      return("Passive Smoker: Exposure to second-hand smoke. (Categorical, Mean ± SD: 3.59 ± 2.28)")
    } else if (input$variable_selector == "Clubbing of Finger Nails") {
      return("Clubbing of Finger Nails: Presence of clubbing in fingernails, a potential symptom of lung cancer. (Categorical, Mean ± SD: 3.92 ± 2.35)")
    } else if (input$variable_selector == "Wheezing") {
      return("Wheezing: Frequency of wheezing as a symptom in the patient. (Categorical, Mean ± SD: 3.84 ± 2.51)")
    } else if (input$variable_selector == "Balanced Diet") {
      return("Balanced Diet: Adherence to a balanced diet by the patient. (Categorical, Mean ± SD: 4.09 ± 2.43)")
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
  
  mini_dataset <- df %>%
    select(Age, Gender, Chest.Pain, Fatigue, Shortness.of.Breath, Wheezing, Frequent.Cold, Dry.Cough, Level)
  
  categories <- c('Low', 'Medium', 'High')
  counts <- reactive({
    filtered_data <- switch(input$symptom,
                            "Chest Pain" = mini_dataset %>% filter(Chest.Pain == input$chestpain),
                            "Fatigue" = mini_dataset %>% filter(Fatigue == input$fatigue),
                            "Shortness of Breath" = mini_dataset %>% filter(Shortness.of.Breath == input$shortbreath),
                            "Wheezing" = mini_dataset %>% filter(Wheezing == input$wheeze),
                            "Frequent Cold" = mini_dataset %>% filter(Frequent.Cold == input$frequentcold),
                            "Dry Cough" = mini_dataset %>% filter(Dry.Cough == input$drycough))
    
    
    counts <- filtered_data %>%
      group_by(Level) %>%
      summarise(count = n())
    
    counts <- merge(data.frame(Level = categories), counts, by = "Level", all.x = TRUE)
    
    counts$count[is.na(counts$count)] <- 0
    
    counts$count
  })
  
  output$caption3 <- renderText("This graph shows the number of patients that exhibited symptoms of that magnitude grouped in their respective levels of Lung Cancer compared to the total number of patients with that level of Lung Cancer")
  
  output$graph3 <- renderPlotly({
    graph <- plot_ly(x = categories, y = counts(), type = "bar") %>%
      layout(
        yaxis = list(range = c(0, 400), tickmode = "linear", tick0 = 0, dtick = 40),
        xaxis = list(categoryorder = "array", categoryarray = c("Low", "Medium", "High")),
        annotations = list(
          list(x = categories[1], y = 303, text = paste("Out of ", 303), showarrow = FALSE),
          list(x = categories[2], y = 332, text = paste("Out of ", 332), showarrow = FALSE),
          list(x = categories[3], y = 365, text = paste("Out of ", 365), showarrow = FALSE)
        ))
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

# Run the application
shinyApp(ui = ui, server = server)
