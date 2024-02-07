library(shiny)
library(ggplot2)
library(dplyr)
library(cowplot)
library(shinylive)
library(httpuv)

source("Project2.R")

data_long <- data %>%
  select(age, occasional, regular, previous_smoker, never_smoking) %>%
  pivot_longer(cols = -age, names_to = "category", values_to = "value")

data_wide <- data %>%
  select(age, population_without_information,population_with_information) %>%
  pivot_longer(cols = -age, names_to = "category", values_to = "value")

ui <- fluidPage(
  tags$iframe(src = "digitalhealth.html", width = "100%", height = "600px"),
  tags$style(HTML("body {background-color: rgb(153, 182, 228); margin: 0;}")),
  titlePanel("Smoking Graphs by Age Group in the USA"),
  selectInput("age_group", "Select Age Group:", choices = unique(data$age)),
  fluidRow(
    column(width = 6, plotOutput("pieChart", width= "100%", height = "500px")),
    column(width = 6, plotOutput("barChart", width= "100%", height = "500px"))
  )
)

server <- function(input, output, session) {
    filtered_data <- reactive({
    
    data_long_filtered <- data_long %>%
      filter(age == input$age_group) %>%
      select(category, value)
    
    data_wide_filtered <- data_wide %>%
      filter(age == input$age_group) %>%
      select(category, value)
    list(data_long = data_long_filtered, data_wide = data_wide_filtered)
  })
  
  output$pieChart <- renderPlot({
    ggplot(filtered_data()$data_long, aes(x = "", y = value, fill = category)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste(round(value / sum(value) * 100, 1), "%"), fontface = "bold"),
                position = position_stack(vjust = 0.5),
                color = "white", size = 4) +  
      coord_polar(theta = "y", start = 0) + 
      theme_void() +
      theme(panel.grid = element_blank(),
            legend.position = "right",
            legend.box.margin = margin(5, 5, 5, 5),
            legend.box.background = element_rect(color = "white", fill = "white"),
            plot.background = element_rect(fill = "#99B6E4", color = "#99B6E4"),
            plot.margin = margin(20, 20, 150, 20),
            plot.title = element_text(size = 16, face = "bold")) +
      scale_fill_manual(values = c(
        "occasional" = "purple",
        "regular" = "gray",
        "previous_smoker" = "violet",
        "never_smoking" = "navy"),
        breaks = c("occasional", "regular", "previous_smoker", "never_smoking"),
        labels = c("Occasionally", "Regularly", "Previously", "Never")) +
      ggtitle("Smoking Frequency by Age Group and Percent") +
      labs(fill = "Smoking Frequency", size = 12)

  })
  
  output$barChart <- renderPlot({
    ggplot(filtered_data()$data_wide, aes(x = "", y = value, fill = category)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = value), stat = "identity", position = position_dodge(width = 1), vjust = -0.5, fontface = "bold") +
      labs(title = "Amount of People of that Age Group With and 
Without Access to Information about Smoking",
           x = "Accessibility to Information",
           y = "Amount of People in Age Group (in 1,000s)",
           fill=  "Accessibility to Information") +
      scale_fill_manual(values = c(
        "population_with_information" = "violet",
        "population_without_information" = "navy"),
        breaks = c("population_with_information", "population_without_information"),
        labels = c("With Access", "Without Access")
      ) +
      theme(axis.title.x = element_text(size = 12, face = "bold"),
            axis.title.y = element_text(size = 12, face = "bold"),
            axis.text.x = element_text(size = 12, face = "bold"),
            axis.text.y = element_text(size = 12, face = "bold"),
            plot.title = element_text(size = 16, face = "bold"),
            plot.margin = margin(10, 10, 10, 10),
            plot.background = element_rect(fill = "#99B6E4", color = "#99B6E4"))
    })
}
  
shinyApp(ui = ui, server = server)