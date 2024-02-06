library(pacman)

pacman::p_load(
  here,
  janitor,
  rio,
  ggplot2,
  tidyverse,
  cowplot
)

surv_raw <- import(here("data2.csv"))

data <- read.csv("data2.csv")


data_long <- data %>%
  select(age, occasional, regular, previous_smoker, never_smoking) %>%
  pivot_longer(cols = -age, names_to = "category", values_to = "value")

data_wide <- data %>%
  select(age, population_without_information,population_with_information) %>%
  pivot_longer(cols = -age, names_to = "category", values_to = "value")
  
pie_chart <- ggplot(data_long, aes(x = "", y = value, fill = category)) +
    geom_bar(stat = "identity") +
    coord_polar(theta = "y") + 
    theme_void() +
    theme(panel.grid = element_blank(),
          legend.position = "right",
          legend.box.margin = margin(5, 5, 5, 5),
          legend.box.background = element_rect(color = "white", fill = "white"),
          plot.background = element_rect(fill = "#99B6E4", color = "#99B6E4"), 
          plot.margin = margin(10, 10, 10, 10),
          plot.title = element_text(size = 16, face = "bold", margin= margin(10, 10, 10, 10))
    ) +
    scale_fill_manual(values = c(
      "occasional" = "lightblue",
      "regular" = "gray",
      "previous_smoker" = "violet",
      "never_smoking" = "navy"),
      breaks = c("occasional", "regular", "previous_smoker", "never_smoking"),
      labels = c("Occasionally", "Regularly", "Previously", "Never")) +
    ggtitle("Smoking Frequency by Age Group & Percent") +
    labs(fill = "Smoking Frequency", size = 12)
  
   bar_chart <- ggplot(data_wide, aes(x = "", y = value, fill = category)) +
     geom_bar(stat = "identity", position = "dodge") +
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
  
  print(pie_chart)
  
  print(bar_chart)




