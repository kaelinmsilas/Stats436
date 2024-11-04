library(shiny)
library(dplyr)
library(ggplot2)
library(readr)

cases <- read_csv("https://raw.githubusercontent.com/kaelinmsilas/Stats436/main/covid-data.csv")
cases$date <- as.Date(cases$date)
cases$year <- format(cases$date, "%Y")

country_year_cases <- cases %>%
  group_by(location, year) %>%
  summarize(total_cases = sum(new_cases, na.rm = TRUE)) %>%
  ungroup()
country_year_cases$year <- as.numeric(country_year_cases$year)

top_20_countries <- c("China", "India", "United States", "Indonesia", "Pakistan", "Nigeria", "Brazil", "Bangladesh", "Russia", "Mexico", 
                      "Japan", "Ethiopia", "Philippines", "Egypt", "Vietnam", 
                      "DR Congo", "Turkey", "Iran", "Germany", "Thailand")

filtered_data <- country_year_cases %>%
  filter(location %in% top_20_countries)

ui <- fluidPage(
  titlePanel("COVID-19 Cases by Country Over Years (Top 20 Populated Countries)"),
  
  sidebarLayout(
    sidebarPanel(
      textInput(
        inputId = "country_input",
        label = "Type Country Names (comma-separated):",
        value = paste(top_20_countries[1:5], collapse = ", ") 
      )
    ),
    mainPanel(
      plotOutput("covid_plot")
    )
  )
)

server <- function(input, output) {
  output$covid_plot <- renderPlot({
    selected_countries <- unlist(strsplit(trimws(input$country_input), ",\\s*"))
    
    selected_countries <- selected_countries[selected_countries != ""]
    
    plot_data <- filtered_data %>%
      filter(location %in% selected_countries)
    
    ggplot(plot_data, aes(x = factor(year), y = total_cases, fill = location)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "COVID-19 Cases by Country (Top 20 Populated Countries)",
           x = "Year",
           y = "Total Cases") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_viridis_d(name = "Country", option = "D")
  })
}

shinyApp(ui = ui, server = server)
