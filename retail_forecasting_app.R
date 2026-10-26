library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)

daily <- read_csv("outputs/daily_retail_metrics.csv", show_col_types = FALSE) |>
  mutate(Date = as.Date(Date))
pred <- read_csv("outputs/forecast_predictions.csv", show_col_types = FALSE) |>
  mutate(Date = as.Date(Date))
metrics <- read_csv("outputs/model_metrics.csv", show_col_types = FALSE)

ui <- fluidPage(
  titlePanel("Retail Revenue Intelligence Dashboard"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("dates", "Historical date range",
        start = min(daily$Date), end = max(daily$Date),
        min = min(daily$Date), max = max(daily$Date), format = "yyyy-mm-dd"),
      helpText("Forecast metrics use a chronological holdout period.")
    ),
    mainPanel(tabsetPanel(
      tabPanel("Overview", plotOutput("dailyPlot", height="330px"),
               plotOutput("monthlyPlot", height="330px")),
      tabPanel("Forecast Lab", plotOutput("forecastPlot", height="380px"),
               tableOutput("metricTable"),
               helpText("MAPE excludes dates with zero actual revenue.")),
      tabPanel("Data", tableOutput("dailyTable"))
    ))
  )
)
server <- function(input, output, session) {
  filtered <- reactive(daily |> filter(Date >= input$dates[1], Date <= input$dates[2]))
  output$dailyPlot <- renderPlot(ggplot(filtered(), aes(Date, Revenue)) +
    geom_line() + labs(title="Daily retail revenue", x="Date", y="Revenue (GBP)") + theme_minimal())
  output$monthlyPlot <- renderPlot(filtered() |> mutate(Month=floor_date(Date, "month")) |>
    group_by(Month) |> summarise(Revenue=sum(Revenue), .groups="drop") |>
    ggplot(aes(Month, Revenue)) + geom_col() +
    labs(title="Monthly retail revenue", x="Month", y="Revenue (GBP)") + theme_minimal())
  output$forecastPlot <- renderPlot(pred |> pivot_longer(c(Actual, SeasonalNaive, ETS, ARIMA),
    names_to="Series", values_to="Revenue") |> ggplot(aes(Date, Revenue, color=Series)) +
    geom_line() + labs(title="Actual vs forecast", x="Date", y="Revenue (GBP)") + theme_minimal())
  output$metricTable <- renderTable(metrics |> mutate(across(where(is.numeric), ~round(.x, 2))))
  output$dailyTable <- renderTable(filtered() |> mutate(Date=format(Date, "%Y-%m-%d"),
    Revenue=round(Revenue,2)) |> head(100))
}
shinyApp(ui, server)
