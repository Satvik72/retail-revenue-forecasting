
# Retail Revenue Intelligence Dashboard
# Run analysis.R first so the outputs folder is ready.

required <- c(
  "shiny", "ggplot2", "dplyr",
  "tidyr", "lubridate", "readr"
)

missing <- required[
  !vapply(required, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing) > 0) {
  install.packages(missing)
}

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)

# Convert numeric day counts into proper dates.
# The value 14944 represents 2010-12-01
# when counted from 1970-01-01.

fix_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  
  txt <- as.character(x)
  numeric_like <- !is.na(txt) &
    grepl("^-?[0-9]+(\\.0+)?$", txt)
  
  if (all(numeric_like | is.na(txt))) {
    return(as.Date(as.numeric(txt), origin = "1970-01-01"))
  }
  
  as.Date(txt)
}

# Load generated output files.
daily <- read_csv(
  "outputs/daily_retail_metrics.csv",
  show_col_types = FALSE
)

pred <- read_csv(
  "outputs/forecast_predictions.csv",
  show_col_types = FALSE
)

metrics_path <- "outputs/model_metrics.csv"

if (file.exists(metrics_path)) {
  metrics <- read_csv(metrics_path, show_col_types = FALSE)
} else {
  metrics <- data.frame(
    Message = "Model metrics file was not found."
  )
}

# FIX: Convert dates in BOTH datasets.
daily <- daily |>
  mutate(Date = fix_date(Date))

pred <- pred |>
  mutate(Date = fix_date(Date))

# Check that the required columns exist.
stopifnot(
  all(c("Date", "Revenue") %in% names(daily)),
  all(c("Date", "Actual", "SeasonalNaive", "ETS", "ARIMA")
      %in% names(pred))
)

# Dashboard interface.
ui <- fluidPage(
  titlePanel("Retail Revenue Intelligence Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(
        "dates",
        "Historical date range",
        start = min(daily$Date, na.rm = TRUE),
        end = max(daily$Date, na.rm = TRUE),
        min = min(daily$Date, na.rm = TRUE),
        max = max(daily$Date, na.rm = TRUE),
        format = "yyyy-mm-dd"
      ),
      
      helpText(
        "Forecast metrics use a time-ordered holdout period."
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Overview",
          plotOutput("dailyPlot", height = "330px"),
          plotOutput("monthlyPlot", height = "330px")
        ),
        
        tabPanel(
          "Forecast Lab",
          plotOutput("forecastPlot", height = "380px"),
          tableOutput("metricTable")
        ),
        
        tabPanel(
          "Data",
          tableOutput("dailyTable")
        )
      )
    )
  )
)

# Dashboard calculations and plots.
server <- function(input, output, session) {
  
  filtered <- reactive({
    req(input$dates)
    
    daily |>
      filter(
        Date >= input$dates[1],
        Date <= input$dates[2]
      )
  })
  
  output$dailyPlot <- renderPlot({
    ggplot(filtered(), aes(x = Date, y = Revenue)) +
      geom_line() +
      labs(
        title = "Daily Revenue",
        x = "Date",
        y = "Revenue (GBP)"
      ) +
      theme_minimal()
  })
  
  output$monthlyPlot <- renderPlot({
    filtered() |>
      mutate(Month = floor_date(Date, "month")) |>
      group_by(Month) |>
      summarise(
        Revenue = sum(Revenue, na.rm = TRUE),
        .groups = "drop"
      ) |>
      ggplot(aes(x = Month, y = Revenue)) +
      geom_col() +
      labs(
        title = "Monthly Revenue",
        x = "Month",
        y = "Revenue (GBP)"
      ) +
      theme_minimal()
  })
  
  output$forecastPlot <- renderPlot({
    pred_long <- pred |>
      pivot_longer(
        cols = c(Actual, SeasonalNaive, ETS, ARIMA),
        names_to = "Series",
        values_to = "Revenue"
      )
    
    ggplot(
      pred_long,
      aes(x = Date, y = Revenue, color = Series)
    ) +
      geom_line() +
      labs(
        title = "Forecast Model Comparison",
        x = "Date",
        y = "Revenue (GBP)",
        color = "Series"
      ) +
      theme_minimal()
  })
  
  output$metricTable <- renderTable({
    metrics
  })
  
  output$dailyTable <- renderTable({
    filtered() |>
      mutate(Date = format(Date, "%Y-%m-%d")) |>
      head(100)
  })
}

shinyApp(ui, server)
