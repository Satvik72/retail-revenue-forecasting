# Retail Revenue Forecasting and Temporal Demand Analysis
# Run from the project root in RStudio.
# Put the UCI file "Online Retail.xlsx" inside the data/ folder first.

required <- c("readxl", "dplyr", "tidyr", "lubridate", "ggplot2", "forecast", "tseries", "writexl")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)
invisible(lapply(required, library, character.only = TRUE))

dir.create("outputs", showWarnings = FALSE)
file_path <- "data/Online Retail.xlsx"
if (!file.exists(file_path)) {
  stop("Dataset not found. Download Online Retail.xlsx from the UCI repository and save it to data/Online Retail.xlsx")
}

raw <- readxl::read_excel(file_path)
names(raw) <- make.names(names(raw))
# Expected fields: InvoiceNo, StockCode, Description, Quantity, InvoiceDate, UnitPrice, CustomerID, Country
needed <- c("InvoiceNo", "Quantity", "InvoiceDate", "UnitPrice")
if (!all(needed %in% names(raw))) stop("Expected columns were not found. Check the downloaded UCI file.")

data <- raw |>
  mutate(
    InvoiceDate = lubridate::as_datetime(InvoiceDate),
    Quantity = as.numeric(Quantity),
    UnitPrice = as.numeric(UnitPrice),
    Revenue = Quantity * UnitPrice,
    InvoiceNo = as.character(InvoiceNo)
  ) |>
  filter(!is.na(InvoiceDate), !is.na(Quantity), !is.na(UnitPrice)) |>
  filter(Quantity > 0, UnitPrice > 0, !grepl("^C", InvoiceNo)) |>
  mutate(Date = as.Date(InvoiceDate))

# This study forecasts daily net-positive transaction revenue after excluding cancellations,
# returns/negative quantities and non-positive unit prices. Report this choice as a limitation.
daily <- data |>
  group_by(Date) |>
  summarise(Revenue = sum(Revenue, na.rm = TRUE),
            UnitsSold = sum(Quantity, na.rm = TRUE),
            Transactions = n_distinct(InvoiceNo), .groups = "drop") |>
  complete(Date = seq(min(Date), max(Date), by = "day"),
           fill = list(Revenue = 0, UnitsSold = 0, Transactions = 0)) |>
  arrange(Date)

write.csv(daily, "outputs/daily_retail_metrics.csv", row.names = FALSE)
cat("Raw records:", nrow(raw), "\n")
cat("Retained records after cleaning:", nrow(data), "\n")
cat("Daily observations:", nrow(daily), "\n")
cat("First date:", as.character(min(daily$Date)), "\n")
cat("Last date:", as.character(max(daily$Date)), "\n")

# EDA plots
p1 <- ggplot(daily, aes(Date, Revenue)) +
  geom_line(linewidth = 0.35) +
  labs(title = "Daily Retail Revenue", x = "Date", y = "Revenue (GBP)") +
  theme_minimal()
ggsave("outputs/daily_revenue.png", p1, width = 10, height = 5, dpi = 300)

monthly <- daily |>
  mutate(Month = floor_date(Date, "month")) |>
  group_by(Month) |>
  summarise(Revenue = sum(Revenue), UnitsSold = sum(UnitsSold), .groups = "drop")
write.csv(monthly, "outputs/monthly_retail_metrics.csv", row.names = FALSE)
p2 <- ggplot(monthly, aes(Month, Revenue)) +
  geom_col() +
  labs(title = "Monthly Retail Revenue", x = "Month", y = "Revenue (GBP)") +
  theme_minimal()
ggsave("outputs/monthly_revenue.png", p2, width = 9, height = 5, dpi = 300)

# Time-ordered holdout: reserve the final 60 days for testing.
h <- min(60, max(14, floor(nrow(daily) * 0.15)))
if (nrow(daily) <= h + 30) stop("Not enough daily observations for a train/test split.")
train <- head(daily$Revenue, nrow(daily) - h)
test <- tail(daily$Revenue, h)
train_ts <- ts(train, frequency = 7)

fit_arima <- forecast::auto.arima(train_ts, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
fit_ets <- forecast::ets(train_ts)
fit_naive <- forecast::snaive(train_ts, h = h)
pred_arima <- as.numeric(forecast::forecast(fit_arima, h = h)$mean)
pred_ets <- as.numeric(forecast::forecast(fit_ets, h = h)$mean)
pred_snaive <- as.numeric(fit_naive$mean)

predictions <- tibble(
  Date = tail(daily$Date, h),
  Actual = test,
  SeasonalNaive = pmax(0, pred_snaive),
  ETS = pmax(0, pred_ets),
  ARIMA = pmax(0, pred_arima)
)
write.csv(predictions, "outputs/forecast_predictions.csv", row.names = FALSE)

metric_fun <- function(actual, predicted, model) {
  tibble(
    Model = model,
    MAE = mean(abs(actual - predicted), na.rm = TRUE),
    RMSE = sqrt(mean((actual - predicted)^2, na.rm = TRUE)),
    # MAPE is computed only where actual revenue is non-zero.
    MAPE_percent = if (any(actual != 0)) mean(abs((actual[actual != 0] - predicted[actual != 0]) / actual[actual != 0])) * 100 else NA_real_,
    sMAPE_percent = mean(200 * abs(actual - predicted) / (abs(actual) + abs(predicted) + 1e-8), na.rm = TRUE)
  )
}
metrics <- bind_rows(
  metric_fun(predictions$Actual, predictions$SeasonalNaive, "Seasonal Naive"),
  metric_fun(predictions$Actual, predictions$ETS, "ETS"),
  metric_fun(predictions$Actual, predictions$ARIMA, "ARIMA")
)
write.csv(metrics, "outputs/model_metrics.csv", row.names = FALSE)
print(metrics)

pred_long <- predictions |>
  pivot_longer(cols = c(Actual, SeasonalNaive, ETS, ARIMA),
               names_to = "Series", values_to = "Revenue")
p3 <- ggplot(pred_long, aes(Date, Revenue, color = Series)) +
  geom_line(linewidth = 0.6) +
  labs(title = paste0("Actual vs Forecast Revenue (", h, "-day holdout)"),
       x = "Date", y = "Revenue (GBP)", color = "Series") +
  theme_minimal()
ggsave("outputs/forecast_comparison.png", p3, width = 10, height = 5, dpi = 300)

# Statistical analysis: compare weekday revenue distributions using Kruskal-Wallis.
daily_weekday <- daily |> mutate(Weekday = wday(Date, label = TRUE, week_start = 1))
kw <- kruskal.test(Revenue ~ Weekday, data = daily_weekday)
capture.output(kw, file = "outputs/weekday_kruskal_test.txt")
print(kw)

# Optional stationarity diagnostic on training series; interpret cautiously.
capture.output({
  cat("ADF test on training daily revenue:\n")
  print(tseries::adf.test(train))
}, file = "outputs/adf_test.txt")

cat("\nFinished. Review the CSVs, PNGs, and test outputs in the outputs/ folder.\n")
