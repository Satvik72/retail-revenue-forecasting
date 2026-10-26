# Retail Revenue Forecasting and Temporal Demand Analysis Using R

## Files
- `analysis.R`: data preparation, EDA, statistical analysis, model training, evaluation and charts.
- `app.R`: R Shiny dashboard (run after `analysis.R`).
- `data/`: place the official UCI `Online Retail.xlsx` file here.
- `outputs/`: generated CSVs, figures and test results.

## Setup
1. Download the dataset from https://archive.ics.uci.edu/dataset/352/online+retail
2. Save the downloaded `Online Retail.xlsx` file into this project's `data` folder.
3. Open this folder as an RStudio Project (or set it as the working directory).
4. Run `analysis.R` from top to bottom.
5. Review generated metrics and plots in `outputs/`.
6. Run `app.R` to launch the Shiny dashboard.

## Important research integrity notes
- Do not write model results into the paper until you have run the code and inspected `outputs/model_metrics.csv`.
- The script excludes cancellations, negative quantities/returns and non-positive unit prices. State this clearly and discuss how it affects revenue estimates.
- The final 60 days are used as a time-ordered holdout. Do not randomly split time-series observations.
- MAPE ignores days where actual revenue is zero; report this definition and consider sMAPE/MAE/RMSE together.
- This is historical UK online retail data, not contemporary Indian market data.
