# Coffee Shop Revenue Analysis

An R coursework project exploring coffee-shop daily revenue using data visualization, multiple linear regression and model diagnostics.

## What this project demonstrates
- Exploratory data analysis and checks for missing values, duplicates and outliers.
- Correlation analysis and data visualization.
- Multiple linear regression, stepwise variable selection and ANOVA.
- Residual diagnostics and exploratory variable transformations.

## Run
Requires R and the following packages:

```r
install.packages(c("dplyr", "car", "e1071", "corrplot", "MASS"))
```

Run from the repository folder so that the relative path resolves to `coffee_shop_revenue.csv`:

```sh
Rscript analysis.R
```

Run the script in RStudio or R. It creates plots and statistical summaries. 

## Data
The analysis uses 700 records and seven variables from the [Coffee Shop Daily Revenue Prediction Dataset](https://www.kaggle.com/datasets/himelsarder/coffee-shop-daily-revenue-prediction-dataset), published on Kaggle by Himel Sarder. The coursework report states that the first 700 records were used. The local file is `coffee_shop_revenue.csv`.

## Interpretation
The analysis explores relationships with daily revenue. Outlier removal may affect the results. Without an independent test set or cross-validation, model fit does not establish predictive performance on new data. Associations do not imply causation.

## Provenance
This project was originally developed as part of a university coursework assignment. 
