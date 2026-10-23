data <- read.csv("coffee_shop_revenue.csv", header = TRUE)

# 1.2 Perform Exploratory Data Analysis (EDA) using R functions/packages

# Check the dimensions of the data (number of rows and columns in the dataset)
dim(data) 

# View the structure of the dataset, checking the type of data and basic information for each column
str(data)  

# Checking for duplicate data
sum(duplicated(data))  # Returns the number of duplicate records

#Checking for missing values in the data
missing_values <- colSums(is.na(data))  # Calculate the number of missing values in each column
missing_percentage <- colSums(is.na(data)) / nrow(data) * 100  # Calculate the percentage of missing values in each column
# Displays the number of missing values and the percentage of missing values
missing_values
missing_percentage


# Plot box plots for all numeric columns to check for outliers
numeric_columns <- c("Number_of_Customers_Per_Day", "Average_Order_Value", 
                     "Operating_Hours_Per_Day", "Number_of_Employees", 
                     "Marketing_Spend_Per_Day", "Location_Foot_Traffic", 
                     "Daily_Revenue")


par(mfrow = c(2, 4)) 
for (col in numeric_columns) {
  boxplot(data[[col]], main = paste("Boxplot for", col), 
          col = "orange", border = "black", horizontal = TRUE)
}
par(mfrow = c(1, 1))  


# Identify outliers in each column
outliers_list <- lapply(numeric_columns, function(col) {
  outliers <- boxplot.stats(data[[col]])$out  # Getting outliers
  return(outliers)
})

# Stores exceptions in a list and outputs them by column name
names(outliers_list) <- numeric_columns

# Prints the outliers for each column, only if they are present
for (col in names(outliers_list)) {
  if (length(outliers_list[[col]]) > 0) {  # Prints only if the length of the exception is greater than 0
    cat(paste(col, ":", paste(outliers_list[[col]], collapse = ", "), "\n"))
  }
}

# Handling of outlier  (deletion of rows containing outlier)
library(dplyr)


remove_outliers <- function(df, numeric_cols) {
  for (col in numeric_cols) {
    stats <- boxplot.stats(df[[col]])
    outlier_values <- stats$out
    df <- df[!df[[col]] %in% outlier_values, ]  # Filter out row containing outlier
  }
  return(df)
}

cleaned_data <- remove_outliers(data, numeric_columns)

# Viewing data after removing outlier
dim(cleaned_data)


# Look at the first few rows of the data to get a quick overview of the basics of the data
head(cleaned_data) # Default view of the first 6 rows of data


# Get summary statistics of the data, including Min, 1st Qu, Median, Mean, 3rd Qu and Max
summary(cleaned_data) 


# Data diversity test - counting unique values in each column of the data to understand the diversity and distribution of the data
unique_values <- sapply(cleaned_data, function(x) length(unique(x)))  
unique_values 

# Correlation test - checks for covariance between independent variables (ensures that the choice of independent variables is appropriate)
library(car)  
vif(lm(Daily_Revenue ~ Number_of_Customers_Per_Day + Average_Order_Value + 
         Operating_Hours_Per_Day + Number_of_Employees + Marketing_Spend_Per_Day + 
         Location_Foot_Traffic, data =cleaned_data))

# summary statistics

# Normalizing numerical data
normalized_data <- as.data.frame(lapply(cleaned_data, function(x) if(is.numeric(x)) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
} else x))

# Calculate and output normalized variance and standard deviation
variance_normalized <- sapply(normalized_data, function(x) if(is.numeric(x)) var(x, na.rm = TRUE) else NA)
sd_normalized <- sapply(normalized_data, function(x) if(is.numeric(x)) sd(x, na.rm = TRUE) else NA)
list(variance_normalized, sd_normalized)


library(e1071)

# Calculate mean, median, range, skewness and kurtosis
mean_values <- sapply(cleaned_data, function(x) if(is.numeric(x)) mean(x, na.rm = TRUE) else NA)
median_values <- sapply(cleaned_data, function(x) if(is.numeric(x)) median(x, na.rm = TRUE) else NA)
range_values <- sapply(cleaned_data, function(x) if(is.numeric(x)) diff(range(x, na.rm = TRUE)) else NA)
skewness_values <- sapply(cleaned_data, function(x) if(is.numeric(x)) skewness(x, na.rm = TRUE) else NA)
kurtosis_values <- sapply(cleaned_data, function(x) if(is.numeric(x)) kurtosis(x, na.rm = TRUE) else NA)


# Displays mean, median, standard deviation, variance, range, skewness and kurtosis
mean_values
median_values
range_values
skewness_values
kurtosis_values

# Calculate and visualise correlations between numerical variables
correlation_matrix <- cor(cleaned_data[, sapply(cleaned_data, is.numeric)])
print(correlation_matrix)
library(corrplot)
corrplot(correlation_matrix, method = "circle")



# 1.3 Visualize the relationships between variables

# 1.3.1 Scatterplot

# Plotting Scatterplot Matrices for Numeric Variables
pairs(cleaned_data[, c("Number_of_Customers_Per_Day", "Average_Order_Value", "Operating_Hours_Per_Day", 
                  "Number_of_Employees", "Marketing_Spend_Per_Day", "Location_Foot_Traffic", "Daily_Revenue")])

# 1.3.2 Histograms and density plots of individual variables

# Plotting a Histogram of Number_of_Customers_Per_Day
hist(cleaned_data$Number_of_Customers_Per_Day, main = "Histogram of Number of Customers Per Day", 
     xlab = "Number of Customers Per Day", col = "lightblue", border = "black")



# Plotting a Histogram of Average_Order_Value 
hist(cleaned_data$Average_Order_Value, main = "Histogram of Average Order Value", 
     xlab = "Average Order Value", col = "lightblue", border = "black")


# Plotting a density of Average_Order_Value 
plot(density(cleaned_data$Average_Order_Value, na.rm = TRUE), 
     main = "Density Plot of Average Order Value", 
     xlab = "Average Order Value", col = "red", lwd = 2)

# Plotting a Histogram of Operating_Hours_Per_Day 
hist(cleaned_data$Operating_Hours_Per_Day, main = "Histogram of Operating Hours Per Day", 
     xlab = "Operating Hours Per Day", col = "lightblue", border = "black")



# Plotting a Histogram of Number_of_Employees 
hist(cleaned_data$Number_of_Employees, main = "Histogram of Number of Employees", 
     xlab = "Number of Employees", col = "lightblue", border = "black")


# Plotting a Histogram of Marketing_Spend_Per_Day 
hist(cleaned_data$Marketing_Spend_Per_Day, main = "Histogram of Marketing Spend Per Day", 
     xlab = "Marketing Spend Per Day", col = "lightblue", border = "black")

# Plotting a density of Marketing_Spend_Per_Day 
plot(density(cleaned_data$Marketing_Spend_Per_Day, na.rm = TRUE), 
     main = "Density Plot of Marketing Spend Per Day", 
     xlab = "Marketing Spend Per Day", col = "red", lwd = 2)

# Plotting a Histogram of Location_Foot_Traffic 
hist(cleaned_data$Location_Foot_Traffic, main = "Histogram of Location Foot Traffic", 
     xlab = "Location Foot Traffic", col = "lightblue", border = "black")


# Plotting a Histogram of Daily_Revenue 
hist(cleaned_data$Daily_Revenue, main = "Histogram of Daily Revenue", 
     xlab = "Daily Revenue", col = "lightblue", border = "black")

# Plotting a density of Daily_Revenue 
plot(density(cleaned_data$Daily_Revenue, na.rm = TRUE), 
     main = "Density Plot of Daily Revenue", 
     xlab = "Daily Revenue", col = "red", lwd = 2)


# 1.3.3 Relationship between independent and dependent variables

# Plotting scatterplots and adding regression lines

# Number_of_Customers_Per_Day vs. Daily_Revenue
plot(cleaned_data$Number_of_Customers_Per_Day, cleaned_data$Daily_Revenue,
     main = "Regression of Number of Customers Per Day vs. Daily Revenue",
     xlab = "Number of Customers Per Day", ylab = "Daily Revenue",
     col = rgb(0, 0, 1, 0.5), pch = 19, cex = 1.2,  
     xlim = c(0, max(cleaned_data$Number_of_Customers_Per_Day) * 1.1),  
     ylim = c(0, max(cleaned_data$Daily_Revenue) * 1.1),  
     bty = "l")  
abline(lm(Daily_Revenue ~ Number_of_Customers_Per_Day, data = cleaned_data), col = "red", lwd = 2)
grid(lwd = 1, col = "gray")

# Average_Order_Value vs. Daily_Revenue 
plot(cleaned_data$Average_Order_Value, cleaned_data$Daily_Revenue,
     main = "Regression of Average Order Value vs. Daily Revenue",
     xlab = "Average Order Value", ylab = "Daily Revenue",
     col = rgb(0, 1, 0, 0.5), pch = 19, cex = 1.2,
     xlim = c(0, max(cleaned_data$Average_Order_Value) * 1.1),
     ylim = c(0, max(cleaned_data$Daily_Revenue) * 1.1),
     bty = "l")
abline(lm(Daily_Revenue ~ Average_Order_Value, data = cleaned_data), col = "red", lwd = 2)
grid(lwd = 1, col = "gray")

# Operating_Hours_Per_Day vs. Daily_Revenue 
plot(cleaned_data$Operating_Hours_Per_Day, cleaned_data$Daily_Revenue,
     main = "Regression of Operating Hours Per Day vs. Daily Revenue",
     xlab = "Operating Hours Per Day", ylab = "Daily Revenue",
     col = rgb(1, 0, 0, 0.5), pch = 19, cex = 1.2,
     xlim = c(0, max(cleaned_data$Operating_Hours_Per_Day) * 1.1),
     ylim = c(0, max(cleaned_data$Daily_Revenue) * 1.1),
     bty = "l")

abline(lm(Daily_Revenue ~ Operating_Hours_Per_Day, data = cleaned_data), col = "red", lwd = 2)
grid(lwd = 1, col = "gray")

# Number_of_Employees vs. Daily_Revenue 
plot(cleaned_data$Number_of_Employees, cleaned_data$Daily_Revenue,
     main = "Regression of Number of Employees vs. Daily Revenue",
     xlab = "Number of Employees", ylab = "Daily Revenue",
     col = rgb(0, 0, 0, 0.5), pch = 19, cex = 1.2,
     xlim = c(0, max(cleaned_data$Number_of_Employees) * 1.1),
     ylim = c(0, max(cleaned_data$Daily_Revenue) * 1.1),
     bty = "l")
abline(lm(Daily_Revenue ~ Number_of_Employees, data = cleaned_data), col = "red", lwd = 2)


grid(lwd = 1, col = "gray")

# Marketing_Spend_Per_Day vs. Daily_Revenue 
plot(cleaned_data$Marketing_Spend_Per_Day, cleaned_data$Daily_Revenue,
     main = "Regression of Marketing Spend Per Day vs. Daily Revenue",
     xlab = "Marketing Spend Per Day", ylab = "Daily Revenue",
     col = rgb(1, 0, 1, 0.5), pch = 19, cex = 1.2,
     xlim = c(0, max(cleaned_data$Marketing_Spend_Per_Day) * 1.1),
     ylim = c(0, max(cleaned_data$Daily_Revenue) * 1.1),
     bty = "l")
abline(lm(Daily_Revenue ~ Marketing_Spend_Per_Day, data = cleaned_data), col = "red", lwd = 2)

grid(lwd = 1, col = "gray")



# Location_Foot_Traffic vs. Daily_Revenue 
plot(cleaned_data$Location_Foot_Traffic, cleaned_data$Daily_Revenue,
     main = "Regression of Location Foot Traffic vs. Daily Revenue",
     xlab = "Location Foot Traffic", ylab = "Daily Revenue",
     col = rgb(0, 1, 1, 0.5), pch = 19, cex = 1.2,
     xlim = c(0, max(cleaned_data$Location_Foot_Traffic) * 1.1),
     ylim = c(0, max(cleaned_data$Daily_Revenue) * 1.1),
     bty = "l")
abline(lm(Daily_Revenue ~ Location_Foot_Traffic, data = cleaned_data), col = "red", lwd = 2)
grid(lwd = 1, col = "gray")

# 2. fit model
# Variable selection according to 2
library(car)  
vif(lm(Daily_Revenue ~ Number_of_Customers_Per_Day + Average_Order_Value + 
         Marketing_Spend_Per_Day , data =cleaned_data))


model<- lm(Daily_Revenue ~ Number_of_Customers_Per_Day + Average_Order_Value + Marketing_Spend_Per_Day , data =cleaned_data)
summary(model)

# Use step() to perform a two-way stepwise regression
full_model <- lm(Daily_Revenue ~ 
                   Number_of_Customers_Per_Day + 
                   Average_Order_Value + 
                   Operating_Hours_Per_Day + 
                   Number_of_Employees + 
                   Marketing_Spend_Per_Day + 
                   Location_Foot_Traffic, 
                 data = data)

stepwise_model <- step(model, 
                       scope = list(lower = ~1, upper = full_model), 
                       direction = "both", 
                       trace = 1)
summary(stepwise_model)

# 3. ANOVA Analysis

#The effect of each independent variable on the dependent variable
anova(stepwise_model)

# Construct ANOVA table (showing formulas)

anova_summary <- data.frame(
  `Source of Variation` = c("Regression (Model)", "Error", "Total"),
  `Sum of Squares` = c("SSR", "SSE", "SST"),
  df = c("p", "n - p - 1", "n - 1"),
  MS = c("MSR = SSR / p", "MSE = SSE / (n - p - 1)", ""),
  `F.statistic` = c("F = MSR / MSE", "", ""),
  `p.Value` = c(" P(F > MSR/MSE)", "", "")
)

# Print ANOVA table
print(anova_summary, row.names = FALSE)


# ANOVA Overall regression analysis
# Get dependent and independent variables
y <- cleaned_data$Daily_Revenue
x <- cleaned_data[, c("Number_of_Customers_Per_Day", "Average_Order_Value", "Marketing_Spend_Per_Day", "Operating_Hours_Per_Day")]  
n <- length(y)
# Calculate the mean value of y
y_bar <- mean(y)
#  Calculate the sum of squares (SSTO)
SSTO <- sum((y - y_bar)^2)
#  Calculation of projected values
y_hat <- predict(stepwise_model) 
# Calculate the sum of squares regression (SSR)
SSR <- sum((y_hat - y_bar)^2)
#  Calculate the sum of squares error (SSE)
SSE <- sum((y - y_hat)^2)

#  Export SSTO, SSR, SSE
cat("SSTO:", SSTO, "\n")
cat("SSR:", SSR, "\n")
cat("SSE:", SSE, "\n")

#  Calculate R^2
R_squared <- SSR / SSTO
cat("R^2 =", R_squared, "\n")


# Calculate the mean square (MSR and MSE)
MSR <- SSR / 4  # Because there are 4 independent variables, the regression has 4 degrees of freedom
MSE <- SSE / (n - 5)  # The degree of freedom is n-5 because there are 4 independent variables and 1 constant term

#  Calculating the F-value
F_stat <- MSR / MSE

#  Calculation of critical values and p-values
critical_value <- qf(0.95, 4, n - 5)  
p_value <- 1 - pf(F_stat, 4, n - 5)

#  Construction of ANOVA table
ANOVA_table <- data.frame(
  "Source of Variation" = c("Regression", "Error", "Total"),
  "Sum of Squares" = c(SSR, SSE, SSTO),
  "df" = c(4, n - 5, n - 1),  
  "MS" = c(MSR, MSE, NA),
  "F-statistic" = c(F_stat, NA, NA),
  "p-value" = c(p_value, NA, NA)
)

#  Output ANOVA table
print(ANOVA_table)

#  Judging significance based on F-values and critical values
if (F_stat > critical_value) {
  cat("At a 0.05 significance level, reject the null hypothesis H0, indicating a significant linear relationship.\n")
} else {
  cat("At a 0.05 significance level, fail to reject the null hypothesis H0, indicating no significant linear relationship.\n")
}

# A more precise expression of p-value
log_p_value <- pf(F_stat, 4, n - 5, lower.tail = FALSE, log.p = TRUE)
print(log_p_value)
cat("p-value ≈ 10^", log_p_value, "\n")

# 4.Diagnostics & Remedial Measures


# Residuals vs Fitted
plot(stepwise_model, 1)
lines(lowess(fitted(stepwise_model), residuals(stepwise_model)), col = "red", lwd = 2)

# Residuals vs Leverage
plot(stepwise_model, 5)
lines(lowess(hatvalues(stepwise_model), rstandard(stepwise_model)), col = "red", lwd = 2) 


# Q-Q Plot with thicker line
qqnorm(residuals(stepwise_model))
qqline(residuals(stepwise_model), col = "red", lwd = 2) 


# Histogram of Residuals
hist(residuals(stepwise_model), main = "Histogram of Residuals", xlab = "Residuals")




# Attempts to solve non-linear problems

# Refit the regression model (using a different transformation)）
# Logarithmically transform Operating_Hours_Per_Day and refit the model
cleaned_data$log_Operating_Hours <- log(cleaned_data$Operating_Hours_Per_Day + 1)  
lm_model_log <- lm(Daily_Revenue ~ Number_of_Customers_Per_Day + 
                     Average_Order_Value + Marketing_Spend_Per_Day + Operating_Hours_Per_Day+ 
                     log_Operating_Hours, data = cleaned_data)

# Square root transformation of Operating_Hours_Per_Day and refit the model
cleaned_data$sqrt_Operating_Hours <- sqrt(cleaned_data$Operating_Hours_Per_Day)   
lm_model_sqrt <- lm(Daily_Revenue ~ Number_of_Customers_Per_Day + 
                      Average_Order_Value + Marketing_Spend_Per_Day +  Operating_Hours_Per_Day+
                      sqrt_Operating_Hours, data = cleaned_data)

# Evaluating the fit of different models
summary(lm_model_log)    
summary(lm_model_sqrt)   

# Adding polynomial regression terms
lm_poly2 <- lm(Daily_Revenue ~ Number_of_Customers_Per_Day + 
                 Average_Order_Value + Marketing_Spend_Per_Day + 
                 poly(Operating_Hours_Per_Day, 2), data = cleaned_data)

lm_poly3 <- lm(Daily_Revenue ~ Number_of_Customers_Per_Day + 
                 Average_Order_Value + Marketing_Spend_Per_Day + 
                 poly(Operating_Hours_Per_Day, 3), data = cleaned_data)

# View model fit results
summary(lm_poly2)  
summary(lm_poly3)  



# Using the Box-Cox Transform
library(MASS)

# Perform Box-Cox transformation and plot lambda curve to identify optimal lambda
bc <- boxcox(stepwise_model, plotit = TRUE)

# Extract and display the optimal lambda value
lambda_optimal <- bc$x[which.max(bc$y)]
cat("Optimal Lambda:", lambda_optimal, "\n")

# Transform the dependent variable using the optimal lambda
cleaned_data$bc_Daily_Revenue <- if(lambda_optimal == 0){
  log(cleaned_data$Daily_Revenue)  # Handle lambda near zero
} else {
  (cleaned_data$Daily_Revenue^lambda_optimal - 1) / lambda_optimal
}

# Rebuild the linear model using Box-Cox transformed dependent variable
lm_bc <- lm(bc_Daily_Revenue ~ Number_of_Customers_Per_Day +
              Average_Order_Value +
              Marketing_Spend_Per_Day +
              Operating_Hours_Per_Day, 
            data = cleaned_data)

# Summary of the new model
summary(lm_bc)

# Diagnostic plot (Residuals vs Fitted values)
plot(lm_bc, 1)
lines(lowess(fitted(lm_bc), residuals(lm_bc)), col = "red", lwd = 2)


