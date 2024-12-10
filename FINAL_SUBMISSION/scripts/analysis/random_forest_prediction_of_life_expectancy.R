# -----------------------------------
# Load Required Libraries
# -----------------------------------
library(tidyverse)
library(readxl)
library(writexl)
library(VIM)      # For visualizing missing data
library(mice)      # For imputation
library(reshape2)  # For reshaping the correlation matrix
library(ggplot2)   # For visualization
library(randomForest)
library(caret)
library(openxlsx)

# -----------------------------------
# Step 1: Read the Life Expectancy Data
# -----------------------------------
df_life_expectancy <- read_csv("./FINAL_SUBMISSION/data/master.csv")

# -----------------------------------
# Step 2: Clean and Preprocess Data
# -----------------------------------
# Replace "NA" string with actual NA for all columns
df_life_expectancy$Developing[df_life_expectancy$Developing == "NA"] <- 1
for (col_name in colnames(df_life_expectancy)) {
  df_life_expectancy[[col_name]][df_life_expectancy[[col_name]] == "NA"] <- NA
}

# -----------------------------------
# Step 3: Handle Missing Data
# -----------------------------------
# Count missing values per column
missing_counts <- colSums(is.na(df_life_expectancy))
missing_percentage <- (missing_counts / nrow(df_life_expectancy)) * 100

# Create a summary table for missing values
missing_summary <- data.frame(
  Column = names(df_life_expectancy),
  Missing_Count = missing_counts,
  Missing_Percentage = missing_percentage
)
print(missing_summary)

# Remove columns with more than 30% missing values
columns_to_remove <- missing_summary$Column[missing_summary$Missing_Percentage > 30]
df_life_expectancy_filtered <- df_life_expectancy[, !names(df_life_expectancy) %in% columns_to_remove]

# Remove rows for specific countries
countries_to_remove <- c("ABW", "AND", "DMA", "HKG", "KNA", "MAC", "MHL", "NRU", 
                         "PLW", "SMR", "TUV", "TWN", "UVK", "WBG")
df_life_expectancy_filtered_cleaned <- df_life_expectancy_filtered[!df_life_expectancy_filtered$ISO %in% countries_to_remove, ]

# Remove rows with missing 'life_expectancy_at_birth_SEX_BTSX'
df_life_expectancy_filtered_cleaned <- df_life_expectancy_filtered_cleaned %>%
  filter(!is.na(life_expectancy_at_birth_SEX_BTSX) & life_expectancy_at_birth_SEX_BTSX != "")

# -----------------------------------
# Step 4: Check and Clean Column Names
# -----------------------------------
# Clean column names for easier manipulation
colnames(df_life_expectancy_filtered_cleaned) <- gsub("%", "percent", colnames(df_life_expectancy_filtered_cleaned))
colnames(df_life_expectancy_filtered_cleaned) <- gsub("-", "_", colnames(df_life_expectancy_filtered_cleaned))
colnames(df_life_expectancy_filtered_cleaned) <- gsub(",", "_", colnames(df_life_expectancy_filtered_cleaned))
colnames(df_life_expectancy_filtered_cleaned) <- gsub("/", "_", colnames(df_life_expectancy_filtered_cleaned))
colnames(df_life_expectancy_filtered_cleaned) <- gsub(" ", "_", colnames(df_life_expectancy_filtered_cleaned))

# -----------------------------------
# Step 5: Convert Relevant Columns to Numeric
# -----------------------------------
columns_to_convert <- c("life_expectancy_at_birth_SEX_MLE", "life_expectancy_at_birth_SEX_BTSX", 
                        "yr_sch", "yr_sch_pri", "yr_sch_sec", "yr_sch_ter", 
                        "Gross_domestic_product_per_capita__current_prices", 
                        "Population", "General_government_revenue", 
                        "General_government_total_expenditure", "General_government_net_lending_borrowing", 
                        "General_government_primary_net_lending_borrowing", "General_government_gross_debt", 
                        "Current_account_balance",
                        "bcg_immunization_coverage", "dtp_immunization_coverage", "hepb3_immunization_coverage", 
                        "hib3_immunization_coverage", "mcv1_immunization_coverage", "pol3_immunization_coverage", 
                        "che_percent_of_gdp", "ext_percent_of_che", "gghe_d_percent_of_che", 
                        "oop_percent_of_che", "pvt_d_percent_of_che", "alc_consump_per_capita_liters_SEX_BTSX", 
                        "pregnant_women_anaemia_prevalence_SEVERITY_TOTAL", 
                        "pregnant_women_mean_hemoglobin", "children_anaemia_prevalence_SEVERITY_TOTAL", 
                        "children_mean_hemoglobin", "thin_children_prevalence_SEX_BTSX_AGEGROUP_YEARS05_19", 
                        "overweight_children_prevalence_SEX_BTSX_AGEGROUP_YEARS05_19", 
                        "obese_children_prevalence_SEX_BTSX_AGEGROUP_YEARS05_19", 
                        "underweight_adults_prevalence_SEX_BTSX", "overweight_adults_prevalence_SEX_BTSX", 
                        "obese_adults_prevalence_SEX_BTSX")
# Convert to numeric
df_life_expectancy_filtered_cleaned[columns_to_convert] <- 
  lapply(df_life_expectancy_filtered_cleaned[columns_to_convert], 
         function(x) as.numeric(as.character(x)))

# -----------------------------------
# Step 6: Select Relevant Columns for Further Analysis
# -----------------------------------
# Define columns for further analysis
selected_columns <- c("ISO", "country", "year", "Developing", "life_expectancy_at_birth_SEX_BTSX", 
                      "yr_sch", "yr_sch_pri", "yr_sch_sec", "yr_sch_ter", 
                      "Gross_domestic_product_per_capita__current_prices", 
                      "Population", "General_government_revenue", 
                      "General_government_total_expenditure", "General_government_net_lending_borrowing", 
                      "General_government_primary_net_lending_borrowing", "General_government_gross_debt", 
                      "Current_account_balance", "bcg_immunization_coverage", "dtp_immunization_coverage", 
                      "hepb3_immunization_coverage", "hib3_immunization_coverage", "mcv1_immunization_coverage", 
                      "pol3_immunization_coverage", "che_percent_of_gdp", "ext_percent_of_che", "gghe_d_percent_of_che", 
                      "oop_percent_of_che", "pvt_d_percent_of_che", "alc_consump_per_capita_liters_SEX_BTSX", 
                      "pregnant_women_anaemia_prevalence_SEVERITY_TOTAL", 
                      "pregnant_women_mean_hemoglobin", "children_anaemia_prevalence_SEVERITY_TOTAL", 
                      "children_mean_hemoglobin", "thin_children_prevalence_SEX_BTSX_AGEGROUP_YEARS05_19", 
                      "overweight_children_prevalence_SEX_BTSX_AGEGROUP_YEARS05_19", 
                      "obese_children_prevalence_SEX_BTSX_AGEGROUP_YEARS05_19", 
                      "underweight_adults_prevalence_SEX_BTSX", 
                      "overweight_adults_prevalence_SEX_BTSX", "obese_adults_prevalence_SEX_BTSX")

df_selected <- df_life_expectancy_filtered_cleaned[, selected_columns]

# -----------------------------------
# Step 7: Convert Relevant Columns to Factor
# -----------------------------------
df_selected$year <- as.integer(df_selected$year)
df_selected$country <- as.factor(df_selected$country)
df_selected$ISO <- as.factor(df_selected$ISO)
df_selected$Developing <- as.factor(df_selected$Developing)

# # Save the selected and cleaned dataset to Excel
# write_xlsx(df_selected, "C://Users//wisen//Downloads//selected_columns_optimized_data.xlsx")

# -----------------------------------
# Step 8: Handle Missing Values via Imputation
# -----------------------------------
# Impute missing data using random forests
imputed_df <- mice(df_selected, m = 1, method = "rf", seed = 123)

# Get the imputed data
completed_data <- complete(imputed_df, 1)

# # Save the completed data to CSV
# write.csv(completed_data, "complete_check.csv")

# -----------------------------------
# Step 9: Create Correlation Matrix
# -----------------------------------
# Remove non-numeric columns for correlation calculation
df_numeric <- completed_data %>%
  select(-country, -year, -ISO, -Developing)

# Calculate correlation matrix
cor_matrix <- cor(df_numeric, use = "pairwise.complete.obs")

# Convert correlation matrix to a data frame for easy export
cor_matrix_df <- as.data.frame(cor_matrix)

# # Write the correlation matrix to an Excel file
# write.xlsx(cor_matrix_df, "C://Users//wisen//Downloads//correlation_matrix2.xlsx")

# -----------------------------------
# Step 10: Selecting Data for Regression Analysis
# -----------------------------------
df_regression <- completed_data %>%
  select(-country, -year, -ISO, -"yr_sch_pri", -"yr_sch_sec", -"yr_sch_ter")  # Exclude 'country', 'year', 'ISO' and school year columns

# Print column names of the regression dataset
colnames(df_regression)

# -----------------------------------
# Step 11: Linear Regression Model
# -----------------------------------
# Fit the linear regression model
lm_model <- lm(life_expectancy_at_birth_SEX_BTSX ~ ., data = df_regression)

# Step 12: Summarize the Linear Model
summary(lm_model)

# Predict the values using the linear model
predictions <- predict(lm_model, newdata = df_regression)

# Calculate residuals and Mean Squared Error (MSE)
residuals <- df_regression$life_expectancy_at_birth_SEX_BTSX - predictions
mse <- mean(residuals^2)
print(mse)

# Step 13: Residuals and Fitted Values
residuals <- lm_model$residuals
fitted_values <- lm_model$fitted.values

# Plot 1: Residuals vs Fitted Values Plot
ggplot(data = NULL, aes(x = fitted_values, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  ggtitle("Residuals vs Fitted Values") +
  xlab("Fitted Values") +
  ylab("Residuals")

# Plot 2: Histogram of Residuals
ggplot(data = NULL, aes(x = residuals)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  ggtitle("Histogram of Residuals") +
  xlab("Residuals") +
  ylab("Frequency")

# Plot 3: Q-Q Plot for Residuals
qqnorm(residuals)
qqline(residuals, col = "red", lwd = 2)

# -----------------------------------
# Step 14: Check for Multicollinearity using VIF (Variance Inflation Factor)
# -----------------------------------
# Install the 'car' package if not already installed (for VIF)
if (!require(car)) install.packages("car", dependencies = TRUE)

# Calculate VIF for the predictors in the linear model
vif_values <- vif(lm_model)

# Step 15: View the VIF values
print(vif_values)

# Step 16: Identify predictors with high VIF values (typically > 5 or 10)
high_vif_predictors <- names(vif_values[vif_values > 5])

# Print the predictors with high VIF values
print(high_vif_predictors)

# -----------------------------------
# Step 17: Select Relevant Columns for Random Forest Regression
# -----------------------------------
# List of columns to select for Random Forest model
columns_to_select <- c("Developing", "life_expectancy_at_birth_SEX_BTSX", "yr_sch", 
                       "Gross_domestic_product_per_capita__current_prices", "Population", 
                       "General_government_gross_debt", "bcg_immunization_coverage", 
                       "dtp_immunization_coverage", "hepb3_immunization_coverage", 
                       "hib3_immunization_coverage", "mcv1_immunization_coverage", 
                       "pol3_immunization_coverage", "che_percent_of_gdp", "ext_percent_of_che", 
                       "gghe_d_percent_of_che", "oop_percent_of_che", "pvt_d_percent_of_che", 
                       "alc_consump_per_capita_liters_SEX_BTSX", 
                       "pregnant_women_anaemia_prevalence_SEVERITY_TOTAL", 
                       "pregnant_women_mean_hemoglobin", "children_anaemia_prevalence_SEVERITY_TOTAL", 
                       "children_mean_hemoglobin", "thin_children_prevalence_SEX_BTSX_AGEGROUP_YEARS05_19", 
                       "overweight_children_prevalence_SEX_BTSX_AGEGROUP_YEARS05_19", 
                       "obese_children_prevalence_SEX_BTSX_AGEGROUP_YEARS05_19", 
                       "underweight_adults_prevalence_SEX_BTSX", "overweight_adults_prevalence_SEX_BTSX", 
                       "obese_adults_prevalence_SEX_BTSX")

# Select the relevant columns for the Random Forest model
df_random_forest <- df_regression[, columns_to_select]

# -----------------------------------
# Step 18: Train Random Forest Model
# -----------------------------------
# Set a seed for reproducibility
set.seed(123)

# Split data into training (80%) and test (20%) sets
trainIndex <- createDataPartition(df_random_forest$life_expectancy_at_birth_SEX_BTSX, p = 0.8, list = FALSE)
train_data <- df_random_forest[trainIndex, ]
test_data <- df_random_forest[-trainIndex, ]

# Train Random Forest model
rf_model2 <- randomForest(life_expectancy_at_birth_SEX_BTSX ~ ., data = train_data, importance = TRUE)

# Step 19: Print Random Forest Model Summary
print(rf_model2)

# -----------------------------------
# Step 20: Predict on the Test Set
# -----------------------------------
rf_predictions <- predict(rf_model2, newdata = test_data)

# -----------------------------------
# Step 21: Evaluate Model Performance
# -----------------------------------
# Calculate Mean Absolute Error (MAE), Mean Squared Error (MSE), and R-squared
mae <- mean(abs(rf_predictions - test_data$life_expectancy_at_birth_SEX_BTSX))
mse <- mean((rf_predictions - test_data$life_expectancy_at_birth_SEX_BTSX)^2)
rsq <- cor(rf_predictions, test_data$life_expectancy_at_birth_SEX_BTSX)^2

cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Mean Squared Error (MSE):", mse, "\n")
cat("R-squared:", rsq, "\n")

# -----------------------------------
# Step 22: Display Feature Importance
# -----------------------------------
# Print the feature importance values
print(rf_model2$importance)

# -----------------------------------
# Step 23: Get Out-of-Bag (OOB) Mean Squared Error (MSE)
# -----------------------------------
# Get the Out-of-Bag (OOB) error estimate
oob_error <- rf_model2$mse[rf_model2$ntree]
cat("Out-of-Bag (OOB) Mean Squared Error (MSE):", oob_error, "\n")

# -----------------------------------
# Step 24: Visualize Feature Importance
# -----------------------------------
# # Save the variable importance plot to a file
# png("C://Users//wisen//Documents//variable_importance_plot1.png", width = 2000, height = 600)
# varImpPlot(rf_model2)
# dev.off()



