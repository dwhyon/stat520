# Load required libraries
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

# Step 1: Read the Life Expectancy Data
df_life_expectancy <- read_csv("C://Users//wisen//Downloads//WHO_numeric_data.csv")

# Replace "NA" string with actual NA for all columns
for (col_name in colnames(df_life_expectancy)) {
  df_life_expectancy[[col_name]][df_life_expectancy[[col_name]] == "NA"] <- NA
}

# Step 2: Count missing values per column
missing_counts <- colSums(is.na(df_life_expectancy))

# Step 3: Calculate percentage of missing values per column
missing_percentage <- (missing_counts / nrow(df_life_expectancy)) * 100

# Step 4: Create a summary table for missing values
missing_summary <- data.frame(
  Column = names(df_life_expectancy),
  Missing_Count = missing_counts,
  Missing_Percentage = missing_percentage
)

# Print the summary table
print(missing_summary)

# Step 5: Identify columns with more than 30% missing values
columns_to_remove <- missing_summary$Column[missing_summary$Missing_Percentage > 30]

# Step 6: Remove these columns from the dataset
df_life_expectancy_filtered <- df_life_expectancy[, !names(df_life_expectancy) %in% columns_to_remove]

# Step 7: List of countries to remove
countries_to_remove <- c("ASM", "BMU", "COK", "DMA", "GRL", "KNA", "MCO", 
                         "MHL", "NIU", "NRU", "PLW", "PYF", "SMR", "TKL", "TUV")

# Step 8: Remove rows corresponding to the specified countries
df_life_expectancy_filtered_cleaned <- df_life_expectancy_filtered[!df_life_expectancy_filtered$country %in% countries_to_remove, ]

# Step 9: Remove rows with blank (NA or empty) values in the 'life_expectancy_at_birth_SEX_BTSX' column
df_life_expectancy_filtered_cleaned <- df_life_expectancy_filtered_cleaned %>%
  filter(!is.na(life_expectancy_at_birth_SEX_BTSX) & life_expectancy_at_birth_SEX_BTSX != "")

# Step 10: Check and List the Actual Column Names in the Dataset
print(colnames(df_life_expectancy_filtered_cleaned))

# Write the cleaned dataset to an Excel file
write_xlsx(df_life_expectancy_filtered_cleaned, "C://Users//wisen//Downloads//check_na.xlsx")

# List of columns to convert to numeric
columns_to_convert <- c("life_expectancy_at_birth_SEX_MLE", "life_expectancy_at_birth_SEX_BTSX", 
                        "bcg_immunization_coverage", "dtp_immunization_coverage", "year",
                        "hepb3_immunization_coverage", "hib3_immunization_coverage", 
                        "mcv1_immunization_coverage", "pol3_immunization_coverage", 
                        "che_%_of_gdp", "ext_%_of_che", "gghe-d_%_of_che", 
                        "oop_%_of_che", "pvt-d_%_of_che", "alc_consump_per_capita_liters_SEX_MLE", 
                        "alc_consump_per_capita_liters_SEX_FMLE", "alc_consump_per_capita_liters_SEX_BTSX", 
                        "pregnant_women_anaemia_prevalence_SEX_FMLE", 
                        "pregnant_women_anaemia_prevalence_SEVERITY_MODERATE", 
                        "pregnant_women_anaemia_prevalence_SEVERITY_MILD", 
                        "pregnant_women_anaemia_prevalence_SEVERITY_TOTAL", 
                        "pregnant_women_anaemia_prevalence_SEVERITY_SEVERE", 
                        "pregnant_women_mean_hemoglobin", "children_anaemia_prevalence_SEX_BTSX", 
                        "children_anaemia_prevalence_SEVERITY_MILD", 
                        "children_anaemia_prevalence_SEVERITY_MODERATE", 
                        "children_anaemia_prevalence_SEVERITY_TOTAL", 
                        "children_anaemia_prevalence_SEVERITY_SEVERE", "children_mean_hemoglobin", 
                        "thin_children_prevalence_SEX_FMLE_AGEGROUP_YEARS05-09", 
                        "thin_children_prevalence_SEX_BTSX_AGEGROUP_YEARS05-19", 
                        "thin_children_prevalence_SEX_MLE_AGEGROUP_YEARS05-19", 
                        "thin_children_prevalence_SEX_FMLE_AGEGROUP_YEARS05-19", 
                        "thin_children_prevalence_SEX_MLE_AGEGROUP_YEARS05-09", 
                        "thin_children_prevalence_SEX_MLE_AGEGROUP_YEARS10-19", 
                        "thin_children_prevalence_SEX_BTSX_AGEGROUP_YEARS10-19", 
                        "thin_children_prevalence_SEX_FMLE_AGEGROUP_YEARS10-19", 
                        "thin_children_prevalence_SEX_BTSX_AGEGROUP_YEARS05-09", 
                        "overweight_children_prevalence_SEX_MLE_AGEGROUP_YEARS05-09", 
                        "overweight_children_prevalence_SEX_FMLE_AGEGROUP_YEARS10-19", 
                        "overweight_children_prevalence_SEX_MLE_AGEGROUP_YEARS05-19", 
                        "overweight_children_prevalence_SEX_MLE_AGEGROUP_YEARS10-19", 
                        "overweight_children_prevalence_SEX_BTSX_AGEGROUP_YEARS05-19", 
                        "overweight_children_prevalence_SEX_FMLE_AGEGROUP_YEARS05-09", 
                        "overweight_children_prevalence_SEX_BTSX_AGEGROUP_YEARS05-09", 
                        "overweight_children_prevalence_SEX_FMLE_AGEGROUP_YEARS05-19", 
                        "overweight_children_prevalence_SEX_BTSX_AGEGROUP_YEARS10-19", 
                        "obese_children_prevalence_SEX_BTSX_AGEGROUP_YEARS05-09", 
                        "obese_children_prevalence_SEX_FMLE_AGEGROUP_YEARS05-19", 
                        "obese_children_prevalence_SEX_MLE_AGEGROUP_YEARS05-09", 
                        "obese_children_prevalence_SEX_FMLE_AGEGROUP_YEARS05-09", 
                        "obese_children_prevalence_SEX_FMLE_AGEGROUP_YEARS10-19", 
                        "obese_children_prevalence_SEX_BTSX_AGEGROUP_YEARS05-19", 
                        "obese_children_prevalence_SEX_MLE_AGEGROUP_YEARS05-19", 
                        "obese_children_prevalence_SEX_MLE_AGEGROUP_YEARS10-19", 
                        "obese_children_prevalence_SEX_BTSX_AGEGROUP_YEARS10-19", 
                        "underweight_adults_prevalence_SEX_FMLE", 
                        "underweight_adults_prevalence_SEX_BTSX", "underweight_adults_prevalence_SEX_MLE", 
                        "overweight_adults_prevalence_SEX_BTSX", "overweight_adults_prevalence_SEX_MLE", 
                        "overweight_adults_prevalence_SEX_FMLE", "obese_adults_prevalence_SEX_BTSX", 
                        "obese_adults_prevalence_SEX_MLE", "obese_adults_prevalence_SEX_FMLE")

# Convert only the selected columns to numeric
df_life_expectancy_filtered_cleaned[columns_to_convert] <- 
  lapply(df_life_expectancy_filtered_cleaned[columns_to_convert], 
         function(x) as.numeric(as.character(x)))

# Verify conversion
str(df_life_expectancy_filtered_cleaned)

colnames(df_life_expectancy_filtered_cleaned) <- gsub("%", "percent", colnames(df_life_expectancy_filtered_cleaned))
colnames(df_life_expectancy_filtered_cleaned) <- gsub("-", "_", colnames(df_life_expectancy_filtered_cleaned))

# Check the new column names to make sure they are cleaned up
colnames(df_life_expectancy_filtered_cleaned)


# List of selected columns
selected_columns <- c("country", "year", "life_expectancy_at_birth_SEX_BTSX",
                      "bcg_immunization_coverage", "dtp_immunization_coverage", 
                      "hepb3_immunization_coverage", "hib3_immunization_coverage",
                      "mcv1_immunization_coverage", "pol3_immunization_coverage", 
                      "che_percent_of_gdp", "ext_percent_of_che", "gghe_d_percent_of_che",
                      "oop_percent_of_che", "pvt_d_percent_of_che", 
                      "alc_consump_per_capita_liters_SEX_BTSX", 
                      "pregnant_women_anaemia_prevalence_SEVERITY_TOTAL", 
                      "pregnant_women_mean_hemoglobin", "children_anaemia_prevalence_SEVERITY_TOTAL", 
                      "children_mean_hemoglobin", 
                      "thin_children_prevalence_SEX_BTSX_AGEGROUP_YEARS05_19", 
                      "thin_children_prevalence_SEX_BTSX_AGEGROUP_YEARS10_19", 
                      "thin_children_prevalence_SEX_BTSX_AGEGROUP_YEARS05_09", 
                      "overweight_children_prevalence_SEX_BTSX_AGEGROUP_YEARS05_19", 
                      "overweight_children_prevalence_SEX_BTSX_AGEGROUP_YEARS05_09", 
                      "overweight_children_prevalence_SEX_BTSX_AGEGROUP_YEARS10_19", 
                      "obese_children_prevalence_SEX_BTSX_AGEGROUP_YEARS05_09", 
                      "obese_children_prevalence_SEX_BTSX_AGEGROUP_YEARS05_19", 
                      "obese_children_prevalence_SEX_BTSX_AGEGROUP_YEARS10_19", 
                      "underweight_adults_prevalence_SEX_BTSX", 
                      "overweight_adults_prevalence_SEX_BTSX", "obese_adults_prevalence_SEX_BTSX")

# Select columns and clean names
df_selected <- df_life_expectancy_filtered_cleaned[, selected_columns]
colnames(df_selected) <- gsub("\\s+", "_", gsub("-", "_", colnames(df_selected)))

# Convert 'year' to integer and 'country' to factor for efficiency
df_selected$year <- as.integer(df_selected$year)
df_selected$country <- as.factor(df_selected$country)

# Save to Excel
write_xlsx(df_selected, "C://Users//wisen//Downloads//selected_columns_optimized_data.xlsx")

# Check the structure
str(df_selected)


# Perform imputation using random forests (rf) for missing values
imputed_df <- mice(df_selected, m = 1, method = "rf", seed = 123)

# Check the imputed data (completed data after imputation)
completed_data <- complete(imputed_df, 1)  # Retrieve the first imputed dataset

# View the structure of the completed (imputed) data
str(completed_data)

write.csv(completed_data,"complete_check.csv")


# Step 1: Remove non-numeric columns ('country' and 'year')
df_numeric <- completed_data %>%
  select(-country, -year)  # Exclude 'country' and 'year'

# Step 2: Calculate the correlation matrix for numeric columns
cor_matrix <- cor(df_numeric, use = "pairwise.complete.obs")  # Handle missing data by pairwise deletion

# Step 3: Convert the correlation matrix to a data frame for easy export
cor_matrix_df <- as.data.frame(cor_matrix)

# Step 4: Write the correlation matrix to an Excel file
write.xlsx(cor_matrix_df, "C://Users//wisen//Downloads//correlation_matrix2.xlsx")



############# Linear Regression Model

lm_model <- lm(life_expectancy_at_birth_SEX_BTSX ~ ., data = df_numeric)

# Step 3: Summarize the linear model
summary(lm_model)


predictions <- predict(lm_model, newdata = df_numeric)
residuals <- df_numeric$life_expectancy_at_birth_SEX_BTSX - predictions
mse <- mean(residuals^2)
print(mse)

# Step 2: Get residuals
residuals <- lm_model$residuals

# Step 3: Get fitted values
fitted_values <- lm_model$fitted.values

# 1. Residuals vs Fitted Values Plot
ggplot(data = NULL, aes(x = fitted_values, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  ggtitle("Residuals vs Fitted Values") +
  xlab("Fitted Values") +
  ylab("Residuals")

# 2. Histogram of Residuals
ggplot(data = NULL, aes(x = residuals)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  ggtitle("Histogram of Residuals") +
  xlab("Residuals") +
  ylab("Frequency")

# 3. Q-Q Plot
qqnorm(residuals)
qqline(residuals, col = "red", lwd = 2)


# Step 4: Check for multicollinearity using VIF (Variance Inflation Factor)
# Install the 'car' package if not already installed (for VIF)
if (!require(car)) install.packages("car", dependencies = TRUE)

# Calculate VIF for the predictors
vif_values <- vif(lm_model)

# Step 5: View the VIF values
print(vif_values)

# Step 6: Identify predictors with high VIF values (typically > 5 or 10)
high_vif_predictors <- names(vif_values[vif_values > 5])

# Print the predictors with high VIF values
print(high_vif_predictors)



########## Random Forest Regression

set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(df_numeric$life_expectancy_at_birth_SEX_BTSX, p=0.8, list=FALSE)
train_data <- df_numeric[trainIndex, ]
test_data <- df_numeric[-trainIndex, ]

# --- Step 18: Train Random Forest model ---
rf_model2 <- randomForest(life_expectancy_at_birth_SEX_BTSX ~ ., data=train_data, importance=TRUE)

# --- Step 19: Print Random Forest model summary ---
print(rf_model2)

# --- Step 20: Predict on the test set ---
rf_predictions <- predict(rf_model2, newdata=test_data)

# --- Step 21: Evaluate model performance ---
mae <- mean(abs(rf_predictions - test_data$life_expectancy_at_birth_SEX_BTSX))
mse <- mean((rf_predictions - test_data$life_expectancy_at_birth_SEX_BTSX)^2)
rsq <- cor(rf_predictions, test_data$life_expectancy_at_birth_SEX_BTSX)^2

cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Mean Squared Error (MSE):", mse, "\n")
cat("R-squared:", rsq, "\n")

# --- Step 22: Display feature importance ---
print(rf_model2$importance)

# --- Step 23: Get Out-of-Bag (OOB) Mean Squared Error (MSE) ---
oob_error <- rf_model2$mse[rf_model2$ntree]
cat("Out-of-Bag (OOB) Mean Squared Error (MSE):", oob_error, "\n")

# --- Step 24: Visualize Feature Importance ---
png("C://Users//wisen//Documents//variable_importance_plot.png", width=2000, height=600)
varImpPlot(rf_model2)
dev.off()
