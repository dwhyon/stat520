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

# -----------------------------------
# Step 1: Read the Life Expectancy Data
# -----------------------------------
df_life_expectancy <- read_csv("C://Users//wisen//Downloads//WHO_numeric_data.csv")

# Step 2: Clean the data by replacing 'NA' string with actual NA
df_life_expectancy[df_life_expectancy == "NA"] <- NA

# -----------------------------------
# Step 3: Check for Missing Values
# -----------------------------------
missing_counts <- colSums(is.na(df_life_expectancy))
missing_percentage <- (missing_counts / nrow(df_life_expectancy)) * 100

# Step 4: Create a summary table for missing data
missing_summary <- data.frame(
  Column = names(df_life_expectancy),
  Missing_Count = missing_counts,
  Missing_Percentage = missing_percentage
)
print(missing_summary)

# Step 5: Remove columns with more than 30% missing values
columns_to_remove <- missing_summary$Column[missing_summary$Missing_Percentage > 30]
df_life_expectancy_filtered <- df_life_expectancy[, !names(df_life_expectancy) %in% columns_to_remove]

# -----------------------------------
# Step 6: Remove Specific Countries
# -----------------------------------
countries_to_remove <- c("ASM", "BMU", "COK", "DMA", "GRL", "KNA", "MCO", 
                         "MHL", "NIU", "NRU", "PLW", "PYF", "SMR", "TKL", "TUV")
df_life_expectancy_filtered_cleaned <- df_life_expectancy_filtered[!df_life_expectancy_filtered$country %in% countries_to_remove, ]

# -----------------------------------
# Step 7: Remove rows with blank (NA or empty) values in specific column
# -----------------------------------
df_life_expectancy_filtered_cleaned <- df_life_expectancy_filtered_cleaned %>%
  filter(!is.na(life_expectancy_at_birth_SEX_BTSX) & life_expectancy_at_birth_SEX_BTSX != "")

# -----------------------------------
# Step 8: Check and Clean Column Names
# -----------------------------------
colnames(df_life_expectancy_filtered_cleaned) <- gsub("%", "percent", colnames(df_life_expectancy_filtered_cleaned))
colnames(df_life_expectancy_filtered_cleaned) <- gsub("-", "_", colnames(df_life_expectancy_filtered_cleaned))

# -----------------------------------
# Step 9: Convert Columns to Numeric
# -----------------------------------
columns_to_convert <- c("life_expectancy_at_birth_SEX_MLE", "life_expectancy_at_birth_SEX_BTSX", 
                        "bcg_immunization_coverage", "dtp_immunization_coverage", "year",
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
                        "overweight_children_prevalence_SEX_BTSX_AGEGROUP_YEARS05_19", 
                        "obese_children_prevalence_SEX_BTSX_AGEGROUP_YEARS05_09", 
                        "underweight_adults_prevalence_SEX_BTSX")
df_life_expectancy_filtered_cleaned[columns_to_convert] <- 
  lapply(df_life_expectancy_filtered_cleaned[columns_to_convert], 
         function(x) as.numeric(as.character(x)))

# -----------------------------------
# Step 10: Select Relevant Columns
# -----------------------------------
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
                      "overweight_children_prevalence_SEX_BTSX_AGEGROUP_YEARS05_19", 
                      "obese_children_prevalence_SEX_BTSX_AGEGROUP_YEARS05_09", 
                      "underweight_adults_prevalence_SEX_BTSX")
df_selected <- df_life_expectancy_filtered_cleaned[, selected_columns]

# -----------------------------------
# Step 11: Convert 'year' and 'country' columns
# -----------------------------------
df_selected$year <- as.integer(df_selected$year)
df_selected$country <- as.factor(df_selected$country)

# Save cleaned and selected data
write_xlsx(df_selected, "C://Users//wisen//Downloads//selected_columns_optimized_data.xlsx")

# -----------------------------------
# Step 12: Imputation of Missing Data using Random Forest
# -----------------------------------
imputed_df <- mice(df_selected, m = 1, method = "rf", seed = 123)
completed_data <- complete(imputed_df, 1)

# Save completed data
write.csv(completed_data, "complete_check.csv")

# -----------------------------------
# Step 13: Calculate and Save Correlation Matrix
# -----------------------------------
df_numeric <- completed_data %>%
  select(-country, -year)

cor_matrix <- cor(df_numeric, use = "pairwise.complete.obs")
cor_matrix_df <- as.data.frame(cor_matrix)

write.xlsx(cor_matrix_df, "C://Users//wisen//Downloads//correlation_matrix2.xlsx")

# -----------------------------------
# Step 14: Linear Regression Model
# -----------------------------------
lm_model <- lm(life_expectancy_at_birth_SEX_BTSX ~ ., data = df_numeric)

# Summarize the linear model
summary(lm_model)

# Predictions and residuals
predictions <- predict(lm_model, newdata = df_numeric)
residuals <- df_numeric$life_expectancy_at_birth_SEX_BTSX - predictions
mse <- mean(residuals^2)
cat("MSE:", mse, "\n")

# Visualizations for residuals
ggplot(data = NULL, aes(x = lm_model$fitted.values, y = residuals)) +
  geom_point() + geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  ggtitle("Residuals vs Fitted Values") + xlab("Fitted Values") + ylab("Residuals")

ggplot(data = NULL, aes(x = residuals)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  ggtitle("Histogram of Residuals") + xlab("Residuals") + ylab("Frequency")

# Q-Q plot
qqnorm(residuals)
qqline(residuals, col = "red", lwd = 2)

# Check for multicollinearity using VIF
if (!require(car)) install.packages("car", dependencies = TRUE)
vif_values <- vif(lm_model)
high_vif_predictors <- names(vif_values[vif_values > 5])
print(high_vif_predictors)

# -----------------------------------
# Step 15: Random Forest Regression
# -----------------------------------
set.seed(123)
trainIndex <- createDataPartition(df_numeric$life_expectancy_at_birth_SEX_BTSX, p = 0.8, list = FALSE)
train_data <- df_numeric[trainIndex, ]
test_data <- df_numeric[-trainIndex, ]

rf_model2 <- randomForest(life_expectancy_at_birth_SEX_BTSX ~ ., data = train_data, importance = TRUE)

# Evaluate model performance
rf_predictions <- predict(rf_model2, newdata = test_data)
mae <- mean(abs(rf_predictions - test_data$life_expectancy_at_birth_SEX_BTSX))
mse <- mean((rf_predictions - test_data$life_expectancy_at_birth_SEX_BTSX)^2)
rsq <- cor(rf_predictions, test_data$life_expectancy_at_birth_SEX_BTSX)^2

cat("Random Forest Model Performance: MAE =", mae, ", MSE =", mse, ", R-squared =", rsq, "\n")

# Feature importance
importance(rf_model2)
png("C://Users//wisen//Documents//variable_importance_plot1.png", width = 2000, height = 600)
varImpPlot(rf_model2)
dev.off()

# -----------------------------------
# Step 16: Saving the Random Forest Model
# -----------------------------------
save(rf_model2, file = "random_forest_model.rda")
