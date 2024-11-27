# Install and load necessary libraries (if not already installed)
install.packages(c("tidyverse", "readxl", "writexl", "VIM", "mice", "reshape2", "ggplot2", "randomForest", "caret"))
library(tidyverse)
library(readxl)
library(writexl)
library(VIM)      # For visualizing missing data
library(mice)      # For imputation
library(reshape2)  # For reshaping the correlation matrix
library(ggplot2)   # For visualization
library(randomForest)
library(caret)

# --- Step 1: Read the Life Expectancy Data ---
df_life_expectancy <- read_csv("C://Users//wisen//OneDrive//Desktop//master.csv")

# --- Step 2: Filter data for years between 2000 and 2019 ---
df_life_expectancy <- df_life_expectancy %>%
  filter(Year >= 2000 & Year <= 2019)

# --- Step 3: Clean column names by removing non-alphanumeric characters ---
clean_colnames <- function(names) {
  names %>%
    gsub("[^[:alnum:]_]", "", .)
}

colnames(df_life_expectancy) <- clean_colnames(colnames(df_life_expectancy))
colnames(df_life_expectancy)

# --- Step 4: Replace "NA" string with actual NA for all columns ---
for(col_name in colnames(df_life_expectancy)) {
  df_life_expectancy[[col_name]][df_life_expectancy[[col_name]] == "NA"] <- NA
}

# --- Step 5: Check the summary of the dataframe to ensure replacement worked ---
summary(df_life_expectancy)


# --- Step 6: List of countries to remove ---
countries_to_remove <- c(
  "Puerto Rico", "South Sudan", "Monaco", "Montenegro", "San Marino", 
  "Palau", "Marshall Islands", "Somalia", "occupied Palestinian territory, including east Jerusalem", 
  "Serbia", "Democratic People's Republic of Korea", "Cook Islands", "Andorra", 
  "Nauru", "Dominica", "Tuvalu", "Vanuatu", "Equatorial Guinea", "Antigua and Barbuda", 
  "Timor-Leste", "Niue", "Grenada", "China", "Nigeria", "Solomon Islands", 
  "Saint Kitts and Nevis", "Guinea", "Chad"
)


# --- Step 7: Remove rows containing the specified countries ---
df <- df_life_expectancy %>%
  filter(!Country %in% countries_to_remove)

# --- Step 8: Verify the changes ---
summary(df)

# --- Step 9: Save the cleaned data to an Excel file ---
write_xlsx(df, "countries_cleaned_data.xlsx")

# --- Step 10: Check for missing data ---
colSums(is.na(df))

# --- Step 11: Visualize missing data ---
md.pattern(df)

# Use VIM for missing data visualization
png("C://Users//wisen//Documents//missing_values2.png", width = 1500, height = 800)
aggr_plot <- aggr(df, col=c('blue','red'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(df), cex.axis=0.7, gap=3, 
                  ylab=c("Histogram of missing data", "Pattern"))
dev.off()

# --- Step 12: Impute missing data ---
imputed_df <- mice(df, m=5, method="rf", seed=123)
new_df <- complete(imputed_df, 3)

# --- Step 13: Check for missing data in imputed data ---
colSums(is.na(new_df))

# --- Step 14: Extract numeric columns ---
numeric_columns <- new_df %>%
  select(where(is.numeric))

# --- Step 15: Create histograms for each numeric column ---
for(col_name in colnames(numeric_columns)) {
  p <- ggplot(new_df, aes_string(x=col_name)) +
    geom_histogram(binwidth=1, fill="blue", color="black", alpha=0.7) +
    theme_minimal() +
    labs(title=paste("Histogram of", col_name), x=col_name, y="Frequency")
  print(p)
}

# --- Step 16: Select columns for modeling ---
df_selected <- new_df %>%
  select(LifeexpectancyatbirthyearsBothsexes,
         apc_liters, dtp3_1yo_perc, hepb3_1yo_perc, hib3_1yo_perc, 
         mcv1_1_yo_perc, mcv2_perc, pab_perc, pol3_1_yo_perc, 
        BCGimmunizationcoverageamong1yearolds, GDP_percapita_USD, 
         unemployment, hiv_perc)

# --- Step 17: Train-test split for Random Forest model ---
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(df_selected$LifeexpectancyatbirthyearsBothsexes, p=0.8, list=FALSE)
train_data <- df_selected[trainIndex, ]
test_data <- df_selected[-trainIndex, ]

# --- Step 18: Train Random Forest model ---
rf_model2 <- randomForest(LifeexpectancyatbirthyearsBothsexes ~ ., data=train_data, importance=TRUE)

# --- Step 19: Print Random Forest model summary ---
print(rf_model2)

# --- Step 20: Predict on the test set ---
rf_predictions <- predict(rf_model2, newdata=test_data)

# --- Step 21: Evaluate model performance ---
mae <- mean(abs(rf_predictions - test_data$LifeexpectancyatbirthyearsBothsexes))
mse <- mean((rf_predictions - test_data$LifeexpectancyatbirthyearsBothsexes)^2)
rsq <- cor(rf_predictions, test_data$LifeexpectancyatbirthyearsBothsexes)^2

cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Mean Squared Error (MSE):", mse, "\n")
cat("R-squared:", rsq, "\n")

# --- Step 22: Display feature importance ---
print(rf_model2$importance)

# --- Step 23: Get Out-of-Bag (OOB) Mean Squared Error (MSE) ---
oob_error <- rf_model2$mse[rf_model2$ntree]
cat("Out-of-Bag (OOB) Mean Squared Error (MSE):", oob_error, "\n")

# --- Step 24: Visualize Feature Importance ---
png("C://Users//wisen//Documents//variable_importance_plot.png", width=800, height=600)
varImpPlot(rf_model2)
dev.off()

# --- Step 25: Make predictions on new input data ---
new_data_input <- data.frame(
  apc_liters = 20.5,                    
  dtp3_1yo_perc = 95.0,                
  hepb3_1yo_perc = 90.0,                
  hib3_1yo_perc = 85.0,                 
  hpv_9_14_yo_girls_perc = 70.0,        
  mcv1_1_yo_perc = 92.0,                
  mcv2_perc = 90.0,                     
  pab_perc = 98.0,                      
  pcv3_1_yo_perc = 87.0,                
  pol3_1_yo_perc = 95.0,                
  rotaC_1_yo_perc = 88.0,               
  BCGimmunizationcoverageamong1yearolds = 93.0,
  GDP_percapita_USD = 5000,             
  unemployment = 7.5,                   
  hiv_perc = 1.2                        
)

# Ensure the new data frame matches the structure of the training data
new_data_input <- as.data.frame(new_data_input)

# Predict Life Expectancy using the trained Random Forest model
predicted_life_expectancy <- predict(rf_model2, newdata=new_data_input)
cat("Predicted Life Expectancy:", predicted_life_expectancy, "\n")





