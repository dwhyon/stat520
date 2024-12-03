# Install and load necessary libraries (if not already installed)
install.packages(c("tidyverse", "readxl", "writexl", "VIM", "mice", "reshape2", "ggplot2"))
library(tidyverse)
library(readxl)
library(writexl)
library(VIM)      # For visualizing missing data
library(mice)      # For imputation
library(reshape2)  # For reshaping the correlation matrix
library(ggplot2)   # For visualization

# --- Step 1: Read the Life Expectancy Data ---
df_life_expectancy <- read_excel("C://Users//wisen//Documents//overall.xlsx")

# --- Step 2: Read the HIV Data ---
df_hiv <- read_csv("C://Users//wisen//Downloads//HIV data Who 15 - 49 years.csv")

# --- Step 3: Left join the datasets based on 'Country' and 'Year' ---
df_combined <- df_life_expectancy %>%
  left_join(df_hiv, by = c("Country", "Year"))

# --- Step 4: Filter data for years between 2000 and 2019 ---
df_combined_filtered <- df_combined %>%
  filter(Year >= 2000 & Year <= 2019)

# --- Step 5: Clean column names by removing non-alphanumeric characters ---
# Function to clean column names (removes non-alphanumeric characters)
clean_colnames <- function(names) {
  names %>%
    gsub("[^[:alnum:]_]", "", .)  # Remove anything that is not alphanumeric or underscore
}

# Apply the function to clean column names
colnames(df_combined_filtered) <- clean_colnames(colnames(df_combined_filtered))

# --- Step 6: Convert 'Country' and 'Year' to factors ---
df_combined_filtered$Country <- as.factor(df_combined_filtered$Country)
df_combined_filtered$Year <- as.factor(df_combined_filtered$Year)


column_names <- c("adi_grams_Both_sexes", "apc_liters", "dtp3_1yo_perc", "hepb3_1yo_perc", 
                  "hib3_1yo_perc", "hpv_9_14_yo_girls_perc", "mcv1_1_yo_perc", "mcv2_perc", 
                  "pab_perc", "pcv3_1_yo_perc", "pol3_1_yo_perc", "rotaC_1_yo_perc", 
                  "BCGimmunizationcoverageamong1yearolds", "GDP_percapita_USD", 
                  "LifeexpectancyatbirthyearsBothsexes", "Prevalence")

# Replace "NA" string with actual NA for all columns
for(col_name in column_names) {
  df_combined_filtered[[col_name]][df_combined_filtered[[col_name]] == "NA"] <- NA
}

# Check the summary of the dataframe to ensure replacement worked
summary(df_combined_filtered)

write_xlsx(df_combined_filtered, "C://Users//wisen//Documents//check123.xlsx")


# List of columns to convert from Character to Numeric
columns_to_convert <- c("hepb3_1yo_perc", "hib3_1yo_perc", "hpv_9_14_yo_girls_perc", 
                        "mcv1_1_yo_perc", "mcv2_perc", "pab_perc", "pcv3_1_yo_perc", 
                        "pol3_1_yo_perc", "rotaC_1_yo_perc", 
                        "BCGimmunizationcoverageamong1yearolds", "GDP_percapita_USD", 
                        "LifeexpectancyatbirthyearsBothsexes")

# Function to replace "NA" strings with actual NA and convert to numeric
convert_to_numeric <- function(x) {
  x <- as.character(x)  # Ensure it's a character vector
  x[x %in% c("NA", "N/A", "unknown", "NaN", "inf", "-inf", "")] <- NA  # Replace known non-numeric strings with NA
  return(as.numeric(x))  # Coerce to numeric, non-numeric strings become NA
}

# Apply the conversion to each specified column
for(col_name in columns_to_convert) {
  df_combined_filtered[[col_name]] <- convert_to_numeric(df_combined_filtered[[col_name]])
}


summary(df_combined_filtered)


colnames(df_combined_filtered)

# Check if the replacement was successful
summary(df_combined_filtered)

# --- Step 7: Check for missing values (NA) in the dataset ---
# View summary of missing data
summary(df_combined_filtered)  # Check for NA values

colnames (df_combined_filtered)



# --- Step 8: Imputation with mice (include 'Country' and 'Year') ---
# Perform imputation, considering both numeric and categorical columns
# The method 'rf' is used for random forest imputation (can handle mixed data types)
imputed_df <- mice(df_combined_filtered, m = 5, method = "rf", seed = 123)

# --- Step 9: Extract the completed data from the imputed object (2nd imputation set) ---
new_df_imputed <- complete(imputed_df, 2)

# --- Step 10: Ensure all columns are in the correct format ---
# After imputation, we want to ensure that 'Country' and 'Year' are factors and numeric columns remain numeric
new_df_imputed$Country <- as.factor(new_df_imputed$Country)
new_df_imputed$Year <- as.factor(new_df_imputed$Year)

# --- Step 11: View the structure and first few rows of the imputed dataset ---
str(new_df_imputed)  # Check the structure of the imputed dataset
head(new_df_imputed)  # View the first few rows

# --- Step 12: Check for missing data after imputation ---
# Check if any columns still have NA values after imputation
colSums(is.na(new_df_imputed))

# --- Step 13: Select only numeric columns (excluding 'Country' and 'Year') ---
# We can use select_if() to select only numeric columns
numeric_columns_imputed <- new_df_imputed %>%
  select_if(is.numeric)  # Select only numeric columns

# --- Step 14: Calculate the correlation matrix ---
correlation_matrix_imputed <- cor(numeric_columns_imputed, use = "pairwise.complete.obs", method = "pearson")

# --- Step 15: View the correlation matrix ---
print(correlation_matrix_imputed)

# --- Step 16: Visualize the correlation matrix with a heatmap ---
# Convert the correlation matrix to a long format for ggplot
correlation_melted <- melt(correlation_matrix_imputed)

# Plot the correlation matrix as a heatmap
ggplot(correlation_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Correlation Heatmap (Imputed Data)", x = "Variables", y = "Variables", fill = "Correlation")

# --- Step 17: If you want to save the imputed data to a new Excel file ---
write_xlsx(new_df_imputed, "C://Users//wisen//Documents/imputed_data_new.xlsx")


model <- lm(LifeexpectancyatbirthyearsBothsexes ~ . -Country -Year, data = new_df_imputed)

# --- Step 3: Check Model Summary ---
summary(model)



# Load the randomForest package
library(randomForest)

rf_model <- randomForest(LifeexpectancyatbirthyearsBothsexes ~ . -Country -Year, 
                         data = new_df_imputed, 
                         importance = TRUE, 
                         ntree = 500,  # Number of trees in the forest
                         mtry = sqrt(ncol(new_df_imputed) - 2),  # Number of variables to consider for each split (default: sqrt(number of predictors))
                         na.action = na.omit)  # Ensure that NAs are omitted during model fitting

# --- Step 4: View the Random Forest model summary ---
print(rf_model)

# Check variable importance
importance(rf_model)

# Plot variable importance
varImpPlot(rf_model)



