# Install and load necessary libraries (if not already installed)
install.packages(c("tidyverse", "readxl", "writexl"))
library(tidyverse)
library(readxl)
library(writexl)

# --- Step 1: Read the Life Expectancy Data ---
# Read the Life Expectancy data
df <- read_excel("C://Users//wisen//Downloads//Life_Expectancy_Data.xlsx")
head(df, 10)

# --- Step 2: Read the Population Statistics Data ---
# Read the Population Statistics data
population_stats <- read_csv("C://Users//wisen//Documents//population_statistics.csv")
head(population_stats)

# --- Step 3: Reshape the Population Data into Long Format ---
population_stats_long <- population_stats %>%
  pivot_longer(cols = starts_with("20"),
               names_to = "Year",
               values_to = "Population")

population_stats_long$Year <- as.numeric(population_stats_long$Year)
head(population_stats_long)

# --- Step 4: Rename Columns in Population Data to Match ---
population_stats_long <- population_stats_long %>%
  rename(Country = "Country Name")

head(population_stats_long)

# --- Step 5: Replace Country Names in Population Data Using `case_when` ---
population_stats_long <- population_stats_long %>%
  mutate(Country = case_when(
    Country == "Bolivia" ~ "Bolivia (Plurinational State of)",
    Country == "Bahamas, The" ~ "Bahamas",
    Country == "Gambia, The" ~ "Gambia",
    Country == "Cote d'Ivoire" ~ "CÃ´te d'Ivoire",
    Country == "Congo, Rep." ~ "Congo",
    Country == "Korea, Rep." ~ "Democratic People's Republic of Korea",
    Country == "Congo, Dem. Rep." ~ "Democratic Republic of the Congo",
    Country == "Egypt, Arab Rep." ~ "Egypt",
    Country == "Iran, Islamic Rep." ~ "Iran (Islamic Republic of)",
    Country == "Kyrgyz Republic" ~ "Kyrgyzstan",
    Country == "Lao PDR" ~ "Lao People's Democratic Republic",
    Country == "Micronesia, Fed. Sts." ~ "Micronesia (Federated States of)",
    Country == "Korea, Dem. People's Rep." ~ "Republic of Korea",
    Country == "Moldova" ~ "Republic of Moldova",
    Country == "Slovak Republic" ~ "Slovakia",
    Country == "Eswatini" ~ "Swaziland",
    Country == "North Macedonia" ~ "The former Yugoslav republic of Macedonia",
    Country == "Turkiye" ~ "Turkey",
    Country == "United Kingdom" ~ "United Kingdom of Great Britain and Northern Ireland",
    Country == "Tanzania" ~ "United Republic of Tanzania",
    Country == "United States" ~ "United States of America",
    Country == "Venezuela, RB" ~ "Venezuela (Bolivarian Republic of)",
    Country == "Yemen, Rep." ~ "Yemen",
    TRUE ~ Country
  ))

head(population_stats_long)

# --- Step 6: Merge the Datasets (df and Population Statistics) ---
merged_data <- df %>%
  left_join(population_stats_long, by = c("Country", "Year"))

head(merged_data, 10)

# --- Step 7: Read the Death Rate Data ---
# Read the Death Rate data from CSV
death_rate_data <- read_csv("C://Users//wisen//Downloads//death_rate.csv")

# Inspect the first few rows of the death rate dataset
head(death_rate_data)

# --- Step 8: Reshape the Death Rate Data into Long Format ---
death_rate_data_long <- death_rate_data %>%
  pivot_longer(cols = starts_with("20"),
               names_to = "Year",
               values_to = "Death_Rate")

# Convert the 'Year' column to numeric to ensure easy merging
death_rate_data_long$Year <- as.numeric(death_rate_data_long$Year)

# --- Step 9: Rename Columns in Death Rate Data to Match ---
death_rate_data_long <- death_rate_data_long %>%
  rename(Country = "Country Name")

# Inspect the reshaped death rate data
head(death_rate_data_long)

# --- Step 10: Replace Country Names in Death Rate Data Using `case_when` ---
death_rate_data_long <- death_rate_data_long %>%
  mutate(Country = case_when(
    Country == "Bolivia" ~ "Bolivia (Plurinational State of)",
    Country == "Bahamas, The" ~ "Bahamas",
    Country == "Gambia, The" ~ "Gambia",
    Country == "Cote d'Ivoire" ~ "CÃ´te d'Ivoire",
    Country == "Congo, Rep." ~ "Congo",
    Country == "Korea, Rep." ~ "Democratic People's Republic of Korea",
    Country == "Congo, Dem. Rep." ~ "Democratic Republic of the Congo",
    Country == "Egypt, Arab Rep." ~ "Egypt",
    Country == "Iran, Islamic Rep." ~ "Iran (Islamic Republic of)",
    Country == "Kyrgyz Republic" ~ "Kyrgyzstan",
    Country == "Lao PDR" ~ "Lao People's Democratic Republic",
    Country == "Micronesia, Fed. Sts." ~ "Micronesia (Federated States of)",
    Country == "Korea, Dem. People's Rep." ~ "Republic of Korea",
    Country == "Moldova" ~ "Republic of Moldova",
    Country == "Slovak Republic" ~ "Slovakia",
    Country == "Eswatini" ~ "Swaziland",
    Country == "North Macedonia" ~ "The former Yugoslav republic of Macedonia",
    Country == "Turkiye" ~ "Turkey",
    Country == "United Kingdom" ~ "United Kingdom of Great Britain and Northern Ireland",
    Country == "Tanzania" ~ "United Republic of Tanzania",
    Country == "United States" ~ "United States of America",
    Country == "Venezuela, RB" ~ "Venezuela (Bolivarian Republic of)",
    Country == "Yemen, Rep." ~ "Yemen",
    TRUE ~ Country
  ))

# Inspect the updated death rate data
head(death_rate_data_long)

# --- Step 11: Merge Death Rate Data with the Existing Merged Data ---
merged_data_with_death_rate <- merged_data %>%
  left_join(death_rate_data_long, by = c("Country", "Year"))

# Inspect the merged data with death rate
head(merged_data_with_death_rate)

# --- Step 12: Read the GDP Data ---
# Read the GDP statistics data from CSV
gdp_stats <- read_csv("C://Users//wisen//Documents//GDP_Statistics.csv")

# Inspect the first few rows of the GDP dataset
head(gdp_stats)

# --- Step 13: Reshape the GDP Data into Long Format ---
gdp_stats_long <- gdp_stats %>%
  pivot_longer(cols = starts_with("20"),
               names_to = "Year",
               values_to = "GDP")

# Convert the 'Year' column to numeric to ensure easy merging
gdp_stats_long$Year <- as.numeric(gdp_stats_long$Year)

# --- Step 14: Rename Columns in GDP Data to Match ---
gdp_stats_long <- gdp_stats_long %>%
  rename(Country = "Country Name")

# Inspect the reshaped GDP data
head(gdp_stats_long)

# --- Step 15: Replace Country Names in GDP Data Using `case_when` ---
gdp_stats_long <- gdp_stats_long %>%
  mutate(Country = case_when(
    Country == "Bolivia" ~ "Bolivia (Plurinational State of)",
    Country == "Bahamas, The" ~ "Bahamas",
    Country == "Gambia, The" ~ "Gambia",
    Country == "Cote d'Ivoire" ~ "CÃ´te d'Ivoire",
    Country == "Congo, Rep." ~ "Congo",
    Country == "Korea, Rep." ~ "Democratic People's Republic of Korea",
    Country == "Congo, Dem. Rep." ~ "Democratic Republic of the Congo",
    Country == "Egypt, Arab Rep." ~ "Egypt",
    Country == "Iran, Islamic Rep." ~ "Iran (Islamic Republic of)",
    Country == "Kyrgyz Republic" ~ "Kyrgyzstan",
    Country == "Lao PDR" ~ "Lao People's Democratic Republic",
    Country == "Micronesia, Fed. Sts." ~ "Micronesia (Federated States of)",
    Country == "Korea, Dem. People's Rep." ~ "Republic of Korea",
    Country == "Moldova" ~ "Republic of Moldova",
    Country == "Slovak Republic" ~ "Slovakia",
    Country == "Eswatini" ~ "Swaziland",
    Country == "North Macedonia" ~ "The former Yugoslav republic of Macedonia",
    Country == "Turkiye" ~ "Turkey",
    Country == "United Kingdom" ~ "United Kingdom of Great Britain and Northern Ireland",
    Country == "Tanzania" ~ "United Republic of Tanzania",
    Country == "United States" ~ "United States of America",
    Country == "Venezuela, RB" ~ "Venezuela (Bolivarian Republic of)",
    Country == "Yemen, Rep." ~ "Yemen",
    TRUE ~ Country
  ))

# Inspect the updated GDP data
head(gdp_stats_long)

# --- Step 16: Merge GDP Data with the Existing Merged Data ---
final_merged_data <- merged_data_with_death_rate %>%
  left_join(gdp_stats_long, by = c("Country", "Year"))

# Inspect the final merged data with GDP
head(final_merged_data)

# --- Step 17: Read the Expenditure per Capita Data ---
# Read the Expenditure per Capita data from CSV
expenditure_per_capita <- read_csv("C://Users//wisen//Documents//Expediture_per_capita.csv")

# Inspect the first few rows of the expenditure per capita dataset
head(expenditure_per_capita)

# --- Step 18: Reshape the Expenditure Data into Long Format ---
expenditure_per_capita_long <- expenditure_per_capita %>%
  pivot_longer(cols = starts_with("20"),
               names_to = "Year",
               values_to = "Expenditure_Per_Capita")

# Convert the 'Year' column to numeric to ensure easy merging
expenditure_per_capita_long$Year <- as.numeric(expenditure_per_capita_long$Year)

# --- Step 19: Rename Columns in Expenditure Data to Match ---
expenditure_per_capita_long <- expenditure_per_capita_long %>%
  rename(Country = "Country Name")

# Inspect the reshaped expenditure data
head(expenditure_per_capita_long)

# --- Step 20: Replace Country Names in Expenditure Data Using `case_when` ---
expenditure_per_capita_long <- expenditure_per_capita_long %>%
  mutate(Country = case_when(
    Country == "Bolivia" ~ "Bolivia (Plurinational State of)",
    Country == "Bahamas, The" ~ "Bahamas",
    Country == "Gambia, The" ~ "Gambia",
    Country == "Cote d'Ivoire" ~ "CÃ´te d'Ivoire",
    Country == "Congo, Rep." ~ "Congo",
    Country == "Korea, Rep." ~ "Democratic People's Republic of Korea",
    Country == "Congo, Dem. Rep." ~ "Democratic Republic of the Congo",
    Country == "Egypt, Arab Rep." ~ "Egypt",
    Country == "Iran, Islamic Rep." ~ "Iran (Islamic Republic of)",
    Country == "Kyrgyz Republic" ~ "Kyrgyzstan",
    Country == "Lao PDR" ~ "Lao People's Democratic Republic",
    Country == "Micronesia, Fed. Sts." ~ "Micronesia (Federated States of)",
    Country == "Korea, Dem. People's Rep." ~ "Republic of Korea",
    Country == "Moldova" ~ "Republic of Moldova",
    Country == "Slovak Republic" ~ "Slovakia",
    Country == "Eswatini" ~ "Swaziland",
    Country == "North Macedonia" ~ "The former Yugoslav republic of Macedonia",
    Country == "Turkiye" ~ "Turkey",
    Country == "United Kingdom" ~ "United Kingdom of Great Britain and Northern Ireland",
    Country == "Tanzania" ~ "United Republic of Tanzania",
    Country == "United States" ~ "United States of America",
    Country == "Venezuela, RB" ~ "Venezuela (Bolivarian Republic of)",
    Country == "Yemen, Rep." ~ "Yemen",
    TRUE ~ Country
  ))

# Inspect the updated expenditure data
head(expenditure_per_capita_long)

# --- Step 21: Merge Expenditure Data with the Existing Merged Data ---
final_merged_data <- final_merged_data %>%
  left_join(expenditure_per_capita_long, by = c("Country", "Year"))

# Inspect the final merged data with expenditure data
head(final_merged_data)

# --- Step 22: Read the Expectancy World Bank Data ---
# Read the Expectancy World Bank data from CSV
expectancy_world_bank <- read_csv("C://Users//wisen//Documents//expectancy_world_bank_download.csv")

# Inspect the first few rows of the expectancy world bank dataset
head(expectancy_world_bank)

# --- Step 23: Reshape the Expectancy Data into Long Format ---
expectancy_world_bank_long <- expectancy_world_bank %>%
  pivot_longer(cols = starts_with("20"),
               names_to = "Year",
               values_to = "Life_Expectancy_WB")

# Convert the 'Year' column to numeric to ensure easy merging
expectancy_world_bank_long$Year <- as.numeric(expectancy_world_bank_long$Year)

# --- Step 24: Rename Columns in Expectancy Data to Match ---
expectancy_world_bank_long <- expectancy_world_bank_long %>%
  rename(Country = "Country Name")

# Inspect the reshaped expectancy world bank data
head(expectancy_world_bank_long)

# --- Step 25: Replace Country Names in Expectancy World Bank Data Using `case_when` ---
expectancy_world_bank_long <- expectancy_world_bank_long %>%
  mutate(Country = case_when(
    Country == "Bolivia" ~ "Bolivia (Plurinational State of)",
    Country == "Bahamas, The" ~ "Bahamas",
    Country == "Gambia, The" ~ "Gambia",
    Country == "Cote d'Ivoire" ~ "CÃ´te d'Ivoire",
    Country == "Congo, Rep." ~ "Congo",
    Country == "Korea, Rep." ~ "Democratic People's Republic of Korea",
    Country == "Congo, Dem. Rep." ~ "Democratic Republic of the Congo",
    Country == "Egypt, Arab Rep." ~ "Egypt",
    Country == "Iran, Islamic Rep." ~ "Iran (Islamic Republic of)",
    Country == "Kyrgyz Republic" ~ "Kyrgyzstan",
    Country == "Lao PDR" ~ "Lao People's Democratic Republic",
    Country == "Micronesia, Fed. Sts." ~ "Micronesia (Federated States of)",
    Country == "Korea, Dem. People's Rep." ~ "Republic of Korea",
    Country == "Moldova" ~ "Republic of Moldova",
    Country == "Slovak Republic" ~ "Slovakia",
    Country == "Eswatini" ~ "Swaziland",
    Country == "North Macedonia" ~ "The former Yugoslav republic of Macedonia",
    Country == "Turkiye" ~ "Turkey",
    Country == "United Kingdom" ~ "United Kingdom of Great Britain and Northern Ireland",
    Country == "Tanzania" ~ "United Republic of Tanzania",
    Country == "United States" ~ "United States of America",
    Country == "Venezuela, RB" ~ "Venezuela (Bolivarian Republic of)",
    Country == "Yemen, Rep." ~ "Yemen",
    TRUE ~ Country
  ))

# Inspect the updated expectancy world bank data
head(expectancy_world_bank_long)

# --- Step 26: Merge Expectancy World Bank Data with the Existing Merged Data ---
final_merged_data <- final_merged_data %>%
  left_join(expectancy_world_bank_long, by = c("Country", "Year"))

# Inspect the final merged data with expectancy world bank data
head(final_merged_data)

# --- Step 27: Save the Final Merged Data ---
write_csv(final_merged_data, "C://Users//wisen//Documents//final_merged_with_all_data.csv")
write_xlsx(final_merged_data, "C://Users//wisen//Documents//final_merged_with_all_data.xlsx")

# Replace spaces and special characters in column names
colnames(final_merged_data) <- gsub(" ", "_", colnames(final_merged_data))
colnames(final_merged_data) <- gsub("/", "_", colnames(final_merged_data))
colnames(final_merged_data) <- gsub("-", "_", colnames(final_merged_data))


# Select relevant columns for analysis
selected_df <- final_merged_data %>%
  select(Life_Expectancy_WB, Alcohol,
         percentage_expenditure, Hepatitis_B, Measles, BMI,Death_Rate,
         under_five_deaths, Polio, Total_expenditure, Diphtheria,
         HIV_AIDS, thinness__1_19_years,
         thinness_5_9_years, Income_composition_of_resources, Schooling,Population.y,GDP.y,Expenditure_Per_Capita)



# Impute missing values using mice


# Check dimensions and for missing values
dim(new_df)
colSums(is.na(new_df))imputed_df <- mice(selected_df, m = 5, method = "rf", seed = 123)

# Complete the data using the second imputed dataset
new_df <- complete(imputed_df, 2)

# Create scatter plots for 'Life_expectancy' vs other columns
column_names <- colnames(new_df)
column_names

write.csv(new_df, "C://Users//wisen//Documents//imputed_data.csv", row.names = FALSE)

# Fit linear model
model <- lm(Life_Expectancy_WB ~ Hepatitis_B +
              Alcohol + Income_composition_of_resources + Schooling + BMI +
              thinness__1_19_years + Polio + Diphtheria  +
              Measles + HIV_AIDS + Population.y + GDP.y + Expenditure_Per_Capita,
            data = new_df)

summary(model)


# Assuming 'new_df' is your dataset with the merged data
# We will first identify all the numeric columns
numeric_columns <- new_df %>%
  select(where(is.numeric)) %>%
  colnames()

# Create histograms for each numeric column
plot_list <- lapply(numeric_columns, function(col) {
  ggplot(new_df, aes_string(x = col)) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
    theme_minimal() +
    labs(title = paste("Histogram of", col), x = col, y = "Frequency") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
})

# Print all the plots
for (plot in plot_list) {
  print(plot)
}

# Residual analysis
res <- resid(model)

# Residual vs Fitted plot
plot(fitted(model), res)
abline(h = 0, col = "red")
plot(density(res))

# Get predicted values from the model
predicted_values <- predict(model)

# Get residuals (differences between actual and predicted values)
residuals <- model$residuals

# Plot residuals vs fitted values (Residuals vs Fitted Plot)
plot(model, which = 1)  # This is the Residuals vs Fitted plot



#### covert life_expectancy into a log transformation 

new_df$log_life_Expectancy <- log(new_df$Life_Expectancy_WB)

colnames(new_df)

# Plot the histogram
ggplot(new_df, aes(x = log_life_Expectancy)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = paste("Histogram of", colnames(new_df)[which(colnames(new_df) == "log_life_Expectancy")]), 
    x = "Log of Life Expectancy", 
    y = "Frequency"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Load the required package
library(MASS)

# Specify the lambda value
lambda_value <- 1.5

# Apply the Box-Cox transformation manually to 'Life_Expectancy_WB' with lambda = 1.5
new_df$boxcox_life_expectancy <- ifelse(
  new_df$Life_Expectancy_WB > 0,  # Ensure positive values for the transformation
  (new_df$Life_Expectancy_WB^lambda_value - 1) / lambda_value,  # Box-Cox for lambda != 0
  log(new_df$Life_Expectancy_WB)  # Box-Cox for lambda = 0 (log transformation)
)

# Check the first few transformed values
head(new_df$boxcox_life_expectancy)

# Optionally, plot a histogram of the transformed data
ggplot(new_df, aes(x = boxcox_life_expectancy)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Box-Cox Transformation (lambda = 1.5) of Life Expectancy", x = "Box-Cox Transformed Life Expectancy", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Step 1: Find the highest value in Life_Expectancy_WB
highest_life_expectancy_WB <- max(new_df$Life_Expectancy_WB, na.rm = TRUE)

# Step 2: Create the new column using the formula
new_df$transformed_life_expectancy <- 1 + highest_life_expectancy_WB - new_df$Life_Expectancy_WB

# Step 3: Apply the log transformation to the new column
new_df$log_transformed_life_expectancy <- log(new_df$transformed_life_expectancy)

# Step 4: Check the transformation by plotting the log-transformed values
ggplot(new_df, aes(x = log_transformed_life_expectancy)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Log Transformation of Inverse Life Expectancy", 
       x = "Log of Transformed Life Expectancy", 
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

