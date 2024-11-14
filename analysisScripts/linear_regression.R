library(tidyverse)
library(readxl)
library(VIM)
library(mice)
library(car)


df <- read_xlsx("./Life_Expectancy_Data.xlsx")
head(df ,10)


# Export to Excel
#write.xlsx(df, file = "C://Users//wisen//Downloads//file2.xlsx")

# notes not related to the data analyzed 
# We can use the mann- whitney u test to analyze data that is highly skewed
# This looks at the median (Non Parametic test)  
# this is because the median is resistant to outliers 


head(df,10)
colSums(is.na(df))

summary(df)
##### Check for missing values in the data 
colSums(is.na(df))

#### checking using the mice to check N/A and where they exist 
md.pattern(df)

##### Using library to see a representation of mising data per column
#### the more the missing data more than 5% of the total rows or data 
### we might rethink of removing it 

aggr_plot <- aggr(df, col=c('blue','red'),
                  numbers= TRUE,
                  sortVars = TRUE,
                  labels = names(df),
                  cex.axis=.7,
                  gap = 3, 
                  ylab= c("Histogram of missing data","Pattern")
)

############## Checking on the methods available for mice
methods(mice)

# Replace spaces and special characters in column names
colnames(df) <- gsub(" ", "_", colnames(df))    # Replace spaces with underscores
colnames(df) <- gsub("/", "_", colnames(df))    # Replace slashes with underscores
colnames(df) <- gsub("-", "_", colnames(df))    # Replace hyphens with underscores, if needed

print(colnames(df))

summary(df)
##### Through background check the most mice used methods are 
####mice.impute.pmm  This looks at the continous varibles and usually if data is not normally distributed 
####mice.impute.rf This leverages on Random forest imputation and used for both categorical and continuos imputations . usually looking at non linear relationshops

# Perform the imputation
imputed_df <- mice(df, m = 5, method = "rf", seed = 123)


summary(imputed_df)

####### Checking on imputed data from the categories that were missing the mosts like population , GDP , Alchohol
imputed_df$imp$Population
imputed_df$imp$Alcohol
imputed_df$imp$GDP

##### We can select any column from the 5 columnns that have been placed out 
new_df <- complete(imputed_df,2) %>% 
  mutate(
    thinness_1_19_years = thinness__1_19_years,
    .keep = "unused"
  )

#### Check a quick dimension rows vs columns 
dim(new_df)
##### Check for missing data in the new_df

colSums(is.na(new_df))

######
selected_df <- new_df %>% 
  select(c("Life_expectancy", "Adult_Mortality", "infant_deaths", "Alcohol", 
           "percentage_expenditure", "Hepatitis_B", "Measles", "BMI", 
           "under_five_deaths", "Polio", "Total_expenditure", "Diphtheria",
           "HIV_AIDS", "GDP", "Population", "thinness_1_19_years",
           "thinness_5_9_years", "Income_composition_of_resources", "Schooling"))

correlation_matrix <- cor(selected_df)
correlation_matrix


####### with no life expectancy since its the dependentant variable 
### this selection is to check multicolinelity 
selected_df2 <- new_df %>% 
  select(c("Adult_Mortality", "infant_deaths", "Alcohol", 
           "percentage_expenditure", "Hepatitis_B", "Measles", "BMI", 
           "under_five_deaths", "Polio", "Total_expenditure", "Diphtheria", 
           "HIV_AIDS", "GDP", "Population", "thinness_1_19_years",
           "thinness_5_9_years", "Income_composition_of_resources", "Schooling"))
###### 
###Let me check for Multicollinearity:
##VIF = 1: No correlation among predictors. This is ideal.
###1 < VIF < 5: Moderate correlation; generally considered acceptable.
###VIF > 5: Indicates potential multicollinearity issues. Some practitioners use a threshold of 10 for serious concerns.


vif(lm(Life_expectancy ~ ., data = selected_df))


###### Visualizing the data dependent variables vs the independent variables 

####Life expectancy vs Income composition of resources 

ggplot (new_df , aes( x= Income_composition_of_resources , y = Life_expectancy , colour = 'red'  )) + 
  geom_point() +
  geom_smooth(method = lm , se = FALSE)

ggplot (new_df , aes (x = Adult_Mortality , y= Life_expectancy , colour ='blue'))+ 
  geom_point()+ 
  geom_smooth(method = lm , se = FALSE)


model <- lm(Life_expectancy ~ Country + Hepatitis_B + GDP + Total_expenditure + 
              Alcohol + Income_composition_of_resources + Schooling + BMI + 
              thinness_1_19_years + Polio + Diphtheria + Adult_Mortality + 
              Measles + under_five_deaths + HIV_AIDS,
            data = new_df)

summary(model)


# get list of residuals  
res <- resid(model) 
res 

# produce residual vs. fitted plot 
plot(fitted(model), res) 

# add a horizontal line at 0  
abline(0,0) 

plot(density(res))



# Filter the dataframe for the United States and summarize the Population
#summary(new_df$Population[new_df$Country == 'United States of America'])



