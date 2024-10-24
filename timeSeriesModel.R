library(tidyverse)
library(readxl)
library(mice)
library(forecast)
library(VIM)
library(car)

df <- read_xlsx("./Life_Expectancy_Data.xlsx")
  
head(df ,10)

##### Summary Statistics of the 
summary(df)


#### Columns with missing data 
colSums(is.na(df))

##### 
##round - rounding off to remove decimals , nrow(df)/ Number of rows in the data 
missing_percentage_per_country <- round (colSums(is.na(df)) / nrow(df) * 100)
missing_percentage_per_country 

#####Univariate analysis per columns 

table(df$Country)
#### All countries have 16 entries except Dominca which has 1

##### Life expectancy 
hist(df$`Life expectancy`)
##### data on the graph for this is skewed to the left 


# Identify continuous variables
continuous_vars <- sapply(df, is.numeric)
head(continuous_vars)

##### Histogram analysis for each column that has continous data 
for (var in names(df)) {
  if (is.numeric(df[[var]])) {
    hist(df[[var]], 
         main = paste("Histogram of", var), 
         xlab = var, 
         col = "blue", 
         border = "black",
         breaks = 10)  # You can adjust the number of breaks
  }
}

####### Average Life Expectancy per Country 
avg_life_expectancy <- data.frame(
  Country = unique(df$Country),
  AverageLifeExpectancy = tapply(df$`Life expectancy`, df$Country, mean, na.rm = TRUE)
)

###### Order from least country with 
avg_life_expectancy <- avg_life_expectancy[order(avg_life_expectancy$AverageLifeExpectancy), ]
avg_life_expectancy
### Country with the most life expectancy is Japan while Sierra leone has the least life expentancy 

######## Looking at tht histogram analysis of
ggplot(df, aes(x=`Life expectancy`,fill=factor(Status))) + 
  geom_histogram(binwidth=1, alpha=0.5, position = 'identity') +
  labs(title  = "Distribution of Life Expectancy Per Status of Development",
       fill = "Group")+
  scale_fill_discrete(labels = c("Developed", "Developing"))

##### Box plot Status Grouped by Life Expectancy 
ggplot(df, aes(x=`Life expectancy`, y = factor(Status), fill = factor(Status))) +
  geom_boxplot() +
  labs(title= "Distribution of Life Expectancy Per Status of Development",
       fill = "Group",
       y =" ") +
  scale_fill_discrete(labels = c("Developed", "Developing"))


##### Average Life Expectancy per Year and  Status 
avg_life_expectancy_perStatusandcountry <- aggregate(`Life expectancy` ~ Year + Status, 
                                                     data = df, 
                                                     FUN = mean, 
                                                     na.rm = TRUE)

avg_life_expectancy_perStatusandcountry


ggplot(avg_life_expectancy_perStatusandcountry, aes(x = Year, y = `Life expectancy`, color = Status)) +
  geom_line(size = 1) +  # Line plot
  geom_point(size = 2) +  # Points on the line
  labs(title = "Average Life Expectancy (2000 - 2015)",
       x = "Year",
       y = "Average Life Expectancy",
       color = "Status") +
  theme_minimal() +
  scale_color_manual(values = c("Developed" = "blue", "Developing" = "orange")) # Custom colors

######Run a timeseries data  

developed_ts <- ts(avg_life_expectancy_perStatusandcountry$`Life expectancy`[avg_life_expectancy_perStatusandcountry$Status == "Developed"],
                   start = 2000, frequency = 1)

developing_ts <- ts(avg_life_expectancy_perStatusandcountry$`Life expectancy`[avg_life_expectancy_perStatusandcountry$Status == "Developing"],
                    start = 2000, frequency = 1)


# Fit an ARIMA model for Developed countries
developed_fit <- auto.arima(developed_ts)

# Fit an ARIMA model for Developing countries
developing_fit <- auto.arima(developing_ts)

# Check residuals for Developed countries model
checkresiduals(developed_fit)
### since the p value >0.05 .... p Value = 0.56 it indicates that there is no autocorrelation

# Check residuals for Developing countries model
checkresiduals(developing_fit)
### since the p value >0.05 ...Pvalue  0.84 it indicates that there is no autocorrelation

# Forecast the next 5 years (2016-2020)
developed_forecast <- forecast(developed_fit, h = 5)
developing_forecast <- forecast(developing_fit, h = 5)

developed_forecast

developing_forecast



############ Regression Analysis for the Life expectancy 


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
colnames(df) <- gsub("\\.", "_", colnames(df)) 


##### Through background check the most mice used methods are 
####mice.impute.pmm  This looks at the continous varibles and usually if data is not normally distributed 
####mice.impute.rf This leverages on Random forest imputation and used for both categorical and continuos imputations . usually looking at non linear relationshops

imputed_df <- mice(df , m=5 , method = "rf" )
summary(imputed_df)

####### Checking on imputed data from the categories that were missing the mosts like population , GDP , Alchohol
imputed_df$imp$Population
imputed_df$imp$Alcohol
imputed_df$imp$GDP

##### We can select any column from the 5 columnns that have been placed out 
new_df <- complete(imputed_df,2) 

#### Check a quick dimension rows vs columns 
dim(new_df)
##### Check for missing data in the new_df

colSums(is.na(new_df))




######
selected_df <- new_df %>% 
  select(c("Hepatitis_B", "GDP", "Total_expenditure", "Alcohol",
           "Income_composition_of_resources", "Schooling", "BMI",
           "thinness__1_19_years", "thinness_5_9_years", "Polio", "Diphtheria",
           "Life_expectancy", "Adult_Mortality", "infant_deaths", 
           "percentage_expenditure", "Measles", "under_five_deaths", 
           "HIV_AIDS"))

correlation_matrix <- cor(selected_df)
correlation_matrix


####### with no life expectancy since its the dependentant variable 
### this selection is to check multicolinelity 
selected_df2 <- new_df %>% 
  select(c("Hepatitis_B", "GDP", "Total_expenditure", "Alcohol",
           "Income_composition_of_resources", "Schooling", "BMI",
           "thinness__1_19_years", "thinness_5_9_years", "Polio", "Diphtheria",
           "Adult_Mortality", "infant_deaths", 
           "percentage_expenditure", "Measles", "under_five_deaths", 
           "HIV_AIDS"))



###### 
###Let me check for Multicollinearity: 

vif(lm(Life_expectancy ~ ., data = selected_df))


###### Visualizing the data dependent variables vs the independent variables 

####Life expectancy vs Income composition of resources 

ggplot ( new_df , aes( x= Income_composition_of_resources , y = Life_expectancy , colour = 'red'  )) + 
  geom_point() +
  geom_smooth(method = lm , se = FALSE)

boxplot(new_df$Income_composition_of_resources)
#### no outliers seen in the data and it has a linear relationship 

##### check for skewness in data 
#describe(new_df)

#### do a histogram for skewneed data , population , 

model <- lm(Life_expectancy ~ Country + Hepatitis_B +GDP +Total_expenditure +Alcohol +Income_composition_of_resources +Schooling +BMI +thinness__1_19_years+Polio +Diphtheria +Adult_Mortality  +Measles +under_five_deaths +HIV_AIDS,
            data = new_df)
summary(model)
