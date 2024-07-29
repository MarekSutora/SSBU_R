# Load required libraries
library(dplyr)
library(ggplot2)

theme_set(theme_bw())

data = read.csv("./Life-Expectancy-Data-Updated.csv")

summary(data)
        
str(data)

hist(data$GDP_per_capita)
# Replace "your_country" with the country you want to analyze
country <- "Italy"

# Filter data for the specific country and calculate the average life expectancy
average_life_expectancy <- data %>%
  filter(Country == country) %>%
  summarize(mean_life_expectancy = mean(Life_expectancy, na.rm = TRUE))

tapply(data$Life_expectancy, data$Region, mean)

# Print the result
cat("The average life expectancy for", country, "between 2000-2015 is:", average_life_expectancy$mean_life_expectancy)


theme_set(theme_bw())

ggplot(data,                    
       aes(x = Life_expectancy,   
           y = Schooling,      
           color = Region))+   
  geom_point()+                 
  labs(x = "Life Expectancy", 
       y = "Schooling")

# Remove non-numerical columns
data_numeric <- data %>%
select(-Country, -Region, -Economy_status_Developed, -Economy_status_Developing)
 
# Calculate the correlation matrix
correlation_matrix <- cor(data_numeric, use = "pairwise.complete.obs")

# Extract the correlations with life expectancy
life_expectancy_correlations <- correlation_matrix["Life_expectancy",]
 
# Sort correlations in descending order
sorted_correlations <- sort(life_expectancy_correlations, decreasing = TRUE)
 
# Print the correlations
print(sorted_correlations)





# Group data by year and calculate the average life expectancy
yearly_avg_life_expectancy <- data %>%
  group_by(Year) %>%
  summarize(avg_life_expectancy = mean(Life_expectancy, na.rm = TRUE))

# Fit a linear regression model
model <- lm(avg_life_expectancy ~ Year, data = yearly_avg_life_expectancy)

# Display model summary
summary(model)

# Create a data frame with prediction years (2016 to 2030)
prediction_years <- data.frame(Year = 2016:2030)

# Predict life expectancy for each year from 2016 to 2030
predicted_life_expectancy <- predict(model, newdata = prediction_years)

# Combine prediction years and life expectancy predictions
predictions <- cbind(prediction_years, Life_expectancy = predicted_life_expectancy)

# Print the predictions
print(predictions)

# Plot the average life expectancy for each year and the fitted regression line
ggplot(yearly_avg_life_expectancy, aes(x = Year, y = avg_life_expectancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Average Life Expectancy by Year",
       x = "Year",
       y = "Average Life Expectancy") +
  theme_minimal()





# Group data by year and region, and calculate the average life expectancy
yearly_avg_life_expectancy <- data %>%
  group_by(Year, Region) %>%
  summarize(avg_life_expectancy = mean(Life_expectancy, na.rm = TRUE))

# Plot the average life expectancy for each year and region with fitted regression lines
ggplot(yearly_avg_life_expectancy, aes(x = Year, y = avg_life_expectancy, color = Region)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Average Life Expectancy by Year and Region",
       x = "Year",
       y = "Average Life Expectancy") +
  theme_minimal() +
  facet_wrap(~ Region) +
  theme(legend.position = "bottom")





# Create a scatter plot of GDP per capita vs. schooling, colored by region
ggplot(data, aes(x = GDP_per_capita, y = Schooling, color = Region)) +
  geom_point() +
  labs(title = "GDP per Capita vs. Schooling",
       x = "GDP per Capita",
       y = "Schooling") +
  theme_minimal() +
  scale_x_log10() +
  theme(legend.position = "bottom")





# Calculate average life expectancy per country
avg_life_expectancy <- data %>%
  group_by(Country) %>%
  summarize(avg_life_expectancy = mean(Life_expectancy, na.rm = TRUE)) %>%
  arrange(avg_life_expectancy)

# Top 5 countries with the highest average life expectancy
top_5_max_life_expectancy <- tail(avg_life_expectancy, 5)

# Top 5 countries with the lowest average life expectancy
top_5_min_life_expectancy <- head(avg_life_expectancy, 5)

# Top 5 countries in alcohol consumption (by average over the years)
top_alcohol_consumption <- data %>%
  group_by(Country) %>%
  summarize(avg_alcohol_consumption = mean(Alcohol_consumption, na.rm = TRUE)) %>%
  arrange(desc(avg_alcohol_consumption)) %>%
  head(5)

# Countries with the highest average GDP per capita
top_gdp_per_capita <- data %>%
  group_by(Country) %>%
  summarize(avg_gdp_per_capita = mean(GDP_per_capita, na.rm = TRUE)) %>%
  arrange(desc(avg_gdp_per_capita)) %>%
  head(5)

# Top 5 countries with the highest average schooling years
top_schooling_years <- data %>%
  group_by(Country) %>%
  summarize(avg_schooling_years = mean(Schooling, na.rm = TRUE)) %>%
  arrange(desc(avg_schooling_years)) %>%
  head(5)

# Print the results
cat("Country with the highest average life expectancy:\n")
print(max_life_expectancy_country)
cat("\nCountry with the lowest average life expectancy:\n")
print(min_life_expectancy_country)
cat("\nTop 5 countries in alcohol consumption:\n")
print(top_alcohol_consumption)
cat("\nTop 5 countries with the highest average GDP per capita:\n")
print(top_gdp_per_capita)
cat("\nTop 5 countries with the highest average schooling years:\n")
print(top_schooling_years)
cat("Top 5 countries with the highest average life expectancy:\n")
print(top_5_max_life_expectancy)
cat("\nTop 5 countries with the lowest average life expectancy:\n")
print(top_5_min_life_expectancy)





# Calculate average BMI and GDP per capita per country
avg_bmi_gdp <- data %>%
  group_by(Country) %>%
  summarize(avg_BMI = mean(BMI, na.rm = TRUE),
            avg_GDP_per_capita = mean(GDP_per_capita, na.rm = TRUE))

# Function to calculate lower and upper bounds for outliers using IQR method
outlier_bounds <- function(x) {
  Q1 <- quantile(x, 0.4, na.rm = TRUE)
  Q3 <- quantile(x, 0.6, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(c(lower_bound, upper_bound))
}

bmi_bounds <- outlier_bounds(avg_bmi_gdp$avg_BMI)
gdp_bounds <- outlier_bounds(avg_bmi_gdp$avg_GDP_per_capita)

# Identify countries with high BMI and high GDP per capita
high_bmi_high_gdp <- avg_bmi_gdp[avg_bmi_gdp$avg_BMI > bmi_bounds[2] & avg_bmi_gdp$avg_GDP_per_capita > gdp_bounds[2],]

# Identify countries with low BMI and low GDP per capita
low_bmi_low_gdp <- avg_bmi_gdp[avg_bmi_gdp$avg_BMI < bmi_bounds[1] & avg_bmi_gdp$avg_GDP_per_capita < gdp_bounds[1],]

# Print the results
cat("Countries with high BMI and high GDP per capita:\n")
print(high_bmi_high_gdp)

cat("\nCountries with low BMI and low GDP per capita:\n")
print(low_bmi_low_gdp)






# Calculate average BMI and GDP per capita per country, and keep the Region
avg_bmi_gdp_region <- data %>%
  group_by(Country, Region) %>%
  summarize(avg_BMI = mean(BMI, na.rm = TRUE),
            avg_GDP_per_capita = mean(GDP_per_capita, na.rm = TRUE))

# Create scatter plot
ggplot(avg_bmi_gdp_region, aes(x = avg_GDP_per_capita, y = avg_BMI, color = Region)) +
  geom_point() +
  geom_text(aes(label = Country), check_overlap = TRUE, hjust = 1.1, vjust = 0.5, size = 3) +
  theme_minimal() +
  labs(title = "Average BMI vs. GDP per Capita by Region",
       x = "Average GDP per Capita",
       y = "Average BMI",
       color = "Region") +
  theme(legend.position = "bottom")









# Calculate average values for each country and keep the Region
avg_values <- data %>%
  group_by(Country, Region) %>%
  summarize(avg_GDP_per_capita = mean(GDP_per_capita, na.rm = TRUE),
            avg_Schooling = mean(Schooling, na.rm = TRUE),
            avg_Hepatitis_B = mean(Hepatitis_B, na.rm = TRUE))

# Scatter plot for Hepatitis B vs Schooling with colors by Region and country labels
ggplot(avg_values, aes(x = avg_Schooling, y = avg_Hepatitis_B, color = Region, label = Country)) +
  geom_point() +
  geom_text(aes(label = Country), vjust = 1, hjust = 1, size = 3) +
  theme_minimal() +
  labs(title = "Hepatitis B vs Schooling",
       x = "Average Schooling",
       y = "Average Hepatitis B",
       color = "Region") +
  theme(legend.position = "bottom")

# Scatter plot for Hepatitis B vs GDP per capita with colors by Region and country labels
ggplot(avg_values, aes(x = avg_GDP_per_capita, y = avg_Hepatitis_B, color = Region, label = Country)) +
  geom_point() +
  geom_text(aes(label = Country), vjust = 1, hjust = 1, size = 3) +
  theme_minimal() +
  labs(title = "Hepatitis B vs GDP per Capita",
       x = "Average GDP per Capita",
       y = "Average Hepatitis B",
       color = "Region") +
  theme(legend.position = "bottom")



avg_values <- data %>%
  group_by(Country, Region) %>%
  summarize(avg_GDP_per_capita = mean(GDP_per_capita, na.rm = TRUE),
            avg_Schooling = mean(Schooling, na.rm = TRUE),
            avg_Polio = mean(Polio, na.rm = TRUE))

# Scatter plot for Polio vs Schooling with colors by Region and country labels
ggplot(avg_values, aes(x = avg_Schooling, y = avg_Polio, color = Region, label = Country)) +
  geom_point() +
  geom_text(aes(label = Country), vjust = 1, hjust = 1, size = 3) +
  theme_minimal() +
  labs(title = "Polio vs Schooling",
       x = "Average Schooling",
       y = "Average Polio",
       color = "Region") +
  theme(legend.position = "bottom")

# Scatter plot for Polio vs GDP per capita with colors by Region and country labels
ggplot(avg_values, aes(x = avg_GDP_per_capita, y = avg_Polio, color = Region, label = Country)) +
  geom_point() +
  geom_text(aes(label = Country), vjust = 1, hjust = 1, size = 3) +
  theme_minimal() +
  labs(title = "Polio vs GDP per Capita",
       x = "Average GDP per Capita",
       y = "Average Polio",
       color = "Region") +
  theme(legend.position = "bottom")






# Calculate average values for each country and keep the Region
avg_values <- data %>%
  group_by(Country, Region) %>%
  summarize(avg_GDP_per_capita = mean(GDP_per_capita, na.rm = TRUE),
            avg_Schooling = mean(Schooling, na.rm = TRUE),
            avg_Incidents_HIV = mean(Incidents_HIV, na.rm = TRUE))

# Scatter plot for Incidents HIV vs Schooling with colors by Region
ggplot(avg_values, aes(x = avg_Schooling, y = avg_Incidents_HIV, color = Region)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Incidents HIV vs Schooling",
       x = "Average Schooling",
       y = "Average Incidents HIV",
       color = "Region") +
  theme(legend.position = "bottom")

# Scatter plot for Incidents HIV vs GDP per capita with colors by Region
ggplot(avg_values, aes(x = avg_GDP_per_capita, y = avg_Incidents_HIV, color = Region)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Incidents HIV vs GDP per Capita",
       x = "Average GDP per Capita",
       y = "Average Incidents HIV",
       color = "Region") +
  theme(legend.position = "bottom")





# Calculate average values for each country and keep the Region
avg_values <- data %>%
  group_by(Country, Region) %>%
  summarize(avg_GDP_per_capita = mean(GDP_per_capita, na.rm = TRUE),
            avg_Schooling = mean(Schooling, na.rm = TRUE),
            avg_Diphtheria = mean(Diphtheria, na.rm = TRUE))

# Scatter plot for Diphtheria vs Schooling with colors by Region and country labels
ggplot(avg_values, aes(x = avg_Schooling, y = avg_Diphtheria, color = Region, label = Country)) +
  geom_point() +
  geom_text(aes(label = Country), vjust = 1, hjust = 1, size = 3) +
  theme_minimal() +
  labs(title = "Diphtheria vs Schooling",
       x = "Average Schooling",
       y = "Average Diphtheria",
       color = "Region") +
  theme(legend.position = "bottom")

# Scatter plot for Diphtheria vs GDP per capita with colors by Region and country labels
ggplot(avg_values, aes(x = avg_GDP_per_capita, y = avg_Diphtheria, color = Region, label = Country)) +
  geom_point() +
  geom_text(aes(label = Country), vjust = 1, hjust = 1, size = 3) +
  theme_minimal() +
  labs(title = "Diphtheria vs GDP per Capita",
       x = "Average GDP per Capita",
       y = "Average Diphtheria",
       color = "Region") +
  theme(legend.position = "bottom")





# Calculate average values for each country and keep the Region
avg_values <- data %>%
  group_by(Country, Region) %>%
  summarize(avg_GDP_per_capita = mean(GDP_per_capita, na.rm = TRUE),
            avg_Schooling = mean(Schooling, na.rm = TRUE),
            avg_Polio = mean(Polio, na.rm = TRUE))

# Scatter plot for Polio vs Schooling with colors by Region and country labels
ggplot(avg_values, aes(x = avg_Schooling, y = avg_Polio, color = Region, label = Country)) +
  geom_point() +
  geom_text(aes(label = Country), vjust = 1, hjust = 1, size = 3) +
  theme_minimal() +
  labs(title = "Polio vs Schooling",
       x = "Average Schooling",
       y = "Average Polio",
       color = "Region") +
  theme(legend.position = "bottom")

# Scatter plot for Polio vs GDP per capita with colors by Region and country labels
ggplot(avg_values, aes(x = avg_GDP_per_capita, y = avg_Polio, color = Region, label = Country)) +
  geom_point() +
  geom_text(aes(label = Country), vjust = 1, hjust = 1, size = 3) +
  theme_minimal() +
  labs(title = "Polio vs GDP per Capita",
       x = "Average GDP per Capita",
       y = "Average Polio",
       color = "Region") +
  theme(legend.position = "bottom")