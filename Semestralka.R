# Load required libraries
library(dplyr)
library(ggplot2)

theme_set(theme_bw())

data = read.csv("./Life-Expectancy-Data-Updated.csv")

summary(data)
        
str(data)

tapply(data$Life_expectancy, data$Region, mean)



data_numeric <- data %>%
  select(-Country, -Region)

correlation_matrix <- cor(data_numeric, method = "pearson", use = "complete.obs")

life_expectancy_correlations <- correlation_matrix["Life_expectancy",]
 
sorted_correlations <- sort(life_expectancy_correlations, decreasing = TRUE)
 
print(sorted_correlations)





yearly_avg_life_expectancy <- data %>%
  group_by(Year) %>%
  summarize(avg_life_expectancy = mean(Life_expectancy, na.rm = TRUE))

model <- lm(avg_life_expectancy ~ Year, data = yearly_avg_life_expectancy)

summary(model)

prediction_years <- data.frame(Year = 2016:2030)
predicted_life_expectancy <- predict(model, newdata = prediction_years)
predictions <- cbind(prediction_years, Life_expectancy = predicted_life_expectancy)

print(predictions)

ggplot(yearly_avg_life_expectancy, aes(x = Year, y = avg_life_expectancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Average Life Expectancy by Year",
       x = "Year",
       y = "Average Life Expectancy") +
  theme_minimal()



yearly_avg_life_expectancy <- data %>%
  group_by(Year, Region) %>%
  summarize(avg_life_expectancy = mean(Life_expectancy, na.rm = TRUE))

ggplot(yearly_avg_life_expectancy, aes(x = Year, y = avg_life_expectancy, color = Region)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Average Life Expectancy by Year and Region",
       x = "Year",
       y = "Average Life Expectancy") +
  theme_minimal() +
  facet_wrap(~ Region) +
  theme(legend.position = "bottom")






avg_life_expectancy <- data %>%
  group_by(Country) %>%
  summarize(avg_life_expectancy = mean(Life_expectancy, na.rm = TRUE)) %>%
  arrange(avg_life_expectancy)

top_5_max_life_expectancy <- tail(avg_life_expectancy, 5)

top_5_min_life_expectancy <- head(avg_life_expectancy, 5)

top_alcohol_consumption <- data %>%
  group_by(Country) %>%
  summarize(avg_alcohol_consumption = mean(Alcohol_consumption, na.rm = TRUE)) %>%
  arrange(desc(avg_alcohol_consumption)) %>%
  head(5)

top_gdp_per_capita <- data %>%
  group_by(Country) %>%
  summarize(avg_gdp_per_capita = mean(GDP_per_capita, na.rm = TRUE)) %>%
  arrange(desc(avg_gdp_per_capita)) %>%
  head(5)

top_schooling_years <- data %>%
  group_by(Country) %>%
  summarize(avg_schooling_years = mean(Schooling, na.rm = TRUE)) %>%
  arrange(desc(avg_schooling_years)) %>%
  head(5)

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





avg_values <- data %>%
  group_by(Country, Region) %>%
  summarize(avg_GDP_per_capita = mean(GDP_per_capita, na.rm = TRUE),
            avg_Schooling = mean(Schooling, na.rm = TRUE),
            avg_BMI = mean(BMI, na.rm = TRUE),
            avg_Hepatitis_B = mean(Hepatitis_B, na.rm = TRUE),
            avg_Polio = mean(Polio, na.rm = TRUE),
            avg_Incidents_HIV = mean(Incidents_HIV, na.rm = TRUE),
            avg_Diphtheria = mean(Diphtheria, na.rm = TRUE),
            avg_Life_expectancy = mean(Life_expectancy, na.rm = TRUE))

ggplot(avg_values, aes(x = avg_GDP_per_capita, y = avg_BMI, color = Region)) +
  geom_point() +
  geom_text(aes(label = Country), check_overlap = TRUE, hjust = 1.1, vjust = 0.5, size = 3) +
  theme_minimal() +
  labs(title = "Average BMI vs. Average GDP per Capita by Region",
       x = "Average GDP per Capita",
       y = "Average BMI",
       color = "Region") +
  theme(legend.position = "bottom")


ggplot(avg_values, aes(x = avg_GDP_per_capita, y = avg_Life_expectancy, color = Region)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Average Life Expectancy vs Average GDP per Capita",
       x = "Average GDP per Capita",
       y = "Average Life Expectancy",
       color = "Region") +
  theme(legend.position = "bottom")

ggplot(avg_values, aes(x = avg_Schooling, y = avg_Life_expectancy, color = Region)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Average Life Expectancy vs Average Schooling",
       x = "Average Schooling",
       y = "Average Life Expectancy",
       color = "Region") +
  theme(legend.position = "bottom")

ggplot(avg_values, aes(x = avg_Schooling, y = avg_Hepatitis_B, color = Region, label = Country)) +
  geom_point() +
  geom_text(aes(label = Country), vjust = 1, hjust = 1, size = 3) +
  theme_minimal() +
  labs(title = "Average Hepatitis B vs Average Schooling",
       x = "Average Schooling",
       y = "Average Hepatitis B",
       color = "Region") +
  theme(legend.position = "bottom")

ggplot(avg_values, aes(x = avg_GDP_per_capita, y = avg_Hepatitis_B, color = Region, label = Country)) +
  geom_point() +
  geom_text(aes(label = Country), vjust = 1, hjust = 1, size = 3) +
  theme_minimal() +
  labs(title = "Average Hepatitis B vs Average GDP per Capita",
       x = "Average GDP per Capita",
       y = "Average Hepatitis B",
       color = "Region") +
  theme(legend.position = "bottom")

ggplot(avg_values, aes(x = avg_Schooling, y = avg_Polio, color = Region)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Average Polio vs Average Schooling",
       x = "Average Schooling",
       y = "Average Polio",
       color = "Region") +
  theme(legend.position = "bottom")

ggplot(avg_values, aes(x = avg_GDP_per_capita, y = avg_Polio, color = Region, label = Country)) +
  geom_point() +
  geom_text(aes(label = Country), vjust = 1, hjust = 1, size = 3) +
  theme_minimal() +
  labs(title = "Average Polio vs Average GDP per Capita",
       x = "Average GDP per Capita",
       y = "Average Polio",
       color = "Region") +
  theme(legend.position = "bottom")

ggplot(avg_values, aes(x = avg_Schooling, y = avg_Incidents_HIV, color = Region, label = Country)) +
  geom_point() +
  geom_text(aes(label = Country), vjust = 1, hjust = 1, size = 3) +
  theme_minimal() +
  labs(title = "Average Incidents HIV Average vs Schooling",
       x = "Average Schooling",
       y = "Average Incidents HIV",
       color = "Region") +
  theme(legend.position = "bottom")

ggplot(avg_values, aes(x = avg_GDP_per_capita, y = avg_Incidents_HIV, color = Region, label = Country)) +
  geom_point() +
  geom_text(aes(label = Country), vjust = 1, hjust = 1, size = 3) +
  theme_minimal() +
  labs(title = "Average Incidents HIV vs Average GDP per Capita",
       x = "Average GDP per Capita",
       y = "Average Incidents HIV",
       color = "Region") +
  theme(legend.position = "bottom")

ggplot(avg_values, aes(x = avg_Schooling, y = avg_Diphtheria, color = Region)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Average Diphtheria vs Average Schooling",
       x = "Average Schooling",
       y = "Average Diphtheria",
       color = "Region") +
  theme(legend.position = "bottom")

ggplot(avg_values, aes(x = avg_GDP_per_capita, y = avg_Diphtheria, color = Region, label = Country)) +
  geom_point() +
  geom_text(aes(label = Country), vjust = 1, hjust = 1, size = 3) +
  theme_minimal() +
  labs(title = "Average Diphtheria vs Average GDP per Capita",
       x = "Average GDP per Capita",
       y = "Average Diphtheria",
       color = "Region") +
  theme(legend.position = "bottom")

ggplot(data,                    
       aes(x = Life_expectancy,   
           y = Schooling,      
           color = Region))+   
  geom_point()+                 
  labs(x = "Life Expectancy", 
       y = "Schooling") +
  theme(legend.position = "bottom")
