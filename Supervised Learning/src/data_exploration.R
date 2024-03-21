library(tidyverse)
library(skimr)
library(here)

data <- read.csv(here("Supervised Learning/data", "Life_Expectancy_Data.csv"))
skimr::skim_without_charts(data)

#we don't need to investigate country as a variable 

#cleaning
#region should be coded into a numeric variable


# Convert the categorical variable to a factor
data$Region <- factor(data$Region)
# Code the factor variable as numeric
data$region_numeric <- as.numeric(data$Region)

# Display the result
print(data$region_numeric)

# These are the numeric codes assigned to each rregion
unique_categories <- unique(data$Region)
numeric_codes <- as.numeric(unique_categories)
category_codes <- data.frame(category = unique_categories, numeric_code = numeric_codes)
print(category_codes)


#drop the region column
data <- subset(data, select = -Region)
data_2014 <- data %>% filter(Year == 2014) %>% select(-Year)
numeric_data_2014 <- data_2014 %>% select(-Country)

#=============================================================================================================
#distribution: looking at the distribution of each variable
# Plot the distribution of the variable using base R
hist((data_2014 %>% filter(Year == 2015) %>% select(percentage.expenditure)), col = "skyblue", border = "black", main = "Distribution of Variable", ylim=c(0,250))
data %>% filter(percentage.expenditure > 1000 & Year == 2015) %>% select(Country) %>% unique() %>% count()

data %>% filter(Year == 2014 & percentage.expenditure > 0) %>% select(percentage.expenditure) %>% count()
str(data)
skim_without_charts(data_2014)    


# Install and load the corrplot package
install.packages("corrplot")
library(corrplot)

# Compute the correlation matrix
correlation_matrix <- cor(numeric_data_2014, use = "pairwise.complete.obs")

# Plot the correlation matrix
corrplot(correlation_matrix, method = "circle", type = "lower", order = "hclust", tl.col = "black", tl.srt = 45)

file_path <- file.path(here("Supervised Learning/data"), "correlation_matrix.csv")

# Save the correlation matrix to a CSV file
write.csv(correlation_matrix, file = file_path, row.names = TRUE)

#Plotting correlation between dependent variable and each independent variable


# Calculate correlation coefficients between dependent variable and each independent variable
correlation_with_dependent <- sapply(numeric_data_2014[, -which(names(numeric_data_2014) == "Life_expectancy")], function(x) cor(x, numeric_data_2014[["Life_expectancy"]]))

# Combine correlation coefficients into a matrix
cor_matrix <- matrix(correlation_with_dependent, nrow = 1)

# Plot correlations for all independent variables in a single bar chart
barplot(cor_matrix, 
        beside = TRUE, 
        col = rainbow(ncol(cor_matrix)), 
        main = "Correlation of Independent Variables with Dependent Variable",
        xlab = "Independent Variables", 
        ylab = "Correlation Coefficient",
        names.arg = names(cor_matrix))
correlation_with_dependent
cor_matrix

numeric_data_2014[, -which(names(numeric_data_2014) == "Life_expectancy")]
subset(numeric_data_2014, select = -Life_expectancy)
numeric_data_2014["Life_expectancy"]

y <- numeric_data_2014[["Life_expectancy"]]
x <- subset(numeric_data_2014, select = -Life_expectancy)
scaled_x <- data.frame(scale(x))
#run linear regression
lm_model <- lm(y ~ ., data = x)
scaled_model <- lm(y ~ ., data = scaled_x)
#try with sclaed variables and see if there's a difference
# Print summary statistics
summary(lm_model)
summary(scaled_model)
