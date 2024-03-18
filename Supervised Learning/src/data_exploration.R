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

#=============================================================================================================
#distribution: looking at the distribution of each variable
