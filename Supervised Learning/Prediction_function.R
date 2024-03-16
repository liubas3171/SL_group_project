
library(readr)  # For reading CSV files
library(caret)  # For data preprocessing and model training


data <- read.csv("Life_Expectancy_Data.csv")
data <- subset(data, Life_expectancy != 0) #removed the rows where life expectancy was zero which didn't make any sense

X <- data[, c('GDP_per_capita', 'Total.expenditure', 'Schooling', "Alcohol_consumption")]
y <- data$Life_expectancy

model <- train(X, y, method = "lm")


y_pred <- predict(model, X)
mae <- mae(y, y_pred)
mse <- mse(y, y_pred)
r_squared <- R2(y, y_pred)

new_data <- data.frame('GDP_per_capita' = 45000, 'Total.expenditure' = 2, 'Schooling' = 12, 'Alcohol_consumption' = 10)
prediction <- predict(model, new_data)
print(paste("Predicted life expectancy:", prediction))
