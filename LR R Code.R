# Load the dataset and check summary statistics
data <- winequality_white
summary(data)

# Export summary statistics to a CSV
write.csv(summary(data), 'results_descriptive_statistics.csv')

# Boxplot for specific variables
boxplot(data$`fixed acidity`, main = "Boxplot of Fixed Acidity")
boxplot(data$`volatile acidity`, main = "Boxplot of Volatile Acidity", outline = FALSE)

# Correlation between variables 'fixed acidity' and 'quality'
cor(data$`fixed acidity`, data$quality, method = "spearman")
cor(data$`fixed acidity`, data$quality, method = "pearson")
cor(data$`fixed acidity`, data$quality, method = "kendall")

# Correlation matrix for all variables
cor_matrix <- cor(data[, c("fixed acidity", "volatile acidity", "citric acid", "residual sugar", "chlorides", 
                           "free sulfur dioxide", "total sulfur dioxide", "density", "pH", "sulphates", 
                           "alcohol", "quality")])
round(cor_matrix, digits = 2)

# Scatterplot for selected variable relationships
library(ggplot2)
ggplot(data, aes(x = `alcohol`, y = quality)) +
  geom_point(color = '#0c4c8a') +
  theme_minimal() +
  ggtitle("Alcohol Content vs. Wine Quality") +
  xlab("Alcohol Content") +
  ylab("Wine Quality")

# Scatterplot matrix for specific variables
pairs(data[, c("fixed acidity", "volatile acidity", "citric acid", "quality")])

# Improved correlation matrix visualization
library(corrplot)
corrplot(cor(data[, c("fixed acidity", "volatile acidity", "citric acid", "quality")]),
         method = "number", type = "upper")

# Linear regression model - Initial model including all variables
model1 <- lm(quality ~ `fixed acidity` + `volatile acidity` + `citric acid` + `residual sugar` + chlorides + 
               `free sulfur dioxide` + `total sulfur dioxide` + density + pH + sulphates + alcohol, 
             data = data)
summary(model1)

# Subsequent models with removed variables based on significance
model2 <- lm(quality ~ `fixed acidity` + `volatile acidity` + `citric acid` + `residual sugar` + chlorides + 
               `free sulfur dioxide` + `total sulfur dioxide` + density + pH, data = data)
summary(model2)

model3 <- lm(quality ~ `fixed acidity` + `volatile acidity` + `citric acid` + `residual sugar` + chlorides + 
               `free sulfur dioxide` + density, data = data)
summary(model3)

# Final model with selected variables
model_final <- lm(quality ~ `fixed acidity` + `volatile acidity` + `citric acid` + `residual sugar` + chlorides, 
                  data = data)
summary(model_final)

# Residual analysis for the final model
residuals <- resid(model_final)

# Plot of Fitted vs. Residuals
plot(fitted(model_final), residuals, main = "Fitted Model vs. Residuals")
abline(0, 0)

# Q-Q Plot for residuals to check normality
qqnorm(residuals)
qqline(residuals, col = "red")

# Density plot of residuals
plot(density(residuals), main = "Density Plot of Residuals")

# Histogram of residuals with overlayed normal curve
hist(residuals, breaks = 20, freq = FALSE, 
     main = "Histogram of Residuals", xlab = "Residuals", col = "lightblue")
curve(dnorm(x, mean = mean(residuals), sd = sd(residuals)), col = "red", lwd = 2, add = TRUE)
