## This script was developed by Michael F Meyer (michael.f.meyer@wsu.edu)
## for the Reproducible Research Techniques with R workshop. 
## The goal of this script is to serve as a tutorial for different 
## types of modeling. The script can be broken into a few main 
## sections: 
## 1. One-way Linear Modeling
## 2. Multiple Linear Regression
## 3. Non-linear Modeling
## 3.1 Polynomial Regression
## 3.2 General Additive Models
## 3.3 Logistic Regression

# Install necessary packages (for the entire script)
install.packages("tidyverse")
install.packages("mgcv")
install.packages("ploynom")
install.packages("hexbin")

library(tidyverse)
library(mgcv)
library(polynom)
library(hexbin)

# Load and inspect the data

bike_data <- read.csv("../data/day.csv",
                      header = TRUE)

head(bike_data)

ggplot(bike_data, aes(atemp*50, (cnt))) +
  geom_point() +
  xlab("Feeling Temperature") +
  ylab("Total bikes rented")


# 1. One-way linear regression --------------------------------------------

# Create a subset of hte data 
bike_linear_data <- bike_data %>%
  mutate(feeling_temperature = atemp*50) %>%
  select(feeling_temperature, cnt)

# Build a linear model
# Count = b1 * feeling_temperature + alpha + error
# y = mx + b 
linear_model <- lm(formula = cnt ~ feeling_temperature,
                   data = bike_linear_data)

# Assess the model
summary(linear_model)

# Assess model object structure
str(linear_model)

# Extract R-squared
r_squared <- summary(linear_model)$r.squared

# Extract p-value
p_value <- summary(linear_model)$coefficients[2, 4]

# Extract slope
slope <- summary(linear_model)$coefficients[2, 1]

# Extract y-intercept
y_intercept <- summary(linear_model)$coefficients[1, 1]

## Challenge Number 1: Knowing what we've learned, how would you
## 1. Extract model residuals and 
## 2. Assess the residuals' distribution? 
## Analysis of choice, but assess the residuals.

residuals <- summary(linear_model)$residuals

challenge1 <- bike_linear_data %>%
  mutate(residuals)

ggplot(challenge1, aes(feeling_temperature, residuals)) +
  geom_point() +
  xlab("Feeling Temperature") +
  ylab("Residuals")

hist(linear_model$residuals)

# Visualize residual deviation from the modeled predictions 3 ways

# 1. Visualize model and points 
ggplot(data = bike_linear_data,
       mapping = aes(x = feeling_temperature, y = cnt)) +
  geom_point(alpha = 0.33) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal()

# 2. Group points into hexbins
ggplot(data = bike_linear_data,
       mapping = aes(x = feeling_temperature, y = cnt)) +
  geom_hex(bins = 10) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_fill_viridis_c(option = "plasma", name = "Number of\nPoints") +
  theme_minimal()

# 3. Lines and color points for residuals. 
# We will build this plot incrementally.
bike_linear_data$predicted_cnt <- predict(linear_model)

bike_linear_data$residuals <- residuals(linear_model)

head(bike_linear_data)

ggplot(data = bike_linear_data,
       mapping = aes(x = feeling_temperature, y = cnt)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_segment(mapping = aes(xend = feeling_temperature,
                             yend = predicted_cnt),
               alpha = 0.2) +
  geom_point(mapping = aes(color = abs(residuals))) +
  scale_color_viridis_c(option = "plasma", name = "Residual\nCount") +
  theme_minimal()
  
## Challenge Number 2a: Create a new data frame of temperatures
## from 0 to 50 with increments of 0.25, and predict values for 
## the new dataset. 
## Challenge Number 2b: Plot those new predictions.  

prediction_data <- data.frame(feeling_temperature = seq(from = 0,
                                                        to = 50,
                                                        by = 0.25))
head(prediction_data)

prediction_data$predicted_cnt <- predict(object = linear_model,
                                         newdata = prediction_data)

ggplot() +
  geom_point(data = bike_linear_data, aes(feeling_temperature, 
                                          cnt)) +
  geom_point(data = prediction_data, aes(feeling_temperature, 
                                         predicted_cnt),
             shape = 18, color = "blue")

# 2. Multiple Linear Regression -------------------------------------------

# Multiple continuous predictors

# Create a new dataset for multiple linear regression 
# with continuous variables. 
multiple_bike_data <- bike_data %>%
  mutate(feeling_temperature = atemp * 50,
         humidity = hum * 100,
         windiness = windspeed * 67) %>%
  select(feeling_temperature, humidity, windiness, cnt)

head(multiple_bike_data)

# First make a model where interactions are considered. 
multiple_linear_model <- lm(formula = cnt ~ feeling_temperature * humidity * windiness,
                            data = multiple_bike_data)

summary(multiple_linear_model)

# Assess the pairs plot for cross-correlations
pairs(multiple_bike_data[ , 1:3])

# Create a function to put correlation value in the upper panel.
# This function can be found in the documentation for pairs.
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# Rerun pairs with upper panel showing correlation values.

pairs(multiple_bike_data[ , 1:3], upper.panel = panel.cor)

# Second make a model where interactions are NOT considered. 
multiple_linear_model_no_inter <- lm(formula =  cnt ~ feeling_temperature + humidity + windiness,
                                     data = multiple_bike_data)

summary(multiple_linear_model_no_inter)
# Calculate the AIC of each linear model
# i.e., all interactions vs no interactions

AIC(multiple_linear_model)
AIC(multiple_linear_model_no_inter)

## Challenge Number 3a: Make a model that considers feeling_temperature,
## humidity, and the interaction of humidity and windiness on cnt

multiple_linear_model_spec_inter <- lm(formula = cnt ~ feeling_temperature + humidity + humidity:windiness,
                                       data = multiple_bike_data)

summary(multiple_linear_model_spec_inter)

# Mutliple linear regression with continuous and categorical predictors

# Create data subset
mixed_bike_data <- bike_data %>%
  mutate(feeling_temperature = atemp * 50,
         humidity = hum * 100,
         windiness = windspeed * 67,
         weather_type = ifelse(weathersit == 1, "Clear", NA),
         weather_type = ifelse(weathersit == 2, "Misty_Cloudy", weather_type),
         weather_type = ifelse(weathersit == 3, "Light_precip", weather_type),
         weather_type = ifelse(weathersit == 4, "Heavy_precip", weather_type),
         weather_type = as.factor(weather_type)) %>%
  select(feeling_temperature, humidity, windiness, weather_type, cnt)

# Check to be sure that there are no NAs
summary(mixed_bike_data)

# Build the model

mixed_type_model <- lm(cnt ~ feeling_temperature * humidity * windiness * weather_type,
                       data = mixed_bike_data)

summary(mixed_type_model)
# 3. Non-Linear Modeling --------------------------------------------------


# 3.1 Polynomial regression -----------------------------------------------

# Create a special dataset
bike_nonlinear_data <- bike_data %>%
  mutate(feeling_temperature = atemp*50) %>%
  select(feeling_temperature, cnt)

# Remind ourselves what these data looked like

ggplot(bike_nonlinear_data, aes(feeling_temperature, cnt)) +
  geom_point()

# Build the model
polynomial_bike_model <- lm(cnt ~ poly(feeling_temperature, degree = 3),
                            data = bike_nonlinear_data)

polynomial_bike_model <- lm(cnt ~ I(feeling_temperature^3),
                            data = bike_nonlinear_data)

summary(polynomial_bike_model)
# To build a polynomial equation without second and first order 
# terms, followup this workflow:


prediction_data <- data.frame(feeling_temperature = seq(from = 0, to = 45, by = 0.25))

prediction_data$predicted_cnt <- predict(object = polynomial_bike_model,
                                         newdata = prediction_data)


ggplot() +
  geom_point(data = bike_data, mapping = aes(atemp*50, (cnt))) +
  geom_point(data = prediction_data, mapping = aes(feeling_temperature, 
                                                   predicted_cnt),
             shape = 18, color = "blue") +
  xlab("Feeling Temperature") +
  ylab("Total bikes rented") +
  theme_minimal()

# Build the model



# 3.2 General Additive Models ---------------------------------------------

# Build the model 
# We will rerun this same R code multiple times using different 
# values for gamma. 
# Remember: Gamma is like a wiggliness factor (per Gavin Simpson),
# where gamma closer to zero is very wiggly, and gamma greater than 1
# less wiggly. 

bike_gam_model <- gam(cnt ~ s(feeling_temperature),
                      data = bike_nonlinear_data,
                      method = "REML", gamma = 10)

summary(bike_gam_model)

plot(bike_gam_model)

# 3.3 Logistic Regresssion ------------------------------------------------

# First look at some data that we will be modeling
ggplot(bike_data, aes(registered)) +
  geom_histogram() +
  facet_grid(~workingday)

ggplot(bike_data, aes(registered)) +
  geom_density() +
  facet_grid(~workingday)

bike_data %>%
  group_by(workingday) %>%
  summarize(mean_registered = mean(registered))

# Build the model 

logistic_model <- glm(formula = workingday ~ registered, 
                      data = bike_data,
                      family = binomial())

summary(logistic_model)

## Challenge 4

prediction_data <- data.frame(registered = seq(from = 0, to = 7000, by = 10))

prediction_data$predicted_cnt <- predict(object = logistic_model,
                                         newdata = prediction_data, 
                                         type = "response")

ggplot() +
  geom_point(data = bike_data, mapping = aes(registered, (workingday))) +
  geom_point(data = prediction_data, mapping = aes(registered, 
                                                   predicted_cnt),
             shape = 18) +
  xlab("Is it is a working day?") +
  ylab("Registered bike reservations")
