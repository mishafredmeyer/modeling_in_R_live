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

library(tidyverse)
library(mgcv)
library(polynom)

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


# Assess the model


# Assess model object structure


# Extract R-squared


# Extract p-value


# Extract slope


# Extract y-intercept

# Challenge Number 1


# Visualize residual deviation from the modeled predictions 3 ways

# 1. Visualize model and points 


# 2. Group points into hexbins


# 3. Lines and color points for residuals. 
# We will build this plot incrementally.



## Challenge Number 2


# 2. Multiple Linear Regression -------------------------------------------

# Multiple continuous predictors

# Create a new dataset for multiple linear regression 
# with continuous variables. 


# First make a model where interactions are considered. 



# Assess the pairs plot for cross-correlations


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


# Second make a model where interactions are NOT considered. 


# Calculate the AIC of each linear model
# i.e., all interactions vs no interactions


## Challenge Number 3

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



# 3. Non-Linear Modeling --------------------------------------------------


# 3.1 Polynomial regression -----------------------------------------------

# Create a special dataset
bike_nonlinear_data <- bike_data %>%
  mutate(feeling_temperature = atemp*50) %>%
  select(feeling_temperature, cnt)

# Remind ourselves what these data looked like


# Build the model

# To build a polynomial equation without second and first order 
# terms, followup this workflow:

# Build the model



# 3.2 General Additive Models ---------------------------------------------

# Build the model 
# We will rerun this same R code multiple times using different 
# values for gamma. 
# Remember: Gamma is like a wiggliness factor (per Gavin Simpson),
# where gamma closer to zero is very wiggly, and gamma greater than 1
# less wiggly. 



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

## Challenge 4
