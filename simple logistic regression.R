# build a simple logistic regression model to predict churn

# load packages
library(fst)
library(tidyverse)

# import data
churn <- read_fst("/Users/samuelwong/Desktop/Work/Data/datacamp/churn.fst")

glimpse(churn)
# Rows: 400
# Columns: 3
# $ has_churned               <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
# $ time_since_first_purchase <dbl> -1.08922097, 1.18298297, -0.84615637, 0.08694165, -1.16664155, 0.49339968, -0.30331208, 0.20622335, 0.13015313, 0.39932468, -0.15927380, 1.…
# $ time_since_last_purchase  <dbl> -0.72132150, 3.63443539, -0.42758226, -0.53567170, -0.67264000, -0.77000301, -0.79723164, 1.14672805, 1.70450257, -0.51586906, 1.17395669, …

# validate imbalance data
table(churn$has_churned)

# 0   1 
# 200 200 

# build a model
glm.model <- glm(has_churned ~ time_since_first_purchase, churn, family = binomial)

glm.model

# Call:  glm(formula = has_churned ~ time_since_first_purchase, family = binomial, 
#            data = churn)
# 
# Coefficients:
# (Intercept)  time_since_first_purchase  
# -0.01518                   -0.35479  
# 
# Degrees of Freedom: 399 Total (i.e. Null);  398 Residual
# Null Deviance:	    554.5 
# Residual Deviance: 543.7 	AIC: 547.7

# create some new data for prediction
explanatory_data <- tibble(time_since_first_purchase = seq(-1, 6, 0.25))

explanatory_data

# make prediction

# Odds ratio
# Odds ratios compare the probability of something happening with the probability of it not happening. 
# This is sometimes easier to reason about than probabilities, particularly when you want to make decisions about choices. 
# For example, if a customer has a 20% chance of churning, 
# it maybe more intuitive to say "the chance of them not churning is four times higher than the chance of them churning".

prediction_data <- explanatory_data %>% 
  mutate(has_churned = predict(glm.model, explanatory_data, type = "response"),     
         most_likely_outcome = round(has_churned), # anything >0.5 = 1 or churn
         # add Odds ratio
         odds_ratio = has_churned / (1 - has_churned),
         # Add the log odds ratio from odds_ratio
         log_odds_ratio = log(odds_ratio),
         # Add the log odds ratio using predict()
         log_odds_ratio2 = predict(glm.model, explanatory_data)
  )

prediction_data

# time_since_first_purchase has_churned most_likely_outcome odds_ratio log_odds_ratio log_odds_ratio2
# <dbl>       <dbl>               <dbl>      <dbl>          <dbl>           <dbl>
# 1                     -1          0.584                   1      1.40          0.340           0.340 
# 2                     -0.75       0.562                   1      1.29          0.251           0.251 
# 3                     -0.5        0.540                   1      1.18          0.162           0.162 
# 4                     -0.25       0.518                   1      1.08          0.0735          0.0735
# 5                      0          0.496                   0      0.985        -0.0152         -0.0152

# Odds ratios provide an alternative to probabilities that make it easier to compare positive and negative responses.
# Using prediction_data, plot odds_ratio vs. time_since_first_purchase
ggplot(prediction_data, aes(time_since_first_purchase, odds_ratio)) +
  # Make it a line plot
  geom_line() +
  # Add a dotted horizontal line at y = 1
  geom_hline(yintercept = 1, linetype = "dotted")

# evaluation metrics
# Calculating the confusion matrix
# Get the actual responses from the dataset
actual_response <- churn$has_churned

# Get the "most likely" predicted responses from the model
predicted_response <- round(fitted(glm.model))

# Create a table of counts
outcomes <- table(predicted_response, actual_response)

# See the result
outcomes

#                   actual_response
# predicted_response   0   1
#                  0 112  76
#                  1  88 124

# Measuring logistic model performance
# Convert outcomes to a yardstick confusion matrix
confusion <- conf_mat(outcomes)

# Plot the confusion matrix
autoplot(confusion)

# Get performance metrics for the confusion matrix
summary(confusion, event_level = "second")

# .metric              .estimator .estimate
# <chr>                <chr>          <dbl>
# 1 accuracy             binary         0.59 
# 2 kap                  binary         0.18 
# 3 sens                 binary         0.62 
# 4 spec                 binary         0.56 
# 5 ppv                  binary         0.585
# 6 npv                  binary         0.596
# 7 mcc                  binary         0.180
# 8 j_index              binary         0.180
# 9 bal_accuracy         binary         0.59 
# 10 detection_prevalence binary         0.53 
# 11 precision            binary         0.585
# 12 recall               binary         0.62 
# 13 f_meas               binary         0.602

