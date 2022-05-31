# load packages
library(tidyverse)
library(caret)
library(naniar)

# Load the data and remove NAs
data("PimaIndiansDiabetes2", package = "mlbench")

# visualise missing data
vis_miss(PimaIndiansDiabetes2)

# for demo purpose, remove all missing data
PimaIndiansDiabetes2 <- na.omit(PimaIndiansDiabetes2)

# Inspect the data
glimpse(PimaIndiansDiabetes2)

# Rows: 392
# Columns: 9
# $ pregnant <dbl> 1, 0, 3, 2, 1, 5, 0, 1, 1, 3, 11, 10, 1, 13, 3, 3, 4, 4, 3, 9, 1, 1, 5, 8, 7, 7, 0, 0, 2, 1, 4, 2, 5, 4, 7, 2, 2, 15,…
# $ glucose  <dbl> 89, 137, 78, 197, 189, 166, 118, 103, 115, 126, 143, 125, 97, 145, 158, 88, 103, 111, 180, 171, 103, 101, 88, 176, 15…
# $ pressure <dbl> 66, 40, 50, 70, 60, 72, 84, 30, 70, 88, 94, 70, 66, 82, 76, 58, 60, 72, 64, 110, 80, 50, 66, 90, 66, 68, 88, 64, 58, …
# $ triceps  <dbl> 23, 35, 32, 45, 23, 19, 47, 38, 30, 41, 33, 26, 15, 19, 36, 11, 33, 47, 25, 24, 11, 15, 21, 34, 42, 39, 60, 41, 34, 1…
# $ insulin  <dbl> 94, 168, 88, 543, 846, 175, 230, 83, 96, 235, 146, 115, 140, 110, 245, 54, 192, 207, 70, 240, 82, 36, 23, 300, 342, 3…
# $ mass     <dbl> 28.1, 43.1, 31.0, 30.5, 30.1, 25.8, 45.8, 43.3, 34.6, 39.3, 36.6, 31.1, 23.2, 22.2, 31.6, 24.8, 24.0, 37.1, 34.0, 45.…
# $ pedigree <dbl> 0.167, 2.288, 0.248, 0.158, 0.398, 0.587, 0.551, 0.183, 0.529, 0.704, 0.254, 0.205, 0.487, 0.245, 0.851, 0.267, 0.966…
# $ age      <dbl> 21, 33, 26, 53, 59, 51, 31, 33, 32, 27, 51, 41, 22, 57, 28, 22, 33, 56, 26, 54, 22, 26, 30, 58, 42, 41, 31, 22, 24, 2…
# $ diabetes <fct> neg, pos, pos, pos, pos, pos, pos, neg, pos, neg, pos, pos, neg, neg, pos, neg, neg, pos, neg, pos, neg, neg, neg, po…

# Split the data into training and test set
set.seed(123)
training.samples <- PimaIndiansDiabetes2$diabetes %>% 
  createDataPartition(p = 0.8, list = FALSE)

# create train and test sets
train.data  <- PimaIndiansDiabetes2[training.samples, ]; nrow(train.data) # 314
test.data <- PimaIndiansDiabetes2[-training.samples, ]; nrow(test.data) # 78

# Simple logistic regression
glm.model.1 <- glm(diabetes ~ glucose, data = train.data, family = binomial)
summary(glm.model.1)$coef

#                 Estimate  Std. Error   z value     Pr(>|z|)
# (Intercept) -6.15882009 0.700096646 -8.797100 1.403974e-18
# glucose      0.04327234 0.005341133  8.101716 5.418949e-16

# The intercept (b0) is -6.15882009 and the coefficient of glucose variable is 0.04327234

# create some dummy data
new.data <- data.frame(glucose = c(20,  180))

# make predictions
probabilities <- glm.model.1 %>% predict(new.data, type = "response")

#           1           2 
# 0.004999659 0.836197127 

predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
predicted.classes

#     1     2 
# "neg" "pos" 

# manually calculate the probability
glucose <- 180
p <- exp(-6.15882009 + 0.04327234 * glucose) / (1 + exp(-6.15882009 + 0.04327234 * glucose))
p # 0.8361972

# plot the probability using the train set
train.data %>%
  mutate(prob = ifelse(diabetes == "pos", 1, 0)) %>%
  ggplot(aes(glucose, prob)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model", 
    x = "Plasma Glucose Concentration",
    y = "Probability of being diabete-pos"
  )

# Multiple logistic regression
# Fit the model with all features
glm.model.2 <- glm(diabetes ~ ., data = train.data, family = binomial)

options(scipen = 999)
summary(glm.model.2)$coef

#                   Estimate  Std. Error     z value              Pr(>|z|)
# (Intercept) -10.5340032606 1.439679266 -7.31690975 0.0000000000002537464
# pregnant      0.1005030526 0.061266974  1.64041157 0.1009196210867034094
# glucose       0.0370962115 0.006486093  5.71934633 0.0000000106934638298
# pressure     -0.0003875933 0.013826185 -0.02803328 0.9776356103183786450
# triceps       0.0141777114 0.019981885  0.70952823 0.4779967375616170355
# insulin       0.0005939876 0.001508231  0.39383055 0.6937061477701320644
# mass          0.0799744726 0.031798907  2.51500698 0.0119030026651822888
# pedigree      1.3291492192 0.482291020  2.75590704 0.0058529628033692242
# age           0.0271822430 0.020199295  1.34570257 0.1783984559420226657

# Summarize the model
summary(glm.model.2)

# Call:
#   glm(formula = diabetes ~ ., family = binomial, data = train.data)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -2.5832  -0.6544  -0.3292   0.6248   2.5968  
# 
# Coefficients:
#                Estimate  Std. Error z value          Pr(>|z|)    
# (Intercept) -10.5340033   1.4396793  -7.317 0.000000000000254 ***
# pregnant      0.1005031   0.0612670   1.640           0.10092    
# glucose       0.0370962   0.0064861   5.719 0.000000010693464 ***
# pressure     -0.0003876   0.0138262  -0.028           0.97764    
# triceps       0.0141777   0.0199819   0.710           0.47800    
# insulin       0.0005940   0.0015082   0.394           0.69371    
# mass          0.0799745   0.0317989   2.515           0.01190 *  
# pedigree      1.3291492   0.4822910   2.756           0.00585 ** 
# age           0.0271822   0.0201993   1.346           0.17840    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 398.80  on 313  degrees of freedom
# Residual deviance: 267.18  on 305  degrees of freedom
# AIC: 285.18
# 
# Number of Fisher Scoring iterations: 5

# Estimate: the intercept (b0) and the beta coefficient estimates associated to each predictor variable
# Std.Error: the standard error of the coefficient estimates. This represents the accuracy of the coefficients. The larger the standard error, the less confident we are about the estimate.
# z value: the z-statistic, which is the coefficient estimate (column 2) divided by the standard error of the estimate (column 3)
# Pr(>|z|): The p-value corresponding to the z-statistic. The smaller the p-value, the more significant the estimate is.

coef(glm.model.2)
#    (Intercept)       pregnant        glucose       pressure        triceps        insulin           mass       pedigree            age 
# -10.5340032606   0.1005030526   0.0370962115  -0.0003875933   0.0141777114   0.0005939876   0.0799744726   1.3291492192   0.0271822430 

# glucose = 0.0370962115
# This means that an increase in glucose is associated with increase in the probability of being diabetes-positive

# pressure = -0.0003875933
# This means that an increase in blood pressure will be associated with a decreased probability of being diabetes-positive

# For example, the regression coefficient for glucose is 0.0370962115
# This indicate that one unit increase in the glucose concentration will increase the odds of being diabetes-positive by 
# exp(0.0370962115) 1.037793 times.

# Make predictions
probabilities <- glm.model.2 %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")

preds <- test.data %>% 
  mutate(probabilities = predict(glm.model.2, test.data, type = "response"),
         predicted.classes = ifelse(probabilities > 0.5, "pos", "neg"),
         predicted.classes = as.factor(predicted.classes))
         
head(preds)

#    pregnant glucose pressure triceps insulin mass pedigree age diabetes probabilities predicted.classes
# 19        1     103       30      38      83 43.3    0.183  33      neg     0.1926284               neg
# 21        3     126       88      41     235 39.3    0.704  27      neg     0.4852623               neg
# 32        3     158       76      36     245 31.6    0.851  28      pos     0.6625272               pos
# 55        7     150       66      42     342 34.7    0.718  42      neg     0.7986815               pos
# 64        2     141       58      34     128 25.4    0.699  24      neg     0.2780734               neg
# 71        2     100       66      20      90 32.9    0.867  28      pos     0.1458773               neg

# Model accuracy
mean(preds$predicted.classes == preds$diabetes) # 0.7564103

library(tidymodels)
cmat <- conf_mat(preds, truth = "diabetes", estimate = "predicted.classes")
cmat

#           Truth
# Prediction neg pos
#        neg  44  11
#        pos   8  15

summary(cmat)

# .metric              .estimator .estimate
# <chr>                <chr>          <dbl>
# 1 accuracy             binary         0.756
# 2 kap                  binary         0.436
# 3 sens                 binary         0.846
# 4 spec                 binary         0.577
# 5 ppv                  binary         0.8  
# 6 npv                  binary         0.652
# 7 mcc                  binary         0.437
# 8 j_index              binary         0.423
# 9 bal_accuracy         binary         0.712
# 10 detection_prevalence binary         0.705
# 11 precision            binary         0.8  
# 12 recall               binary         0.846
# 13 f_meas               binary         0.822


