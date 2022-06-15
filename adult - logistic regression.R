# load packages ####
library(tidyverse)
library(tidymodels)
library(vip)

# load data ####
data_adult <-read.csv("https://raw.githubusercontent.com/guru99-edu/R-Programming/master/adult.csv", stringsAsFactors = T)

glimpse(data_adult)

# Rows: 48,842
# Columns: 10
# $ x               <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27,…
# $ age             <int> 25, 38, 28, 44, 18, 34, 29, 63, 24, 55, 65, 36, 26, 58, 48, 43, 20, 43, 37, 40, 34, 34, 72, 25, 25…
# $ workclass       <fct> Private, Private, Local-gov, Private, ?, Private, ?, Self-emp-not-inc, Private, Private, Private, …
# $ education       <fct> 11th, HS-grad, Assoc-acdm, Some-college, Some-college, 10th, HS-grad, Prof-school, Some-college, 7…
# $ educational.num <int> 7, 9, 12, 10, 10, 6, 9, 15, 10, 4, 9, 13, 9, 9, 9, 14, 10, 9, 9, 16, 13, 10, 4, 13, 13, 9, 9, 9, 9…
# $ marital.status  <fct> Never-married, Married-civ-spouse, Married-civ-spouse, Married-civ-spouse, Never-married, Never-ma…
# $ race            <fct> Black, White, White, Black, White, White, Black, White, White, White, White, White, White, White, …
# $ gender          <fct> Male, Male, Male, Male, Female, Male, Male, Male, Female, Male, Male, Male, Female, Male, Male, Ma…
# $ hours.per.week  <int> 40, 50, 40, 40, 30, 30, 40, 32, 40, 10, 40, 40, 39, 35, 48, 50, 25, 30, 20, 45, 47, 35, 6, 43, 40,…
# $ income          <fct> <=50K, <=50K, >50K, >50K, <=50K, <=50K, <=50K, >50K, <=50K, <=50K, >50K, <=50K, <=50K, <=50K, >50K…

# EDA ####
summary(data_adult$x)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1   12211   24422   24422   36632   48842 

summary(data_adult$age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 17.00   28.00   37.00   38.64   48.00   90.00 

# replace "?" with "Unknown"
# probably good to bin all gov jobs etc.
table(data_adult$workclass)
# ?         Federal-gov        Local-gov     Never-worked          Private     Self-emp-inc Self-emp-not-inc        State-gov      Without-pay 
# 2799             1432             3136               10            33906             1695             3862             1981               21 

table(data_adult$education)
# 10th         11th         12th      1st-4th      5th-6th      7th-8th          9th   Assoc-acdm    Assoc-voc    Bachelors    Doctorate      HS-grad 
# 1389         1812          657          247          509          955          756         1601         2061         8025          594        15784 

# Masters    Preschool  Prof-school Some-college 
# 2657           83          834        10878 

# validate if education is ranked by the educational.num
# try create a new feature for education.level e.g. with uni, without uni, bachelors+
data_adult %>% 
  group_by(education, educational.num) %>% 
  summarise(n = n()) %>% 
  arrange(educational.num)

# education    educational.num     n
# <fct>                  <int> <int>
# 1 Preschool                  1    83
# 2 1st-4th                    2   247
# 3 5th-6th                    3   509
# 4 7th-8th                    4   955
# 5 9th                        5   756
# 6 10th                       6  1389
# 7 11th                       7  1812
# 8 12th                       8   657
# 9 HS-grad                    9 15784
# 10 Some-college              10 10878
# 11 Assoc-voc                 11  2061
# 12 Assoc-acdm                12  1601
# 13 Bachelors                 13  8025
# 14 Masters                   14  2657
# 15 Prof-school               15   834
# 16 Doctorate                 16   594

# try create a new feature - married or not married
table(data_adult$marital.status)
# Divorced     Married-AF-spouse    Married-civ-spouse Married-spouse-absent         Never-married             Separated               Widowed 
# 6633                    37                 22379                   628                 16117                  1530                  1518 

# try create a new feature - white or non-white people
table(data_adult$race)
# Amer-Indian-Eskimo Asian-Pac-Islander              Black              Other              White 
# 470               1519                              4685                406              41762 

# try create a new feature - white woman or non-white woman (curious to see if they are likely to make more $)
table(data_adult$gender)
# Female   Male 
# 16192  32650 

summary(data_adult$hours.per.week)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00   40.00   40.00   40.42   45.00   99.00 

hist(data_adult$hours.per.week)
# most people worked around 40 hrs a week

# how many hours per weeks? assuming one week = 7 days (otherwise, could be 5 business days)
# 7 * 24 = 168 hours if someone works non-stop
# so, how it is possible to work 99 hours a week?
# that is almost like working more than half a day in a week (99 / 7) = 14.14286
# that is possible for some jobs assuming that some people have the flexibility to work anytime e.g. self employed (business owners)
# maybe the value 99 = unknown

# validate for imbalance data
table(data_adult$income)
# <=50K  >50K 
# 37155 11687 

data_adult %>% 
  group_by(income) %>% 
  summarise(n = n(),
            pc = n / nrow(.))

# income     n    pc
# <=50K  37155 0.761
# >50K   11687 0.239

# feature eng ####
# start with some basic data cleaning
# x = a row number (remove)
# education = a categorical feature (remove)
# no feature eng to keep it simple for now
data_cleaned <- data_adult %>% 
  select(-x, -education)

glimpse(data_cleaned)

# Rows: 48,842
# Columns: 8
# $ age             <int> 25, 38, 28, 44, 18, 34, 29, 63, 24, 55, 65, 36, 26, 58, 48, 43, 20, 43, 37, 40, 34, 34, 72, 25, 25, 45, 22, 23, 54, 32, 46, 56, 24, 23, 26, 65…
# $ workclass       <fct> Private, Private, Local-gov, Private, ?, Private, ?, Self-emp-not-inc, Private, Private, Private, Federal-gov, Private, ?, Private, Private, S…
# $ educational.num <int> 7, 9, 12, 10, 10, 6, 9, 15, 10, 4, 9, 13, 9, 9, 9, 14, 10, 9, 9, 16, 13, 10, 4, 13, 13, 9, 9, 9, 9, 10, 10, 7, 13, 10, 9, 9, 13, 3, 6, 9, 14, …
# $ marital.status  <fct> Never-married, Married-civ-spouse, Married-civ-spouse, Married-civ-spouse, Never-married, Never-married, Never-married, Married-civ-spouse, Ne…
# $ race            <fct> Black, White, White, Black, White, White, Black, White, White, White, White, White, White, White, White, White, White, White, White, Asian-Pac…
# $ gender          <fct> Male, Male, Male, Male, Female, Male, Male, Male, Female, Male, Male, Male, Female, Male, Male, Male, Male, Female, Female, Male, Male, Female…
# $ hours.per.week  <int> 40, 50, 40, 40, 30, 30, 40, 32, 40, 10, 40, 40, 39, 35, 48, 50, 25, 30, 20, 45, 47, 35, 6, 43, 40, 90, 20, 54, 35, 60, 38, 50, 50, 40, 40, 40,…
# $ income          <fct> <=50K, <=50K, >50K, >50K, <=50K, <=50K, <=50K, >50K, <=50K, <=50K, >50K, <=50K, <=50K, <=50K, >50K, >50K, <=50K, <=50K, <=50K, >50K, >50K, <=5…

# below of the below
# the model predicts whether or not somebody has high income i.e. >50k (as it is the second of the categories)
# otherwise, the model would predict whether or not somebody has low income i.e. <=50k
# *try replace it with 1 and 0

levels(data_cleaned$income)
# "<=50K" ">50K" 

data_cleaned %>% 
  group_by(income) %>% 
  summarise(n = n())

# income     n
# <=50K  37155
# >50K   11687

# train model ####
# split the data
set.seed(222) 
data_split <- initial_split(data_cleaned, prop = 0.8, strata = income) # enforce similar distributions
train_data <- training(data_split); nrow(train_data) # 39073
test_data <- testing(data_split); nrow(test_data) # 9769

# train a model
model.1 <- glm(income ~ ., family = binomial, data = train_data)

# model interpretation ####
summary(model.1)

# Call:
# glm(formula = income ~ ., family = binomial, data = train_data)
# 
# Deviance Residuals: 
# Min       1Q   Median       3Q      Max  
# -2.7584  -0.5707  -0.2539  -0.0666   3.3650  
# 
# Coefficients:
#                                       Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                         -10.514503   0.245541 -42.822  < 2e-16 ***
# age                                   0.032009   0.001354  23.637  < 2e-16 ***

# workclassFederal-gov                  1.396911   0.116787  11.961  < 2e-16 ***
# workclassLocal-gov                    0.831752   0.103403   8.044 8.71e-16 ***
# workclassNever-worked                -8.171719 106.625960  -0.077  0.93891    
# workclassPrivate                      0.899126   0.090896   9.892  < 2e-16 ***
# workclassSelf-emp-inc                 1.326397   0.111801  11.864  < 2e-16 ***
# workclassSelf-emp-not-inc             0.316902   0.100822   3.143  0.00167 ** 
# workclassState-gov                    0.572991   0.114293   5.013 5.35e-07 ***
# workclassWithout-pay                 -0.421248   0.836348  -0.504  0.61449    

# educational.num                       0.384500   0.006844  56.180  < 2e-16 ***

# marital.statusMarried-AF-spouse       2.270057   0.443194   5.122 3.02e-07 ***
# marital.statusMarried-civ-spouse      2.131882   0.056206  37.930  < 2e-16 ***
# marital.statusMarried-spouse-absent   0.188567   0.173803   1.085  0.27795    
# marital.statusNever-married          -0.389246   0.068294  -5.700 1.20e-08 ***
# marital.statusSeparated              -0.009105   0.130172  -0.070  0.94423    
# marital.statusWidowed                -0.150432   0.124793  -1.205  0.22803  

# raceAsian-Pac-Islander                0.287807   0.207101   1.390  0.16462    
# raceBlack                             0.264032   0.197749   1.335  0.18181    
# raceOther                             0.024701   0.282863   0.087  0.93041    
# raceWhite                             0.511269   0.189613   2.696  0.00701 ** 

# genderMale                            0.126083   0.041870   3.011  0.00260 ** 
# hours.per.week                        0.030979   0.001323  23.412  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 42999  on 39072  degrees of freedom
# Residual deviance: 28454  on 39050  degrees of freedom
# AIC: 28500
# 
# Number of Fisher Scoring iterations: 11

# try relevel
# data_cleaned$gender <- relevel(data_cleaned$gender, "Male")

# the below explain why the summary() does not show all values for all features
# level 1 = 0 Estimate e.g. for gender = female = 0. race = Amer-Indian-Eskimo = 0.
levels(train_data$gender) # Female" "Male" 
levels(train_data$race) # "Amer-Indian-Eskimo" "Asian-Pac-Islander" "Black"              "Other"              "White"  

# 1, how to interpret numeric features? ####
# age, educational.num and hours.per.week = numeric features

#                                       Estimate Std. Error z value Pr(>|z|)    
# age                                   0.032009   0.001354  23.637  < 2e-16 ***
# the interpretation is that all else being equal, older people are more likely to have a high income

#                                       Estimate Std. Error z value Pr(>|z|)    
# educational.num                       0.384500   0.006844  56.180  < 2e-16 ***
# the interpretation is that all else being equal, people with higher educational.num are more likely to have a high income
# the num is ordinal number like rank. the highest is 16 which education = Doctorate 

# 1 Preschool                  1    83
# 2 1st-4th                    2   247
# 3 5th-6th                    3   509
# 4 7th-8th                    4   955
# 5 9th                        5   756
# 6 10th                       6  1389
# 7 11th                       7  1812
# 8 12th                       8   657
# 9 HS-grad                    9 15784
# 10 Some-college              10 10878
# 11 Assoc-voc                 11  2061
# 12 Assoc-acdm                12  1601
# 13 Bachelors                 13  8025
# 14 Masters                   14  2657
# 15 Prof-school               15   834
# 16 Doctorate                 16   594

#                                       Estimate Std. Error z value Pr(>|z|)    
# hours.per.week                        0.030979   0.001323  23.412  < 2e-16 ***

summary(train_data$hours.per.week)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00   40.00   40.00   40.44   45.00   99.00 

# If the hours.per.week = 1, then the effect is 0.03 * 1 = 0.03
# For hours.per.week = 10, the effect is 0.03 * 10 = 0.3 
# The longest hours.per.week observed in this data set is 99 
# and the shortest hours.per.week is 1, 
# so the maximum possible effect for hours.per.week is 0.03 * 99 = 2.97, 
# and thus the most extreme possible effect for hours.per.week is greater than the effect for any of the other variables.

glimpse(train_data)

# 2, how to interpret factor features? ####
# workclass, marital.status, race, gender are factors

# there are 2 types
# 1 = binary e.g. gender
# 2 = multiple e.g. race

levels(train_data$gender)
# [1] "Female" "Male"  

#                                       Estimate Std. Error z value Pr(>|z|)    
# genderMale                            0.126083   0.041870   3.011  0.00260 ** 
# with the above, all else being equal, males were more likely to have high income than females 
# exp(0.126083) = 1.134376 times that of being a female holding all else constant

#                                       Estimate Std. Error z value Pr(>|z|)    
# workclassFederal-gov                  1.396911   0.116787  11.961  < 2e-16 ***
# workclassLocal-gov                    0.831752   0.103403   8.044 8.71e-16 ***
# workclassNever-worked                -8.171719 106.625960  -0.077  0.93891    
# workclassPrivate                      0.899126   0.090896   9.892  < 2e-16 ***
# workclassSelf-emp-inc                 1.326397   0.111801  11.864  < 2e-16 ***
# workclassSelf-emp-not-inc             0.316902   0.100822   3.143  0.00167 ** 
# workclassState-gov                    0.572991   0.114293   5.013 5.35e-07 ***
# workclassWithout-pay                 -0.421248   0.836348  -0.504  0.61449    

levels(train_data$workclass)
# [1] "?"                "Federal-gov"      "Local-gov"        "Never-worked"     "Private"          "Self-emp-inc"     "Self-emp-not-inc" "State-gov"        "Without-pay"     

# the workclass tells "Federal-gov" and "Self-emp-inc" are more likely to have a high income
# "Never-worked" and "Without-pay" are both negative thus they are less likely to have a high income (but this = true?)
# the value of "?" in workclass is defined as having an estimate of 0 thus not shown in summary()
# for workclass = Federal-gov, exp(1.396911) = 4.042693 times that of being a "?" holding all else constant
# for workclass = Never-worked, exp(-8.171719) = 0.0002825319 times that of being a "?" holding all else constant

# only 2 people are high income without pay
data_adult %>% 
  filter(workclass == "Without-pay") %>% 
  group_by(income) %>% 
  summarise(n = n(),
            pc = (n / nrow(.)) * 100)
  
# income     n    pc
# <=50K     19  90.5 
# >50K       2  9.52

#                                       Estimate Std. Error z value Pr(>|z|)    
# raceAsian-Pac-Islander                0.287807   0.207101   1.390  0.16462    
# raceBlack                             0.264032   0.197749   1.335  0.18181    
# raceOther                             0.024701   0.282863   0.087  0.93041    
# raceWhite                             0.511269   0.189613   2.696  0.00701 ** 

levels(train_data$race)
# [1] "Amer-Indian-Eskimo" "Asian-Pac-Islander" "Black"              "Other"              "White"             

# The effect of being a White person is approximately twice as big as being a black or asian person
# for race = Pac-Islander, exp(0.287807) = 1.3335 times that of being a "Amer-Indian-Eskimo" holding all else constant

#                                       Estimate Std. Error z value Pr(>|z|)    
# educational.num                       0.384500   0.006844  56.180  < 2e-16 ***
# marital.statusNever-married          -0.389246   0.068294  -5.700 1.20e-08 ***

# If somebody has educational.num = 1 and marital.status = Never-married, 
# these two effects almost completely cancel each other out

#                                       Estimate Std. Error z value Pr(>|z|)    
# workclassWithout-pay                 -0.421248   0.836348  -0.504  0.61449    
# raceWhite                             0.511269   0.189613   2.696  0.00701 ** 

# If somebody in workclass Without-pay and race = white, 
# these two effects almost completely cancel each other out

# prediction ####
# make predictions using the test data
preds <- test_data %>% 
  mutate(probability = predict(model.1, test_data, type = "response"),
         predicted.class = ifelse(probability > 0.5, ">50K", "<=50K"),
         predicted.class = as.factor(predicted.class))

head(preds)

#     age workclass educational.num     marital.status  race gender hours.per.week income probability predicted.class
# 1   25   Private               7      Never-married Black   Male             40  <=50K 0.007041886           <=50K
# 2   38   Private               9 Married-civ-spouse White   Male             50  <=50K 0.352371016           <=50K
# 9   24   Private              10      Never-married White Female             40  <=50K 0.024785834           <=50K
# 24  25   Private              13      Never-married White   Male             43  <=50K 0.091721793           <=50K
# 36  65         ?               9 Married-civ-spouse White   Male             40  <=50K 0.270477363           <=50K
# 46  28   Private              11 Married-civ-spouse White Female             36   >50K 0.332303939           <=50K

# manually calculate the probability for the first row
# then, manually calculate the probability for the second row
age = (0.032009 * 34)
workclass = 0.899126
educational.num = (0.384500 * 13)
marital.status = 2.131882
race = 0.511269
gender = 0.126083
hours.per.week = (0.030979 * 47)

# the formula
# Probability = 1 / (1 + exp(-x))

age + workclass + educational.num + marital.status + race + gender + hours.per.week # = 11.21118

mod_summary <- summary(model.1)
Intercept <- mod_summary$coefficients[1, 1]

mod_summary$coefficients[1, 1] + 11.21118 # 0.6966773

1 / (1 + exp(-0.696677)) # = 0.6674506 (that is correct)

# evaluation metrics ####
cm <- conf_mat(preds, truth = "income", estimate = "predicted.class")
cm

#            Truth
# Prediction <=50K >50K
#      <=50K  6844 1125
#      >50K    587 1213

# print all metrics
summary(cm)

# .metric              .estimator .estimate
# <chr>                <chr>          <dbl>
# 1 accuracy             binary         0.825
# 2 kap                  binary         0.477
# 3 sens                 binary         0.921
# 4 spec                 binary         0.519
# 5 ppv                  binary         0.859
# 6 npv                  binary         0.674
# 7 mcc                  binary         0.484
# 8 j_index              binary         0.440
# 9 bal_accuracy         binary         0.720
# 10 detection_prevalence binary         0.816
# 11 precision            binary         0.859
# 12 recall               binary         0.921
# 13 f_meas               binary         0.889

# readings ####
# https://www.guru99.com/r-generalized-linear-model.html
# https://www.displayr.com/how-to-interpret-logistic-regression-coefficients/
# https://towardsdatascience.com/an-introduction-to-logistic-regression-for-categorical-data-analysis-7cabc551546c


