# load packages ####
library(tidyverse)
library(tidymodels)
library(vip)
library(caret)
library(car)

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
#    1   12211   24422   24422   36632   48842 

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
#     6633                    37                 22379                   628                 16117                  1530                  1518 

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

# most people worked around 40 hrs a week
hist(data_adult$hours.per.week)

ggplot(data_adult, aes(income, hours.per.week)) +
  geom_boxplot() +
  theme_bw()

# how many hours per weeks? assuming one week = 5 days (otherwise, could be up to 7 days)
# 5 * 24 = 120 hours if someone works non-stop
# so, how it is possible to work 99 hours a week?
# that is almost like working everyday
# it is possible for some jobs assuming that some people have the flexibility to work anytime e.g. self employed (business owners)
# maybe the value 99 = unknown

# validate for imbalance data
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
# education = a categorical feature (remove) *more sensible to use factor actually
# no feature eng to keep it simple for now
data_cleaned <- data_adult %>% 
  select(-x, -educational.num)

glimpse(data_cleaned)

# Rows: 48,842
# Columns: 8
# $ age            <int> 25, 38, 28, 44, 18, 34, 29, 63, 24, 55, 65, 36, 26, 58, 48, 43, 20, 43, 37, 40, 34, 34, 72, 25, 25, 45,…
# $ workclass      <fct> Private, Private, Local-gov, Private, ?, Private, ?, Self-emp-not-inc, Private, Private, Private, Feder…
# $ education      <fct> 11th, HS-grad, Assoc-acdm, Some-college, Some-college, 10th, HS-grad, Prof-school, Some-college, 7th-8t…
# $ marital.status <fct> Never-married, Married-civ-spouse, Married-civ-spouse, Married-civ-spouse, Never-married, Never-married…
# $ race           <fct> Black, White, White, Black, White, White, Black, White, White, White, White, White, White, White, White…
# $ gender         <fct> Male, Male, Male, Male, Female, Male, Male, Male, Female, Male, Male, Male, Female, Male, Male, Male, M…
# $ hours.per.week <int> 40, 50, 40, 40, 30, 30, 40, 32, 40, 10, 40, 40, 39, 35, 48, 50, 25, 30, 20, 45, 47, 35, 6, 43, 40, 90, …
# $ income         <fct> <=50K, <=50K, >50K, >50K, <=50K, <=50K, <=50K, >50K, <=50K, <=50K, >50K, <=50K, <=50K, <=50K, >50K, >50…

# check imbalance data
# * will need to re-visit
data_cleaned %>% 
  group_by(income) %>% 
  summarise(n = n(),
            pc = n / nrow(.))

#   income     n    pc
# 1 <=50K  37155 0.761
# 2 >50K   11687 0.239

# corplot
library(ggcorrplot)
# https://stackoverflow.com/questions/52554336/plot-the-equivalent-of-correlation-matrix-for-factors-categorical-data-and-mi

model.matrix(~0+., data = data_cleaned) %>% 
  cor(use = "pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type = "lower", lab = TRUE, lab_size = 2)

# the model predicts whether or not somebody has high income i.e. >50k (as it is the second of the categories)

levels(data_cleaned$income)
# "<=50K" ">50K" 

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
# -2.7485  -0.5665  -0.2547  -0.0695   3.2210  
# 
# Coefficients:
#                                       Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)                          -8.144167   0.270644 -30.092 < 0.0000000000000002 ***
# age                                   0.031167   0.001372  22.719 < 0.0000000000000002 ***
# workclassFederal-gov                  1.405433   0.117046  12.008 < 0.0000000000000002 ***
# workclassLocal-gov                    0.826192   0.103830   7.957 0.000000000000001760 ***
# workclassNever-worked                -8.187561 108.001503  -0.076              0.93957    
# workclassPrivate                      0.906212   0.091031   9.955 < 0.0000000000000002 ***
# workclassSelf-emp-inc                 1.323429   0.112014  11.815 < 0.0000000000000002 ***
# workclassSelf-emp-not-inc             0.307964   0.101121   3.045              0.00232 ** 
# workclassState-gov                    0.563239   0.114822   4.905 0.000000932658125753 ***
# workclassWithout-pay                 -0.343050   0.822723  -0.417              0.67670    
# education11th                         0.143886   0.184607   0.779              0.43573    
# education12th                         0.665360   0.220511   3.017              0.00255 ** 
# education1st-4th                     -0.999166   0.414102  -2.413              0.01583 *  
# education5th-6th                     -0.462161   0.266033  -1.737              0.08235 .  
# education7th-8th                     -0.391373   0.199613  -1.961              0.04992 *  
# education9th                         -0.191523   0.225622  -0.849              0.39596    
# educationAssoc-acdm                   1.912448   0.153781  12.436 < 0.0000000000000002 ***
# educationAssoc-voc                    1.789563   0.148707  12.034 < 0.0000000000000002 ***
# educationBachelors                    2.683653   0.137445  19.525 < 0.0000000000000002 ***
# educationDoctorate                    3.832370   0.182620  20.985 < 0.0000000000000002 ***
# educationHS-grad                      1.091257   0.135934   8.028 0.000000000000000992 ***
# educationMasters                      3.125380   0.144388  21.646 < 0.0000000000000002 ***
# educationPreschool                   -1.346480   1.031139  -1.306              0.19161    
# educationProf-school                  3.796349   0.171332  22.158 < 0.0000000000000002 ***
# educationSome-college                 1.594575   0.137277  11.616 < 0.0000000000000002 ***
# marital.statusMarried-AF-spouse       2.282810   0.444643   5.134 0.000000283611465477 ***
# marital.statusMarried-civ-spouse      2.135239   0.056517  37.780 < 0.0000000000000002 ***
# marital.statusMarried-spouse-absent   0.190901   0.174162   1.096              0.27303    
# marital.statusNever-married          -0.412519   0.068757  -6.000 0.000000001977167328 ***
# marital.statusSeparated              -0.012906   0.130655  -0.099              0.92131    
# marital.statusWidowed                -0.142235   0.125189  -1.136              0.25589    
# raceAsian-Pac-Islander                0.265217   0.207976   1.275              0.20223    
# raceBlack                             0.269614   0.198467   1.358              0.17431    
# raceOther                            -0.014132   0.283405  -0.050              0.96023    
# raceWhite                             0.510364   0.190340   2.681              0.00733 ** 
# genderMale                            0.122671   0.042026   2.919              0.00351 ** 
# hours.per.week                        0.030777   0.001325  23.227 < 0.0000000000000002 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 42999  on 39072  degrees of freedom
# Residual deviance: 28385  on 39036  degrees of freedom
# AIC: 28459
# 
# Number of Fisher Scoring iterations: 11

# How to interpret the intercept?
# exp(-8.144167) / (1 + exp(-8.144167)) = 0.0002903402
# the probability to have a high income when everything is 0 or at the reference group level is close to 0

# Multicollinearity check

# calculate the VIF for each predictor variable in the model
vif(model.1)

vif_values <- vif(model.1)

#create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")

#                     GVIF Df GVIF^(1/(2*Df))
# age            1.258089  1        1.121646
# workclass      1.229342  8        1.012989
# education      1.207327 15        1.006300
# marital.status 1.518696  6        1.035434
# race           1.046632  4        1.005713
# gender         1.313110  1        1.145910
# hours.per.week 1.096416  1        1.047099

# the prob from the train set
model.1$fitted.values
# 1            2            5            6            7            9           12           13           14           17           18           19           22 
# 0.0075126998 0.3497018610 0.0064196810 0.0064155203 0.0074971162 0.0255564956 0.7391630263 0.0181202817 0.2068539872 0.0117306256 0.2304137291 0.0181612642 0.0235908741 

length(model.1$fitted.values) # 39073

# the below calculates the usefulness of the model
# Null deviance: 42999  on 39072  degrees of freedom
# Residual deviance: 28385  on 39036  degrees of freedom

# X2 = Null deviance – Residual deviance
42999 - 28385 # 14545
39072 - 39036 # 22
p_value <- 1 - pchisq(14614, 36)
p_value # 0

# Chi-Square Score to P Value Calculator
# https://www.statology.org/chi-square-p-value-calculator/
# Since this p-value is much less than .05, we would conclude that the model is highly useful 
# for predicting the probability that a given individual defaults

# the below explains why the summary() does not show all values for all features i.e. the reference group
# level 1 = 0 Estimate e.g. for gender = female = 0. race = Amer-Indian-Eskimo = 0.

levels(train_data$gender) # Female" "Male" 
levels(train_data$race) # "Amer-Indian-Eskimo" "Asian-Pac-Islander" "Black"              "Other"              "White"  

# 1, how to interpret numeric features? ####
# age, educational.num and hours.per.week = numeric features

#                                       Estimate Std. Error z value Pr(>|z|)    
# age                                   0.032009   0.001354  23.637  < 2e-16 ***
# the interpretation is that all else being equal, older people are more likely to have a high income

# so, for someone who is aged 32 
log_odds <- 0.032009 * 32 # aged 32
exp(log_odds) # the log is 2.785112
exp(log_odds) / (1 + exp(log_odds)) # probability is 0.735807 or 73% to be high income vs. low income

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

# if hours.per.week = 1, then the effect is 0.03 * 1 = 0.03
# if hours.per.week = 10, the effect is 0.03 * 10 = 0.3 
# the longest hours.per.week observed in this data set is 99 and the shortest hours.per.week is 1
# so the maximum possible effect for hours.per.week is 0.03 * 99 = 2.97
# thus the most extreme possible effect for hours.per.week is greater than the effect for any of the other variables

# 2, how to interpret factor features? ####
# workclass, marital.status, race, gender are factors

# there are 2 types
# 1 = binary e.g. gender
# 2 = multiple e.g. race

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

# the workclass = "Federal-gov" and "Self-emp-inc" are more likely to have a high income
# "Never-worked" and "Without-pay" are both negative thus they are less likely to have a high income (but this = true?)
# the value of "?" in workclass is defined as having an estimate of 0 (reference group) thus not shown in summary()
# workclass = Federal-gov, exp(1.396911) = 4.042693 times (odds) that of being a "?" holding all else constant
# workclass = Never-worked, exp(-8.171719) = 0.0002825319 times that of being a "?" holding all else constant

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

log_odds <- 0.511269
exp(log_odds) # the log is 1.667406
exp(log_odds) / (1 + exp(log_odds)) # probability is 0.6251039 or 62% to have a high income vs. the reference group
# in the case, the reference group is Amer-Indian-Eskimo based on the below

table(data_cleaned$race)
# Amer-Indian-Eskimo Asian-Pac-Islander              Black              Other              White 
#                470               1519               4685                406              41762 

# the effect of being a White person is approximately twice as big as being a black or asian person
# for race = Pac-Islander, exp(0.287807) = 1.3335 times that of being a "Amer-Indian-Eskimo" (reference group) holding all else constant

#                                       Estimate Std. Error z value Pr(>|z|)    
# educational.num                       0.384500   0.006844  56.180  < 2e-16 ***
# marital.statusNever-married          -0.389246   0.068294  -5.700 1.20e-08 ***

# if somebody has educational.num = 1 and marital.status = Never-married, 
# these two effects almost completely cancel each other out

#                                       Estimate Std. Error z value Pr(>|z|)    
# workclassWithout-pay                 -0.421248   0.836348  -0.504  0.61449    
# raceWhite                             0.511269   0.189613   2.696  0.00701 ** 

# if somebody in workclass Without-pay and race = white, 
# these two effects almost completely cancel each other out

# prediction ####
# make predictions using the test data
preds <- test_data %>% 
  mutate(probability = predict(model.1, test_data, type = "response"),
         predicted.class = ifelse(probability > 0.5, ">50K", "<=50K"),
         predicted.class = as.factor(predicted.class))

head(preds)

#     age        workclass educational.num     marital.status  race gender hours.per.week income probability predicted.class
# 10  55          Private               4 Married-civ-spouse White   Male             10  <=50K  0.03776498           <=50K
# 21  34          Private              13 Married-civ-spouse White   Male             47   >50K  0.66745057            >50K
# 32  56 Self-emp-not-inc               7            Widowed White Female             50  <=50K  0.02179976           <=50K
# 48  54          Private              10 Married-civ-spouse White   Male             50  <=50K  0.56863587            >50K
# 52  39          Private               9           Divorced Black   Male             40  <=50K  0.03635984           <=50K
# 60  33          Private               9 Married-civ-spouse White   Male             40  <=50K  0.25158315           <=50K

# pick a random person
preds %>% 
  filter(age == 17) %>% 
  sample_n(1) %>% 
  gather()

#                 key               value
# 1              age                  17
# 2        workclass             Private
# 3  educational.num                   7
# 4   marital.status       Never-married
# 5             race               White
# 6           gender              Female
# 7   hours.per.week                  40
# 8           income               <=50K
# 9      probability 0.00657075044841042
# 10 predicted.class               <=50K

age = (0.032009 * 17)
workclass = 0.899126
educational.num = (0.384500 * 7)
marital.status = -0.389246
race = 0.511269
gender = 0
hours.per.week = (0.030979 * 40)

# the formula
# probability = 1 / (1 + exp(-x))
age + workclass + educational.num + marital.status + race + gender + hours.per.week # = 5.495962
intercept <- summary(model.1)$coefficients[1, 1] #-10.5145
x <- intercept + 5.495962 # -5.018541
1 / (1 + exp(-x)) # = 0.006570712 (that is correct)

# evaluation metrics ####
cm <- conf_mat(preds, truth = "income", estimate = "predicted.class")
cm

#            Truth
# Prediction <=50K >50K
#      <=50K  6844 1125
#      >50K    587 1213

# print all metrics
autoplot(cm)

summary(cm, event_level = 'second')

#   .metric              .estimator .estimate
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

confusionMatrix(data = preds$predicted.class, reference = preds$income)


# readings ####
# https://www.statisticshowto.com/log-odds/
# https://stats.oarc.ucla.edu/r/dae/logit-regression/
# https://www.guru99.com/r-generalized-linear-model.html
# https://www.displayr.com/how-to-interpret-logistic-regression-coefficients/
# https://towardsdatascience.com/an-introduction-to-logistic-regression-for-categorical-data-analysis-7cabc551546c
# https://www.statology.org/null-residual-deviance/
# https://stats.stackexchange.com/questions/108995/interpreting-residual-and-null-deviance-in-glm-r
# https://quantifyinghealth.com/interpret-logistic-regression-coefficients/
