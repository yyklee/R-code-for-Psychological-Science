###  Multilevel Model ###

#### Prepare Work space ####
# Load Packages
library(tidyverse) 
library(lme4) 
library(nlme) 
library(emmeans) 
library(e1071)
library(dplyr)
library(summarytools)

# Read Data
Data = read.csv()

# Look at the data
Data %>%
  View

#  Prepare Data
# Check skewness --> 

#When the means are proportional to the variances: root 
#when positively skewed: log

Data %>%
  pull(v1) %>%
  skewness(na.rm = TRUE)

Data %>%
  mutate(log_v1 = log(v1+1)) %>%
  pull(log_money) %>%
  skewness(na.rm = TRUE)

Data %>%
  pull(v2) %>%
  skewness(na.rm = TRUE)

Data %>%
  mutate(root_v2 = v2**(1/2)) %>%
  pull(root_v2) %>%
  skewness(na.rm = TRUE)

Data = Data %>%
  mutate(log_money = log(money+1))



# Grand and Group Mean Center Predictors
Data = Data %>% 
  mutate(c.log_v1 = log_v1 - mean(log_v1, na.rm = TRUE)) %>% #Grand mean center
  group_by(state) %>% 
  mutate(c.root_v2 = root_v2 - mean(root_v2, na.rm = TRUE)) %>%  #Group center v
  ungroup



# Random intercept MLM
random_intercept_model = Data %>%
  lmer( dv ~ (1|state) + c.root_v2 + c.log_v1,
        na.action = "na.exclude", data = .)

random_intercept_model %>%
  summary
Rintercept_emmeans = random_intercept_model %>%
  emmeans(c("centered_root_v2", "centered_log_v1"), 
          at = list(centered_root5_acres = c(-1*sd(Data$c.root_v2, na.rm = TRUE), 
                                             sd(Data$c.root_v2, na.rm = TRUE)),
                    centered_log_money    = c(-1*sd(Data$c.log_v1, na.rm = TRUE), 
                                              sd(Data$c.log_v1, na.rm = TRUE)))
  )


# Random slope MLM
random_slope_model = Data %>%
  lmer( voting ~ (1+c.log_v1|state) + c.root_v2 + c.log_v1,
        na.action = "na.exclude", data = .)
random_slope_model %>%
  summary


# Model Comparison
anova(random_intercept_model, random_slope_model)




# Calculate the ICC 
calculate_icc_lmer = function(x) { # x is the MLM model object for your baseline model, created by lmer
  random_effects = data.frame(VarCorr(x))
  intercept_variance  = random_effects[random_effects$var1 == "(Intercept)", 'vcov'][1]
  residual_variance  = random_effects[random_effects$grp == "Residual", 'vcov']
  icc  = intercept_variance / (residual_variance + intercept_variance)
  return(icc)
}
random_slope_model %>% 
  calculate_icc_lmer




# Calculate Effect Sizes
# Print results to calculate partial R^2 values manually
library(car)
random_slope_model %>%
  Anova(type = 3, test = "F")




# Function to automatically calculate partial R2 for each predictor
calculate_partial_R2_lmer = function(x) { # x is any MLM model object created by lmer
  require(car)
  results = data.frame(Anova(x, type = 3, test = "F"))[-1,]
  parameters = row.names(results)
  numerator_df = results["Df"]
  denominator_df = results["Df.res"]
  F_values = results["F"]
  partial_R2 = (((numerator_df/denominator_df)*F_values)/(1+((numerator_df/denominator_df)*F_values)))
  names(partial_R2)[1] = "Partial_R2"
  return(partial_R2)
}
random_slope_model %>%
  calculate_partial_R2_lmer




# Figure
Rslope_emmeans <- random_slope_model %>%
  emmeans(c("centered_root5_acres", "centered_log_money"), 
          at = list(centered_root5_acres = c(-1*sd(Data$c.root_v2, na.rm = TRUE), 
                                             sd(Data$c.root_v2, na.rm = TRUE)),
                    centered_log_money    = c(-1*sd(Data$c.log_v1, na.rm = TRUE), 
                                              sd(Data$c.log_v1, na.rm = TRUE))))

emmip(Rslope_emmeans, c.root_v2~c.log_v1, xlab= "v1", ylab= "dv", CIs = TRUE)