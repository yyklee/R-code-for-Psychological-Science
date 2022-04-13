# Logistic Regression: preparing data to creating plot

#### Prepare Data: Load Packages ####

library(tidyverse) 
library(lme4) 
library(nlme) 
library(emmeans) 
library(dplyr)
library(haven)
library(pscl)
library(MASS)

# Load and View Data 
MyData = read.csv()
MyData %>%
  View

################ Logistic Regression ###################

#1 Prepare Data 

# See if any predictors are already centered
MyData %>%
  summarise_at(c('var1', 'var2', 'var3', 'var4'), mean, na.rm=T)

# Center continuous predictors (if not centered )
MyData = MyData %>%
  mutate(c.var1 = var1 - mean(var1, na.rm=T),
         c.var2 = var2- mean(var2, na.rm = T),
         c.var3 = var3 - mean(var3, na.rm = T),
         c.var4 = var4 - mean(var4, na.rm = T)
  )

# Effect code categorical predictors
# See if categorical variables are numerical 
MyData %>%
  count(cat.var1)
MyData %>%
  count(cat.var2)


#2 Main analysis: Logistic Regression 

log_model = MyData %>%
  glm(dv1 ~ c.var1 + c.var2 + c.var3 + c.var4 + cat.var1 + cat.var2, 
      family =  binomial, data = .) 

logistic_model %>%
  summary

# Calculate the Odds Ratio of the outcome for a one-unit increase in each predictor
logistic_model %>%
  coefficients %>%
  exp

# Estimated means for plotting
emmeans(logistic_model,
        "c.var1",
        at = list(c.var1= c(sd(MyData$c.var1, na.rm = TRUE),
                                -1*sd(MyData$c.var1, na.rm = TRUE))))


# Figure
# Convert emmeans to probabilities
exp(0.2236295)/(1+exp(0.2236295))
exp(0.7363211)/(1+exp(0.7363211))

# Convert SE bounds to probabilities
exp(0.295+0.1158814)/(1+exp(0.2236295+0.1158814))
exp(0.2236295-0.1158814)/(1+exp(0.2236295-0.1158814))
exp(0.7363211+0.1210130)/(1+exp(0.7363211+0.1210130))
exp(0.7363211-0.1210130)/(1+exp(0.7363211-0.1210130))


# Create dataframe for plotting
dataframe <- read.table(text=
                          "x y", header= T)



# Create plot fill in 0 
library(ggplot2);
ggplot(dataframe, aes(as.factor(x), y), group =1) +
  geom_point() +
  ylim(0.4, 0.8) +
  geom_errorbar(ymin=c(0, 0), ymax=c(0, 0), width=.05)+
  labs(y="Probability of DV", x="iv")+
  geom_line(group=1)