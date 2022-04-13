# Poisson Regression: preparing data to creating plot
# Prepare Data  

# Effect code categorical predictors

Data %>%
  count(cat.v1)

# Effect-code cat.v1
Data$cat.v1 = as.factor(Data$cat.v1)
contrasts(Data$cat.v1) = contr.sum(3)

# Run the Poisson Regression
poisson_model = Data %>%
  glm(dv ~ c.v1 + c.v2 + c.v3 + c.v4 + cat.v1 + cat.v2, 
      family = poisson, 
      data = .)
poisson_model %>%
  summary


# Estimated means for plotting
emmeans(poisson_model,
        "cat.v1",
        at = list(sex = c(-1*sd(Data$cat.v1, na.rm = TRUE),
                          sd(Data$cat.v1, na.rm = TRUE))))


# Figure
# Convert emmeans to probabilities
exp(0.7675235)
exp(0.5006234)

# Convert SE bounds to probabilities
exp(0.4678375+0.06768608)
exp(0.4678375-0.06768608)
exp(0.7006937+0.07029666)
exp(0.7006937-0.07029666)

# Create data frame for plotting
dataframe2 <- read.table(text=
                           "x y
                        cat.v1_1 1.5965
                        cat.v1_2 2.0151", header= T)

# Create plot
library(ggplot2);
ggplot(dataframe2, aes(as.factor(x), y), group =1) +
  geom_point() +
  ylim(1 ,2.5) +
  geom_errorbar(ymin=c(1.492051, 1.878356), ymax=c(1.708342, 2.161906), width=.05)+
  labs(y="dv1", x="v1")+
  geom_line(group=1)



