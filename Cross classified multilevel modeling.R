# Cross-classified model
cross_classified_model = Data %>%
  lmer( dv ~ (1|participant/lv2a) + (1|participant/lv2b) + 
          v1*v2, 
        na.action="na.exclude", data=.)

cross_classified_model %>% 
  summary

# Cross-classified model figure
plot_cross <- cross_classified_model %>%
  emmeans(c("v1", "v2"),
          at = list(c.v1 = c(-1, 1)),
                    v2= c(-1, 1)) 
  

emmip(plot_cross, v1~v2, ylab = "dv", CIs = TRUE)


# Calculate the ICC 
calculate_icc_lmer = function(x) { # x is the MLM model object for your baseline model, created by lmer
  random_effects = data.frame(VarCorr(x))
  intercept_variance  = random_effects[random_effects$var1 == "(Intercept)", 'vcov'][1]
  residual_variance  = random_effects[random_effects$grp == "Residual", 'vcov']
  icc  = intercept_variance / (residual_variance + intercept_variance)
  return(icc)
}
cross_classified_model %>% 
  calculate_icc_lmer

# Calculate Effect Sizes
library(car)
cross_classified_model %>%
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
cross_classified_model %>%
  calculate_partial_R2_lmer