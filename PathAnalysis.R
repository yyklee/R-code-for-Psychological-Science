### Path Analysis ###

#### Prepare Workspace ####
# Load Libraries
library(tidyverse)
library(lavaan)
library(semPlot)

# Read Data
interest_data = read_csv(file.choose()) 

# Look at the data
interest_data %>%
  View

# Theoretical model path analysis
# Model specification
model = '
M~ a*IV1 + b*IV2
DV ~ c*M
IV1.through.M:=a*c #looking at the paths
IV2.through.M:=b*c
'

# Estimate the path model
pmodel = model %>%
  sem(data = data)

# Print results, including the fit statistics
pmodel %>%
  summary(fit.measures = TRUE, 
          standardized = TRUE)

# Figure 1: unconstrained
pmodel %>%
  semPaths(whatLabels = "est", 
           rotation = 2, 
           nCharNodes = 0, 
           label.cex = 1
  )

locations = matrix(c(  0,   0, 
                       .5,   0, 
                       -.5,  .5, 
                       -.5,   0, 
                       -.5, -.5), 
                   ncol=2, 
                   byrow=2
)
labels = c("M",
           "dv",
           "iv1",
           "iv2",
          
)

pmodel %>%
  semPaths(whatLabels="est", 
           nodeLabels = labels, 
           layout = locations, 
           sizeMan = 12
  )

# Constrained model path analysis
# Model specification
model_constrained = '
M~ a*IV1 + b*IV2
DV ~ c*M
IV2.through.M:=b*c
'

# Estimate the path model
model_constrained = model_constrained %>%
  sem(data = data)

# Print results, including the fit statistics
model_constrained %>%
  summary(fit.measures = TRUE, 
          standardized = TRUE)

# Figure 2: constrained
model_constrained %>%
  semPaths(whatLabels = "est", 
           rotation = 2, 
           nCharNodes = 0, 
           label.cex = 1
  )

locations = matrix(c(  0,   0, 
                       .5,   0, 
                       -.5,  .5, 
                       -.5,   0, 
                       -.5, -.5), 
                   ncol=2, 
                   byrow=2
)
labels = c("M",
           "dv",
           "iv1",
           "iv2",
           
)

model_constrained %>%
  semPaths(whatLabels="est", 
           nodeLabels = labels, 
           layout = locations, 
           sizeMan = 12
  )


# Compare the two models (likelihood ratio test)
anova(pmodel, model_constrained)



