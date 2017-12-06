library(readxl)
library(readr)
library(tidyr)
library(utils) 
library(dplyr)
library(shiny)
library(DT) 
library(visdat)
library(ggplot2)
library(ggthemes)
library(rpart)
library(ggbeeswarm)
library(plotly)
library(colourpicker)
library(rpart.plot)
library(party)
library(randomForest)
library(tibble)
library(glmnet)
library(knitr)
library(broom)
library(ggfortify)
library(stats)

data <- na.omit(efficacy_summary_file) %>% 
  select_if(is.numeric) %>%
  filter(dosage == 100)

response <- efficacy_summary_file %>% 
  select(ESP)

predictors <- efficacy_summary_file %>%
  select(c("PLA", "ULU", "RIM", "OCS", "ICS", "SLU", "SLE", "cLogP", "huPPB", 
           "muPPB", "MIC_Erdman", 'MICserumErd', "MIC_Rv", "Caseum_binding", "MacUptake"))

y <- as.numeric(unlist(response))
x <- as.matrix(predictors)

fit <-  glmnet(x, y)

#for ELU Error in if (nulldev == 0) stop("y is constant; gaussian glmnet fails at standardization step") : 
# missing value where TRUE/FALSE needed

coeff <- coef(fit,s=0.1)
coeff <- as.data.frame(as.matrix(coeff))

coeff <- coeff %>% 
  filter(coeff > 0)
return(kable(coeff))

