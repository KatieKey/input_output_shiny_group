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


  dataz <- na.omit(efficacy_summary_file) %>% 
    dplyr::mutate(as.numeric(dosage)) %>% 
    dplyr::select_if(is.numeric) %>%
    dplyr::filter(dosage == 50)
  
  response <- dataz %>% 
    dplyr::select("ELU")
  
  predictors <- dataz %>%
    dplyr::select(c("PLA", "ULU", "RIM", "OCS", "ICS", "SLU", "SLE", "cLogP", "huPPB", 
                    "muPPB", "MIC_Erdman", 'MICserumErd', "MIC_Rv", "Caseum_binding", "MacUptake"))
  
  y <- as.numeric(unlist(response))
  x <- as.matrix(predictors)
  
  fit =  glmnet(x, y)
  
  coeff <- coef(fit,s=0.1)
  coeff <- as.data.frame(as.matrix(coeff)) %>% 
    rownames_to_column() 
  colnames(coeff) <- c("predictor", "coeff")
  
  coeff <- coeff %>% 
    dplyr::filter(coeff > 0)
  kable(coeff)
  
  class(coeff)
  
 ########## 
  
  function_data_ELU <- efficacy_summary_file %>% 
    filter(level == "Cmax") %>% 
    gather(key = independent_var, value = indep_measure, -drug, -dosage, -dose_int, -level, -ELU, -ESP, na.rm = TRUE) 
    
    function_data_ELU <- function_data_ELU %>% 
    select(drug, dosage, dose_int, level, ELU, indep_measure, independent_var) 
    
    estimate_results <- function_data_ELU %>% 
      group_by(independent_var, dose_int) %>% 
      nest() %>% 
      dplyr::mutate_if(mod_results = purrr::map(data, lm(ELU ~ scale(function_data_ELU$indep_measure)))) %>% 
      mutate(mod_coefs = purrr::map(mod_results, broom::tidy)) %>% 
      select_if(independent_var, dose_int, mod_results, mod_coefs) %>% 
      unnest(mod_coefs) %>% 
      filter(term == "scale(indep_measure)")
    
    coef_plot <- estimate_results %>%
      mutate(independent_var = forcats::fct_reorder(independent_var, estimate, fun = max)) %>%
      rename(Dose_Interval = dose_int) %>% 
      ggplot(aes(x = estimate, y = independent_var, color = Dose_Interval)) +
      geom_point(aes(size = 1 / std.error)) +
      scale_size_continuous(guide = FALSE) +
      theme_few() + 
      ggtitle(label = "Linear model coefficients as function of independent variables, \n by drug dose and model uncertainty", subtitle = "smaller points have more uncertainty than larger points") +
      geom_vline(xintercept = 0, color = "cornflower blue") 
    
    coef_plot
    
    variable_definitions <- paste0("https://raw.githubusercontent.com/dfat5/erhs_535_group3/",
                                   "master/data/variable_definitions.csv")
    variable_definitions <- read_csv(variable_definitions)
    kable(variable_definitions)
    View(variable_definitions) 

    
  