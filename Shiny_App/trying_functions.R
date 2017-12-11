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
  
 efficacy_summary <- read_csv("~/Documents/Colorado State/R_Programming_Directory/input_output_shiny_group/CSV_Files/efficacy_summary.csv")
  
    function_data_ELU <- efficacy_summary %>% 
      dplyr::filter(level == "Cmax") %>% 
      gather(key = independent_var, value = indep_measure, -drug, -dosage, -dose_int, 
             -level, -ELU, -ESP, na.rm = TRUE)
    
    function_data_ELU <- function_data_ELU %>% 
      dplyr::select(drug, dosage, dose_int, level, ELU, indep_measure, independent_var) 

      model_results <- lm(function_data_ELU$ELU ~ scale(function_data_ELU$indep_measure))
    
    estimate_results_ELU <- function_data_ELU %>% 
      group_by(independent_var, dose_int) %>% 
      nest() %>%
      dplyr::mutate(mod_results = purrr::map(data, model_results)) %>% 
      dplyr::mutate(mod_coefs = purrr::map(mod_results, broom::tidy)) %>% 
      dplyr::select(independent_var, dose_int, mod_results, mod_coefs) %>% 
      unnest(mod_coefs) %>% 
      filter(term == "scale(indep_measure)")
    ####issue is here in code - I think with the map function
    
    coef_plot_ELU <- estimate_results_ELU %>%
      mutate(independent_var = forcats::fct_reorder(independent_var, estimate, fun = max)) %>%
      rename(Dose_Interval = dose_int) %>% 
      ggplot(aes(x = estimate, y = independent_var, color = Dose_Interval)) +
      geom_point(aes(size = 1 / std.error)) +
      scale_size_continuous(guide = FALSE) +
      theme_few() + 
      ggtitle(label = "Linear model coefficients as function of independent variables, \n by drug dose and model uncertainty", subtitle = "smaller points have more uncertainty than larger points") +
      geom_vline(xintercept = 0, color = "cornflower blue") 
    
    return(coef_plot_ELU)
    
    
#######
    
    dataz <- efficacy_summary %>% 
      na.omit(efficacy_summary) %>% 
      dplyr::mutate(as.numeric(as.integer(dosage))) %>% 
      dplyr::select_if(is.numeric) %>%
      filter(dosage == 50)
    
    response <- dataz %>% 
      dplyr::select(input$variable_lasso)
    
    predictors <- dataz %>%
      dplyr::select(c("PLA", "ULU", "RIM", "OCS", "ICS", "SLU", "SLE", "cLogP", "huPPB", 
                      "muPPB", "MIC_Erdman", 'MICserumErd', "MIC_Rv", "Caseum_binding", "MacUptake"))
    
    y <- as.numeric(unlist(response))
    x <- as.matrix(predictors)
    
    fit =  glmnet(x, y)
    
    coeff_50 <- coef(fit,s=0.1)
    coeff_50 <- as.data.frame(as.matrix(coeff_50)) %>% 
      rownames_to_column() 
    colnames(coeff_50) <- c("predictor", "coeff")
    
    coeff_50 <- coeff_50 %>% 
      dplyr::filter(coeff_50 > 0)
    coeff_50
    
#####
    
    dataz_1 <- efficacy_summary %>% 
      na.omit(efficacy_summary) %>% 
      dplyr::mutate(dosage = as.numeric(as.integer(dosage))) %>% 
      dplyr::select_if(is.numeric) %>%
      filter(dosage == 100)
    
    response_1 <- dataz_1 %>% 
      dplyr::select("ELU")
    
    predictors_1 <- dataz_1 %>%
      dplyr::select(c("PLA", "ULU", "RIM", "OCS", "ICS", "SLU", "SLE", "cLogP", "huPPB", 
                      "muPPB", "MIC_Erdman", 'MICserumErd', "MIC_Rv", "Caseum_binding", "MacUptake"))
    
    y <- as.numeric(unlist(response_1))
    x <- as.matrix(predictors_1)
    
    fit =  glmnet(x, y)
    
    coeff <- coef(fit,s=0.1)
    coeff <- as.data.frame(as.matrix(coeff)) %>% 
      rownames_to_column() 
    colnames(coeff) <- c("predictor", "coeff")
    
    coeff <- coeff %>% 
      dplyr::filter(coeff > 0)
   coeff
    
  