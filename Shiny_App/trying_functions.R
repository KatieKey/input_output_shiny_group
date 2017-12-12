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
  
 
    
    function_data <- efficacy_summary %>% 
      filter(level == "Cmax") %>% 
      gather(key = independent_var, value = indep_measure, -drug, -dosage, 
             -dose_int, -level, -ELU, -ESP, na.rm = TRUE) %>% 
      select(drug, dosage, dose_int, level, ELU, indep_measure, independent_var) 
    
    if(dep_var=="ELU") {function_data$vect <- function_data$ELU}
    if(dep_var=="ESP") {function_data$vect <- function_data$ESP}
    
    model_function <- function(data) {
      model_results <- lm(vect ~ scale(indep_measure), data = data)
    }
    
    estimate_results <- function_data %>% 
      group_by(independent_var, dose_int) %>% 
      nest() %>% 
      mutate(mod_results = purrr::map(data, model_function)) %>% 
      mutate(mod_coefs = purrr::map(mod_results, broom::tidy)) %>% 
      select(independent_var, dose_int, mod_results, mod_coefs) %>% 
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
    