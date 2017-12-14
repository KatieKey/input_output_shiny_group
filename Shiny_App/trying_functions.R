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
    
#######
    
    example_data <- efficacy_summary %>% 
      select(drug, dosage, dose_int, level, PLA, SLU, SLE) %>% 
      unite(drug_dosing, drug, dosage, dose_int, sep = "-") %>% 
      filter(level == "Trough") %>% 
      gather(PLA:SLE, key = "ELEMENT", value = concentration) %>% 
      mutate(ELEMENT = factor(ELEMENT, levels = c("PLA", "SLU", "SLE"),
                              labels = c("MOUSE", "LUNGS", "LESION")),
             ELEMENT = as.character(ELEMENT))
    
    mouse <- paste0("https://raw.githubusercontent.com/KatieKey/",
                      "input_output_shiny_group/master/Shiny_App/MouseCoord.csv")
    mouse <- read_csv(mouse)
    
    mouse <- mouse %>% 
      dplyr::select("ELEMENT","HOLE","X","Y") %>% 
      left_join(example_data, by = "ELEMENT")
    
    
    #Plot drug distribution, facetted by drug_dosing
    mouse_plot <- mouse %>% 
      ggplot(aes(mapping = TRUE, x = X, y = Y, group = HOLE, 
                               fill = concentration)) +
      geom_polypath(rule = "evenodd") +
      geom_path(colour = "black", size = .5) +
      theme_void() +
      theme(legend.position = 'right') +
      labs(title = "Biodistribution by drug and dosage", 
           subtitle = "For plasma, standard lung, and standard lesion concentrations") +
      coord_fixed()  +
      scale_fill_viridis(option = "magma") + 
      facet_wrap(~ drug_dosing)
    
    mouse_plot
    
####
    
    in_vivo <- efficacy_summary %>%
      select(drug, dosage, dose_int, PLA, ULU, RIM, OCS, ICS, SLU, SLE) %>% 
      rename(Drugs = "drug") %>% 
      unite(dosage_interval, dosage:dose_int, sep = "") #brings together dosage (50 & 100) with intervals (BID & QD) 
    
    head(in_vivo)
    
    #gather for small multiples in-vivo markers
    in_vivo_SM <- in_vivo %>% 
      gather(key = variable, value = value, -Drugs, -dosage_interval) %>% #allows us to make small multiples by combining all the values into one column and the variable they belong to in another column 
      mutate(variable = factor(variable, levels = c("RIM", "OCS","ICS","ULU","SLU","SLE","PLA"),
                               labels = c("Rim (of lesion)","Outer Caseum","Inner Caseum","Uninvolved Lung", "Standard Lung", "Standard Lesion", "Plasma"))) %>% 
      mutate(dosage_interval = factor(dosage_interval, levels = c("50BID","100QD"))) #prevents the numeric scaling of the x axis values, so it spaces it like two factors in the order given above
    
    head(in_vivo_SM)
    
    #plot small multiples in-vivo markers 
    in_vivo_SMplot <- in_vivo_SM %>% 
      ggplot(aes(x = dosage_interval, y = value, color = Drugs))+ #main structure of x & y axis
      geom_beeswarm(alpha = 0.5, size = 1.5)+ #incorporates beeswarm plot style 
      scale_y_log10()+ #log scale, used for the invivo group to cover wide range of values 
      labs(x = 'Dosage-Interval', y = 'Value')+ 
      ggtitle('In-Vivo Distribution of TB Drugs')+ 
      theme_few()+
      facet_wrap(~ variable, ncol = 4) #creates small multiples by variable 
    
    in_vivo_SMplot 
    
#####
    
    example_data <- efficacy_summary %>% 
      select(drug, dosage, level, PLA, SLU, SLE, ULU, RIM, OCS, ICS) %>% 
      unite(drug_dosing, drug, dosage, sep = "-") %>% 
      filter(level == "Cmax") %>% 
      gather(PLA:ICS, key = "ELEMENT", value = concentration) %>% 
      mutate(ELEMENT = factor(ELEMENT, levels = c("PLA", "SLU", "SLE", "ULU", "RIM", "OCS", "ICS"),
                              labels = c("PLA", "SLU", "SLE", "ULU", "RIM", "OCS", "ICS")),
             ELEMENT = as.character(ELEMENT))
    
    mouse <- paste0("https://raw.githubusercontent.com/KatieKey/input_output_shiny_group/",
                    "master/Shiny_App/ModelCoord.csv")
    mouse <- read_csv(mouse) %>% 
      dplyr::select("ELEMENT","HOLE","X","Y")
      
      mouse <- mouse %>% 
      left_join(example_data, by = "ELEMENT")
    
    
    #Plot drug distribution, facetted by drug_dosing
    mouse_plot <- mouse %>% 
      ggplot(aes(mapping = TRUE, x = X, y = Y, group = HOLE, 
                 fill = concentration)) +
      geom_polypath(rule = "evenodd") +
      geom_path(colour = "black", size = .5) +
      geom_segment(x=-6, y=-2, xend=-28, yend=12) +
      geom_segment(x=-6, y=-4, xend=-28, yend=-12) +
      theme_void() +
      theme(legend.position = 'right') +
      labs(title = "Biodistribution by drug and dosage", 
           subtitle = "For plasma (mouse), standard lung (lungs), standard lesion (small lesion), uninvolved lung \n(box inset), lesion rim (inset), outer caseum (inset) and inner caseum (inset) concentrations \n", 
           caption = paste("prepared ", Sys.Date() )) + 
      coord_fixed()  +
      scale_fill_viridis(option = "magma") + 
      facet_wrap(~ drug_dosing)
    
    mouse_plot
    