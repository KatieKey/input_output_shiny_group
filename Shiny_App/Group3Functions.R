
#FROM: https://github.com/dfat5/erhs_535_group3

##regression tree function
regression_tree_function <- function(dep_var, efficacy_summary_file) {
  
  if (dep_var == "ELU") {
    
    function_data <- efficacy_summary_file %>%
      filter(!is.na(ELU))
    
    tree <- rpart(ELU ~  drug + dosage + level + 
                    PLA + ULU + RIM + OCS + ICS + SLU + SLE + 
                    cLogP + huPPB + muPPB + MIC_Erdman + MICserumErd + MIC_Rv + 
                    Caseum_binding + MacUptake,
                  data = function_data, 
                  control = rpart.control(cp = -1))
    return(tree)
  }
  
  if (dep_var == "ESP") {
    
    function_data <- efficacy_summary_file %>%
      filter(!is.na(ESP))
    
    tree <- rpart(ESP ~  drug + dosage + level + 
                    PLA + ULU + RIM + OCS + ICS + SLU + SLE + 
                    cLogP + huPPB + muPPB + MIC_Erdman + MICserumErd + MIC_Rv + 
                    Caseum_binding + MacUptake,
                  data = function_data, 
                  control = rpart.control(cp = -1))
    return(tree)
  }
}

##


