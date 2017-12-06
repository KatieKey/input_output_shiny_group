
library(shiny)

# put back into Shiny app:
# source("other_groups_functions.R")
# fix "drug" checkbox

efficacy_summary_file_1 <- paste0("https://raw.githubusercontent.com/KatieKey/input_output_shiny_group/",
                                  "master/CSV_Files/efficacy_summary.csv")
efficacy_summary_file <- read_csv(efficacy_summary_file_1)


####Function for Beeswarm In Vitro 

invitro_beeswarm_function <- function(efficacy_summary_file) {
  
  drugs_selected <- input$CheckBeeDrugInVitro
  variables <- input$CheckBeeVarInVitro 
  
  in_vitro <- efficacy_summary_file %>%
    rename(Drugs = "drug") %>% 
    unite(dosage_interval, dosage:dose_int, sep = "")
  
  in_vitro_SM <- in_vitro %>% 
    gather(key = variable, value = value, -Drugs, -dosage_interval) %>% 
    mutate(variable_filtered = variable) %>% 
    mutate(variable = factor(variable, levels = c("Caseum_binding", "cLogP", "huPPB", "muPPB", "MIC_Erdman",
                                                  "MICserumErd", "MIC_Rv", "MacUptake"),
                             labels = c("Caseum \nBinding", "cLogP", 
                                        "Human \nPlasma \nBinding", "Mouse \nPlasma \nBinding", 
                                        "MIC Erdman \nStrain", "MIC Erdman \nStrain \nwith Serum", "MIC Rv Strain",
                                        "Macrophage \nUptake (Ratio)"))) %>% 
    mutate(dosage_interval = factor(dosage_interval, levels = c("50BID", "100QD"))) %>% 
    select(variables) %>%
    filter(Drugs %in% c(drugs_selected))
  
  
  if(!is.null(variables)) {
    in_vitro_SM <- in_vitro_SM %>% 
      dplyr::filter(variable_filtered %in% variables)
  }
  
  if(!is.null(drugs)) {
    in_vitro_SM <- in_vitro_SM %>%
      dplyr::filter(Drugs %in% drugs)
  }
  
  
  in_vitro_SMplot <- in_vitro_SM %>% 
    ggplot(aes(x = dosage_interval, y = value, color = Drugs))+
    geom_beeswarm(alpha = 0.5, size = 1.5)+
    labs(x = 'Dosage-Interval', y = 'Value')+
    ggtitle('In-Vitro Distribution of TB Drugs')+
    theme_few()+
    facet_wrap(~ variable, ncol = 4, scale="free")
  
  ggplotly(in_vitro_SMplot)
  
  return(ggplotly(in_vitro_SMplot))
  
}

invitro_beeswarm_function(efficacy_summary_file)

