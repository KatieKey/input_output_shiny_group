# Efficacy_Function cleans "efficacy" data 
library(dplyr)

efficacy_function <- function(efficacy_df){
  efficacy_clean <- efficacy_df %>% 
    select(Protocol_Animal, Compound, Group, Drug_Dose, Days_Treatment,
           Treatment_Interval,Elung,Espleen) %>% 
    rename(lung_efficacy = Elung,
           spleen_efficacy = Espleen,
           dosage = Drug_Dose,
           days_treatment = Days_Treatment,
           dose_interval = Treatment_Interval, 
           drug = Compound) %>%
    mutate(lung_efficacy = as.numeric(lung_efficacy)) %>% 
    mutate(spleen_efficacy = as.numeric(spleen_efficacy)) %>%
    mutate(dose_interval = as.factor(dose_interval)) %>%
    mutate(days_treatment = as.factor(days_treatment)) %>% 
    group_by(Protocol_Animal, drug, Group, dosage, days_treatment, dose_interval) %>% 
    summarize(lung_efficacy_log = log10(lung_efficacy),
              spleen_efficacy_log = log10(spleen_efficacy))
  
  levels(efficacy_clean$dose_interval)[levels(efficacy_clean$dose_interval)=="Pre Rx 9 week"] <- "_Baseline"
  levels(efficacy_clean$dose_interval)[levels(efficacy_clean$dose_interval)=="M-F"] <- "_QID"
  levels(efficacy_clean$dose_interval)[levels(efficacy_clean$dose_interval)=="4 wk"] <- "20_Control"
  levels(efficacy_clean$dose_interval)[levels(efficacy_clean$dose_interval)=="8 wk"] <- "40_Control"
  levels(efficacy_clean$drug)[levels(efficacy_clean$drug)==""] <- "Baseline"
  
  
  efficacy_clean <- efficacy_clean %>% 
    unite(days_dose, days_treatment, dose_interval, sep = "") %>% 
    separate(days_dose, c("days", "dose"), sep = "_") %>% 
    rename("days_treatment" = days,
           "dose_interval" = dose) %>% 
    mutate(days_treatment = as.numeric(days_treatment))

  return(efficacy_clean)
}

# plasma_function cleans "plasma" Excel spreadsheet
plasma_function <- function(plasma_df){
  plasma_clean <- plasma_df %>%
    select(MouseID, 
           Compound, 
           Group, 
           Protocol_Animal, 
           Dosing, 
           Timepoint, 
           Plasma_Parent) %>%
    rename(drug = Compound, 
           mouse_number = MouseID, 
           plasma_concentration = Plasma_Parent)
  return(plasma_clean)
}


# tissue_laser_ function cleans the Laser Capture Excel spreadsheet

tissue_laser_function <- function(tissue_laser_df) {
  tissue_laser_clean <- tissue_laser_df %>%
    rename(`Parent [ng/ml]` = Parent) %>%
    select(-StudyID, -Metabolite, - Units, - Collection, - `Sample ID`)
  
  n <- nrow(tissue_laser_clean)
  mice_ids <- rep(c(1:(n/4)), each = 4)
  
  tissue_laser_clean <- mutate(tissue_laser_clean, MouseID = mice_ids) %>%
    spread(key = Compartment, value = `Parent [ng/ml]`) %>%
    rename(ULU = `uninvolved lung`, RIM = rim,
           OCS = `outer caseum`, ICS = `inner caseum`) %>%
    mutate(ULU = as.numeric(ULU), RIM = as.numeric(RIM),
           OCS = as.numeric(OCS), ICS = as.numeric(ICS))
  return(tissue_laser_clean)
}



# pk_function cleans PK data
tissue_std_pk_function <- function(tissue_std_pk_df){  
  tissue_std_pk_clean <- tissue_std_pk_df %>%
  rename(mouse_number = mice_ids) %>%
  select(Compound, mouse_number, Group, Protocol_Animal, Dosing, Timepoint, Compartment, Parent) %>%
  rename(drug = Compound,
         `Parent [ng/ml]` = Parent) %>% 
  spread(key = Compartment, value = `Parent [ng/ml]`) %>% 
  rename(SLU = Lung, 
         SLE = Lesion) %>% 
  mutate(SLU = as.numeric(SLU),
         SLE = as.numeric(SLE))
return(tissue_std_pk_clean)
} 

#in_vitro_function cleans in_vitro data
in_vitro_function <- function(in_vitro_df){
  in_vitro_clean <- in_vitro_df 
  return(in_vitro_clean)
  
} 
