# FUNCTIONS FOR CLEANING RAW DATA FILES

library(dplyr)

#### efficacy_function cleans raw efficacy data in Shiny app
# Function Title: Cleaning Efficacy Dataframe

# This function uses the file input from the fileInput widget for "efficacy" as the argument. 
# The dataframe explores lung and spleen efficacies by drug, days of treatment, and dosage. The function 
# cleans the plasma dataframe by first removing columns that are repeating (i.e., units) and putting
# the efficacy values into a log value for easier comprehension. Further, the dosage and days_treatment columns
# were cleaned by changing the factor names in order to compare by dosage and include controls in this analysis.

library(dplyr)

efficacy_function <- function(efficacy_df){
  efficacy_clean <- efficacy_df %>% 
    select(Protocol_Animal, Compound, Group, Drug_Dose, Days_Treatment,
           Treatment_Interval, Dose_Frequency, Elung, Espleen) %>% 
    rename(lung_efficacy = Elung,
           spleen_efficacy = Espleen,
           dosage = Drug_Dose,
           days_treatment = Days_Treatment,
           dose_interval = Dose_Frequency, 
           drug = Compound) %>%
    mutate_at(c("lung_efficacy", "spleen_efficacy"), as.numeric) %>% 
    mutate_at(c("dose_interval", "days_treatment"), as.factor) %>%
    mutate(drug = ifelse(Group == "PreRX", "Baseline", drug)) %>% 
    mutate(lung_efficacy_log = log10(lung_efficacy),
           spleen_efficacy_log = log10(spleen_efficacy)) %>% 
    mutate(days_treatment = as.numeric(days_treatment))
  
  return(efficacy_clean)
}

efficacy_summary <- function(efficacy_clean){
  untreated <- efficacy_clean %>% 
    filter(drug == "Untr") %>% 
    summarize_at(c("lung_efficacy_log", "spleen_efficacy_log"), mean)
  
  efficacy_clean_summarized <- efficacy_clean %>% 
    group_by(drug, dosage, dose_interval) %>%
    summarize_at(c("lung_efficacy_log", "spleen_efficacy_log"), mean, na.rm = TRUE) %>% 
    filter(!(drug %in% c("Baseline", "Untr"))) %>% 
    mutate(ELU = untreated$lung_efficacy_log - lung_efficacy_log,
           ESP = untreated$spleen_efficacy_log - spleen_efficacy_log,
           dose_interval = as.character(dose_interval)) %>% 
    select(drug, dosage, dose_interval, ELU, ESP) %>% 
    rename(dose_int = dose_interval)
  
  return(efficacy_clean_summarized)
}

#### plasma_function cleans raw plasma data in Shiny app
#Function Title: Cleaning Plasma Dataframe

#This function has a dataframe as an argument. The dataframe contains data on plasma
#concentrations. The function cleans the plasma dataframe by selecting only the needed
#variables, renaming variables, and changing the group column to a character.

plasma_function <- function(plasma_df){
  plasma_clean <- plasma_df %>%
    select(MouseID, 
           Compound, 
           Drug_Dose,
           Dose_Frequency,
           Group, 
           Protocol_Animal, 
           Dosing, 
           Timepoint, 
           Plasma_Parent) %>%
    rename(drug = Compound, 
           mouse_number = MouseID, 
           plasma_concentration = Plasma_Parent,
           dosage = Drug_Dose,
           dose_int = Dose_Frequency)  %>%
    mutate(Group = as.character(Group))
  return(plasma_clean)
}

plasma_summarize <- function(plasma_clean){
  plasma_summarized <- plasma_clean %>% 
    filter(Dosing == "Steady_State") %>% 
    group_by(drug, dosage, dose_int, Timepoint) %>% 
    summarize(PLA = mean(plasma_concentration, na.rm = TRUE)) %>% 
    rename(level = Timepoint) %>% 
    mutate(level = ifelse(level == "CMax", "Cmax", level))
  
  return(plasma_summarized)
}

##### Clean the tissue laser data into a tidy format

# 
tissue_laser_function <- function(tissue_laser_df) {
  tissue_laser_clean <- tissue_laser_df %>%
    rename(`Parent [ng/ml]` = Parent) %>%
    select(MouseID, Compound, Drug_Dose, Dose_Frequency, Group, Protocol_Animal,
           Dosing, Timepoint, Timepoint_Hours, Compartment, `Parent [ng/ml]`)
  
  tissue_laser_clean <- tissue_laser_clean %>%
    filter(Dosing == "Steady_State") %>% 
    spread(key = Compartment, value = `Parent [ng/ml]`) %>%
    rename(ULU = `uninvolved lung`, RIM = rim,
           OCS = `outer caseum`, ICS = `inner caseum`) %>%
    mutate_at(c("ULU", "RIM", "OCS", "ICS"), as.numeric)
  return(tissue_laser_clean)
}

tissue_laser_summary <- function(tissue_laser_clean){
  tissue_laser_summarized <- tissue_laser_clean %>% 
    group_by(Compound, Drug_Dose, Dose_Frequency, Timepoint) %>% 
    summarize_at(c("ULU", "RIM", "OCS", "ICS"), .funs = mean, na.rm = TRUE) %>% 
    ungroup() %>% 
    rename(drug = Compound,
           dosage = Drug_Dose, 
           dose_int = Dose_Frequency,
           level = Timepoint)
  return(tissue_laser_summarized)
}

##### tissue_std_pk_function cleans raw tissue std pk data in Shiny app

#Function Title: Clean STD PK Dataframe
#The argument for this function contains information on pharmacokinetic properties of the 
#drugs tested on a mouse-by-mouse level. A mouse id was created as a new column to the
#dataset. Additionally, only the necessary columns were included in the dataframe. The spread
#function was used to convert the Comparment column into columns for each compartment, 
#containing the respective Parent values. These new columns were then renamed to match the 
#SLE and SLU variable names in the tidy data templates and recoded as numerical values.

tissue_std_pk_function <- function(tissue_std_pk_df){
  tissue_std_pk_clean <- tissue_std_pk_df %>% 
    mutate(mouse_number = MouseID) %>%
    select(Compound, mouse_number, Drug_Dose, Dose_Frequency, Group, 
           Protocol_Animal, Dosing, Timepoint, Compartment, Parent) %>%
    rename(drug = Compound,
           `Parent [ng/ml]` = Parent) %>% 
    spread(key = Compartment, value = `Parent [ng/ml]`) %>% 
    rename(SLU = Lung, 
           SLE = Lesion) %>% 
    mutate_at(c("SLU", "SLE"), as.numeric) %>% 
    filter(Dosing == "Steady_State")
  return(tissue_std_pk_clean)
} 

tissue_std_pk_summarize <- function(tissue_std_pk_clean){
  tissue_std_pk_summarized <- tissue_std_pk_clean %>% 
    group_by(drug, Drug_Dose, Dose_Frequency, Timepoint) %>% 
    summarize_at(c("SLE", "SLU"), mean, na.rm = TRUE) %>% 
    ungroup() %>% 
    rename(dosage = Drug_Dose, 
           dose_int = Dose_Frequency,
           level = Timepoint)
  return(tissue_std_pk_summarized)
}

###### in_vitro_function cleans raw in_vitro data in Shiny app
in_vitro_function <- function(in_vitro_df){
  in_vitro_clean <- in_vitro_df %>% 
    rename(drug = Drug) %>% 
    mutate_at(c("huPPB", "MIC_Rv", "MacUptake"), as.numeric)
  return(in_vitro_clean)
}

###### create summary dataframe
create_summary_df <- function(efficacy_clean_summarized, 
                              plasma_summarized,
                              tissue_laser_summarized,
                              tissue_std_pk_summarized,
                              in_vitro_clean){
  summary_df <- plasma_summarized %>% 
    full_join(tissue_laser_summarized, by = c("drug", "dosage", "dose_int", "level")) %>% 
    full_join(tissue_std_pk_summarized, by = c("drug", "dosage", "dose_int", "level")) %>%
    ungroup() %>% 
    mutate(dosage = as.character(dosage)) %>% 
    full_join(efficacy_clean_summarized, by = c("drug", "dosage", "dose_int")) %>% 
    left_join(in_vitro_clean, by = "drug") 
  
  return(summary_df)
}