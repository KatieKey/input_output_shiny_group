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
           spleen_efficacy_log = log10(spleen_efficacy))
  
  levels(efficacy_clean$dose_interval)[levels(efficacy_clean$dose_interval)=="Pre Rx 9 week"] <- "_Baseline"
  levels(efficacy_clean$dose_interval)[levels(efficacy_clean$dose_interval)=="M-F"] <- "_QD"
  levels(efficacy_clean$dose_interval)[levels(efficacy_clean$dose_interval)=="4 wk"] <- "20_Control"
  levels(efficacy_clean$dose_interval)[levels(efficacy_clean$dose_interval)=="8 wk"] <- "40_Control"
  
  
  efficacy_clean <- efficacy_clean %>% 
    unite(days_dose, days_treatment, dose_interval, sep = "") %>% 
    separate(days_dose, c("days", "dose"), sep = "_") %>% 
    rename("days_treatment" = days,
           "dose_interval" = dose) %>% 
    mutate(days_treatment = as.numeric(days_treatment))
  
  return(efficacy_clean)
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
