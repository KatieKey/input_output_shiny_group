# FUNCTIONS FOR CLEANING RAW DATA FILES

library(dplyr)

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
    select(Compound, mouse_number, Group, Protocol_Animal, Dosing, Timepoint, Compartment, Parent) %>%
    rename(drug = Compound,
           `Parent [ng/ml]` = Parent) %>% 
    spread(key = Compartment, value = `Parent [ng/ml]`) %>% 
    rename(SLU = Lung, 
           SLE = Lesion) %>% 
    mutate_at(c("SLU", "SLE"), as.numeric)
  return(tissue_std_pk_clean)
} 
