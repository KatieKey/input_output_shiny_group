

# FUNCTIONS FOR CLEANING RAW DATA FILES

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
  levels(efficacy_clean$dose_interval)[levels(efficacy_clean$dose_interval)=="M-F"] <- "_QD"
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


#### plasma_function cleans raw plasma data in Shiny app
#Function Title: Cleaning Plasma Dataframe

#This function has a dataframe as an argument. The dataframe contains data on plasma
#concentrations. The function cleans the plasma dataframe by selecting only the needed
#variables, renaming variables, and changing the group column to a character.

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
           plasma_concentration = Plasma_Parent)  %>%
    mutate(Group = as.character(Group))
  return(plasma_clean)
}


##### Clean the tissue laser data into a tidy format

# 
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



##### tissue_std_pk_function cleans raw tissue std pk data in Shiny app

#Function Title: Clean STD PK Dataframe
#The argument for this function contains information on pharmacokinetic properties of the 
#drugs tested on a mouse-by-mouse level. A mouse id was created as a new column to the
#dataset. Additionally, only the necessary columns were included in the dataframe. The spread
#function was used to convert the Comparment column into columns for each compartment, 
#containing the respective Parent values. These new columns were then renamed to match the 
#SLE and SLU variable names in the tidy data templates and recoded as numerical values.

tissue_std_pk_function <- function(tissue_std_pk_df){
  n <- nrow(tissue_std_pk_df)
  mice_ids <- rep(c(1:(n/2)), each = 2)
  
  tissue_std_pk_clean <- tissue_std_pk_df %>% 
    mutate(mouse_number = mice_ids) %>%
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
