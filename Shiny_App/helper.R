#Efficacy_Function cleans "efficacy" data and outputs a cleaned version of that Excel spreadsheet 
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

#clean_las_cap function cleans the Laser Capture Excel spreadsheet
las_file_to_clean <- "data/Gates_18_MALDI_Tissue Laser Capture R_liz_edit.xlsx"

clean_las_cap <- function(file_to_clean) {
  
  las_cap <- read_xlsx(file_to_clean) %>%
    rename(`Parent [ng/ml]` = Parent) %>%
    select(-StudyID, -Metabolite, - Units, - Collection, - `Sample ID`)
  
  n <- nrow(las_cap)
  mice_ids <- rep(c(1:(n/4)), each = 4)
  
  las_cap <- mutate(las_cap, MouseID = mice_ids) %>%
    spread(key = Compartment, value = `Parent [ng/ml]`) %>%
    rename(ULU = `uninvolved lung`, RIM = rim,
           OCS = `outer caseum`, ICS = `inner caseum`) %>%
    mutate(ULU = as.numeric(ULU), RIM = as.numeric(RIM),
           OCS = as.numeric(OCS), ICS = as.numeric(ICS))
  return(las_cap)
}

