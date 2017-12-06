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
