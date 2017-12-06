# FUNCTIONS FOR CLEANING RAW DATA FILES

library(dplyr)

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
