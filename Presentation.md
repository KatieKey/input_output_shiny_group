
Input/Output/Shiny Group
========================================================
author: Elle Holbrook, Katie Key, Brian Kim, Lizette Van Zyl
date: 
autosize: true

Outline for Presentation:
========================================================

- what we did
- overview of each data set and how we "tidyed" it
- Shiny app
- challenges
- do differently
- interesting
- conclusion


Our group was tasked with:
========================================================

- inputting the Excel files received from the research group
- outputting the data in "tidy" formats
- developing the Shiny app

Data sets:
========================================================

- Efficacy (Katie)
- Plasma (Brian)
- Tissue Laser (Lizette)
- Tissue Standard PK (Elle)

Efficacy Template
========================================================

```{r, echo=FALSE, fig.width=4, fig.height = 3}
library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(utils)

efficacy <- read_excel("~/Documents/Colorado State/R_Programming_Directory/input_output_shiny_group/data/Gates_18 Efficacy R spreadsheet.xlsx")

head(efficacy, 6)


```

Tidy Efficacy
========================================================

```{r, echo=FALSE}

efficacy_clean <- efficacy %>% 
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

efficacy_clean <- efficacy_clean %>% 
  unite(days_dose, days_treatment, dose_interval, sep = "") %>% 
  separate(days_dose, c("days", "dose"), sep = "_") %>% 
  rename("days_treatment" = days,
         "dose_interval" = dose) %>% 
  mutate(days_treatment = as.numeric(days_treatment))

head(efficacy_clean, 6)


```


Plasma Template
========================================================

```{r}
summary(cars)
```

Tidy Plasma
========================================================

```{r, echo=FALSE}
plot(cars)
```

Tissue Laser Template
========================================================

```{r}
summary(cars)
```

Tidy Tissue Laser
========================================================

```{r, echo=FALSE}
plot(cars)
```

Tissue Standard PK Template
========================================================

```{r}
summary(cars)
```

Tidy Tissue Standard PK
========================================================

```{r, echo=FALSE}
plot(cars)
```

