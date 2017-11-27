#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(readxl)
library(readr)
library(tidyr)
library(utils)
library(dplyr)
library(shiny)

efficacy_function <- function(efficacy_input){
  efficacy_clean <- efficacy_input %>% 
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
  DT::renderDataTable(efficacy_clean) 
  return(efficacy_clean)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("Mycobacteria Research Laboratories"),
  helpText("Upload Data and Explore Data Tables and Graphs for Each of the Respective
           Data Excel Files (i.e., Efficacy)"),
  
  sidebarLayout(
    sidebarPanel(width = 4, (label = h3("Upload & Select Data")),
                 helpText("Upload Excel Files and Select a Data Frame to Explore Below"),
                 
                 fileInput(label = "Efficacy", inputId = "efficacy",
                           buttonLabel = "Efficacy Data", multiple = TRUE, accept = ".xlsx"),
                 fileInput(label = "Plasma", inputId = "plasma", 
                           buttonLabel = "Plasma Data", multiple = TRUE, accept = ".xlsx"),
                 fileInput(label = "Tissue Laser", inputId = "tissue_laser", 
                           buttonLabel = "Tissue Laser Data", multiple = TRUE, accept = ".xlsx"),
                 fileInput(label = "Tissue Std PK", inputId = "tissue_std_pk", 
                           buttonLabel = "Tissue Std PK Data", multiple = TRUE, accept = ".xlsx"),
                 fileInput(label = "In Vitro", inputId = "in_vitro", 
                           buttonLabel = "In Vitro Data", multiple = TRUE, accept = ".xlsx"),
    
    radioButtons("radio", (label = h3("Pick a Data Set")),
                 helpText("Select a Data Set to View Data Table"),
                 choices = list("Efficacy",
                                "Plasma" = 2, "Tissue Laser" = 3,
                                "Tissue Std PK" = 4, "In Vitro" = 5))
    ),
    
    mainPanel(width = 8,
      tabsetPanel(type = "tabs",
                  tabPanel("Clean Data Set", tableOutput("radio")),
                  tabPanel("Summary", tableOutput("plot")),
                  tabPanel("Independent", verbatimTextOutput("summary")),
                  tabPanel("Independent ~ Dependent", tableOutput("indepdep"))
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) 
  
{
  output$table <- DT::renderDataTable({efficacy_function(input$efficacy) })
}



# Run the application 
shinyApp(ui = ui, server = server)

