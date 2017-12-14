#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# Find out more about building applications with Shiny here:
#    http://shiny.rstudio.com/

library(readxl) 
library(readr)
library(tidyr)
library(utils) 
library(dplyr)
library(shiny)
library(DT) 
library(visdat)
library(ggplot2)
library(ggthemes) 
library(rpart)
library(ggbeeswarm)
library(plotly)
library(colourpicker)
library(rpart.plot)
library(party)
library(randomForest)
library(tibble)
library(glmnet)
library(knitr)
library(broom)
library(ggfortify)
library(stats)
library(kableExtra)
library(tidyverse)
library(gghighlight)
library(dendextend)
library(ggdendro)
library(purrr)
library(ggmap)
library(ggpolypath)
library(viridis)

source("helper_revised.R")
source("Group2Functions.R")
source("Group3Functions.R")

# Define UI for application 
ui <- fluidPage(
  
  titlePanel("Mycobacteria Research Laboratories"),
  helpText(h4("Upload Data and Explore Data Tables and Various Plots Using the Tabs Below")),
  helpText("Note: The Plots May Take a Moment to Load"),
  
  sidebarLayout(
    sidebarPanel(width = 3, (label = h3("Upload Data")),
                 
                 fileInput(label = "Efficacy", inputId = "efficacy",
                           buttonLabel = "Efficacy Data", multiple = TRUE, accept = ".xlsx"),
                 fileInput(label = "Plasma", inputId = "plasma", 
                           buttonLabel = "Plasma Data", multiple = TRUE, accept = ".xlsx"),
                 fileInput(label = "Tissue Laser", inputId = "tissue_laser", 
                           buttonLabel = "Tissue Laser Data", multiple = TRUE, accept = ".xlsx"),
                 fileInput(label = "Tissue Std PK", inputId = "tissue_std_pk", 
                           buttonLabel = "Tissue Std PK Data", multiple = TRUE, accept = ".xlsx"),
                 fileInput(label = "In Vitro", inputId = "invitro", 
                           buttonLabel = "In Vitro Data", multiple = TRUE, accept = ".xlsx")
                  ),
    
    mainPanel(width = 8,
      tabsetPanel(type = "tabs",
                  tabPanel("Raw Data Sets", 
                           tabsetPanel(type = "tabs",
                             tabPanel("Efficacy",
                                DT::dataTableOutput("raw_efficacy_table")
                                ),
                             tabPanel("Plasma",
                                DT::dataTableOutput("raw_plasma_table")
                                ),
                             tabPanel("Tissue Laser",
                                DT::dataTableOutput("raw_tissue_laser_table")
                                ),
                             tabPanel("Tissue Std PK",
                                DT::dataTableOutput("raw_tissue_std_pk_table")
                                ),
                             tabPanel("In Vitro",
                                      DT::dataTableOutput("raw_invitro_table")
                                )
                                )
                                ),
                           
                  tabPanel("Clean Data Set",
                           tabsetPanel(type = "tabs",
                                       tabPanel("Efficacy",
                                                DT::dataTableOutput("clean_efficacy_table")
                                                ),
                                       tabPanel("Plasma",
                                                DT::dataTableOutput("clean_plasma_table")
                                                ),
                                       tabPanel("Tissue Laser",
                                                DT::dataTableOutput("clean_tissue_laser_table")
                                                ),
                                       tabPanel("Tissue Std PK",
                                                DT::dataTableOutput("clean_tissue_std_pk_table")
                                                ),
                                       tabPanel("In Vitro",
                                                DT::dataTableOutput("clean_invitro_table")
                                                ),
                                       tabPanel("Efficacy Summary",
                                                DT::dataTableOutput("clean_efficacy_summary_table")
                                               )
                                           )
                                           ),
                           
                  tabPanel("Summary of Clean  Data", 
                           tabsetPanel(type = "tabs",
                                       tabPanel("Efficacy",
                                                plotOutput("summary_efficacy_plot")
                                                ),
                                       tabPanel("Plasma",
                                                plotOutput("summary_plasma_plot")
                                                ),
                                       tabPanel("Tissue Laser",
                                                plotOutput("summary_tissue_laser_plot")
                                                ),
                                       tabPanel("Tissue Std PK",
                                                plotOutput("summary_tissue_std_pk_plot")
                                                ),
                                       tabPanel("In Vitro",
                                                plotOutput("summary_invitro_plot")
                                                ),
                                       tabPanel("Efficacy Summary",
                                                plotOutput("summary_efficacy_summary_plot")
                                        )
                                        )
                                        ),
                  
                  tabPanel("Independent", 
                           tabsetPanel(type = "tabs",
                                       tabPanel("In Vitro", width = 2,
                                                helpText("Select and Deselect to Explore Efficacy Summary Data"),
                                                div(style="display: inline-block;vertical-align:top; width: 150px;",
                                 checkboxGroupInput("CheckBeeVarInVitro",
                                label = h4("Check Variables To Explore"), 
                                choices = list("Caseum_binding" = Caseum_binding, 
                                               "cLogP" = cLogP,
                                               "huPPB" = huPPB,
                                               "muPPB" = muPPB,
                                               "MIC_Erdman" = MIC_Erdman,
                                               "MICserumErd" = MICserumErd,
                                               "MIC_Rv" = MIC_Rv,
                                               "MacUptake" = MacUptake),
                                selected = c("Caseum_binding" = Caseum_binding, 
                                              "cLogP" = cLogP,
                                              "huPPB" = huPPB,
                                              "muPPB" = muPPB,
                                              "MIC_Erdman" = MIC_Erdman,
                                              "MICserumErd" = MICserumErd,
                                              "MIC_Rv" = MIC_Rv,
                                              "MacUptake" = MacUptake)
                                                    )
                                  ),
                                div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
                                div(style="display: inline-block;vertical-align:top; width: 150px;",
                                checkboxGroupInput("CheckBeeDrugInVitro", 
                                 label = h4("Check Drugs To Explore"), 
                                 choices = list("DRUG1" = DRUG1, "DRUG2" = DRUG2, 
                                                "DRUG3" = DRUG3,
                                 "DRUG4" = DRUG4, "DRUG5" = DRUG5, "DRUG6" = DRUG6,
                                 "DRUG7" = DRUG7, "DRUG8" = DRUG8, "DRUG9" = DRUG9,
                                 "DRUG10" = DRUG10, "DRUG11" = DRUG11),
                                 selected = c("DRUG1" = DRUG1, "DRUG2" = DRUG2, 
                                             "DRUG3" = DRUG3,
                                             "DRUG4" = DRUG4, "DRUG5" = DRUG5, "DRUG6" = DRUG6,
                                             "DRUG7" = DRUG7, "DRUG8" = DRUG8, "DRUG9" = DRUG9,
                                             "DRUG10" = DRUG10, "DRUG11" = DRUG11)
                                )
                                 ),
                                 plotlyOutput("beeswarm_invitro_plot", width = "auto", height = "auto")
                                 ),
                                       tabPanel("In Vivo", width = 3,
                                                helpText("Select and Deselect to Explore Efficacy Summary Data"),
                                                div(style="display: inline-block;vertical-align:top; width: 150px;",
                                                checkboxGroupInput("CheckBeeVarInVivo",
                                                                   label = h4("Check Variables To Explore"), 
                                                                   choices = list("RIM" = RIM, 
                                                                                  "OCS" = OCS,
                                                                                  "ICS" = ICS,
                                                                                  "ULU" = ULU,
                                                                                  "SLU" = SLU,
                                                                                  "SLE" = SLE,
                                                                                  "PLA" = PLA),
                                                                   selected = c("RIM" = RIM,
                                                                                "OCS" = OCS, 
                                                                                 "ICS" = ICS,
                                                                                 "ULU" = ULU,
                                                                                 "SLU" = SLU,
                                                                                 "SLE" = SLE,
                                                                                 "PLA" = PLA)
                                                                                 )
                                                                                ),
                                                div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
                                                div(style="display: inline-block;vertical-align:top; width: 150px;",
                                                checkboxGroupInput("CheckBeeDrugInVivo", 
                                                                   label = h4("Check Drugs To Explore"), 
                                                                   choices = list("DRUG1" = DRUG1, "DRUG2" = DRUG2, 
                                                                                  "DRUG3" = DRUG3,
                                                                                  "DRUG4" = DRUG4, "DRUG5" = DRUG5, "DRUG6" = DRUG6,
                                                                                  "DRUG7" = DRUG7, "DRUG8" = DRUG8, "DRUG9" = DRUG9,
                                                                                  "DRUG10" = DRUG10, "DRUG11" = DRUG11),
                                                                   selected = c("DRUG1" = DRUG1, "DRUG2" = DRUG2, 
                                                                                "DRUG3" = DRUG3,
                                                                                "DRUG4" = DRUG4, "DRUG5" = DRUG5, "DRUG6" = DRUG6,
                                                                                "DRUG7" = DRUG7, "DRUG8" = DRUG8, "DRUG9" = DRUG9,
                                                                                "DRUG10" = DRUG10, "DRUG11" = DRUG11)
                                                )
                                                ),
                                                plotlyOutput("beeswarm_invivo_plot", width = "auto", height = "auto")
                                                ),
                                       tabPanel("Dendrogram",
                                                helpText("Help Text: human_plasma = human binding plasma &
                                                         mouse_plasma = mouse binding plasma"),
                                                radioButtons("dendrogram_radio", label = h4("View Data By:"),
                                                             choices = list("Test" = by_test,
                                                                            "Drug" = by_drug)),
                                                plotOutput("dendrogram", width = "140%")
                                                ),
                                       tabPanel("Mouse Model",
                                                radioButtons("mouse_level", label = h4("Pick a Level"),
                                                             choices = list("Cmax" = Cmax,
                                                                            "Trough" = Trough)),
                                                plotOutput("mouse_model")
                                                ),
                                      tabPanel("Lesion Model",
                                               radioButtons("lesion_level", label = h4("Pick a Level"),
                                                            choices = list("Cmax" = Cmax,
                                                                           "Trough" = Trough)),
                                               plotOutput("lesion_model")
                                               )
                                               )
                                               ),
                  
                  tabPanel("Independent ~ Dependent", 
                           tabsetPanel(type = "tabs",
                                       tabPanel("Regression Trees",
                                                div(style="display: inline-block;vertical-align:top; width: 180px;",
                                                radioButtons("regression", label = h4("Pick a Variable"),
                                                             choices = list("Lung Efficacy" = ELU,
                                                                         "Spleen Efficacy" = ESP)
                                                             )
                                                             ),
                                                div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                                                div(style="display: inline-block;vertical-align:top; width: 180px;",
                                                numericInput("min_split", label = h4("Minimum Split for Regression Tree"), 
                                                             value = 1, min = 0)
                                                             ),
                                                div(style="display: inline-block;vertical-align:top; width: 80px;",HTML("<br>")),
                                                div(style="display: inline-block;vertical-align:top; width: 180px;",
                                                numericInput("min_bucket", label = h4("Minimum Buckets for Regression Tree"), 
                                                             value = 1, min = 0)
                                                             ),
                                                plotOutput("regression_tree")
                                                            ),
                                       tabPanel("Best Variables",
                                                radioButtons("variable", label = h4("Pick a Variable"),
                                                             choices = list("Lung Efficacy" = ELU,
                                                                            "Spleen Efficacy" = ESP)),
                                                plotlyOutput("best_variables")
                                                            ),
                                       tabPanel("Scatter Plot",
                                                div(style="display: inline-block;vertical-align:top; width: 150px;",
                                                radioButtons("scatter_variable", label = h4("Pick a Variable"),
                                                             choices = list("Lung Efficacy" = ELU,
                                                                            "Spleen Efficacy" = ESP))
                                                           ),
                                                div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
                                                div(style="display: inline-block;vertical-align:top; width: 150px;",
                                                radioButtons("scatter_level", label = h4("Pick a Level"),
                                                             choices = list("Cmax" = Cmax,
                                                                            "Trough" = Trough)
                                                             )
                                                             ),
                                                plotOutput("scatter_plot")
                                                             ),
                                       tabPanel("Linear Model",
                                                div(style="display: inline-block;vertical-align:top; width: 150px;",
                                                radioButtons("linear_variable", label = h4("Pick a Variable"),
                                                             choices = list("Lung Efficacy" = ELU,
                                                                            "Spleen Efficacy" = ESP)
                                                             )
                                                             ),
                                                div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
                                                div(style="display: inline-block;vertical-align:top; width: 150px;",
                                                radioButtons("linear_level", label = h4("Pick a Level"),
                                                             choices = list("Cmax" = Cmax,
                                                                            "Trough" = Trough)
                                                             )
                                                             ),
                                                plotOutput("linear_model")
                                                            ),
                                       tabPanel("LASSO Model",
                                                div(style="display: inline-block;vertical-align:top; width: 150px;",
                                                radioButtons("variable_lasso", label = h4("Pick a Variable"),
                                                             choices = list("Lung Efficacy" = ELU,
                                                                            "Spleen Efficacy" = ESP)
                                                             )
                                                             ),
                                                div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
                                                div(style="display: inline-block;vertical-align:top; width: 150px;",
                                                radioButtons("lasso_dosage", label = h4("Pick a Dosage"), 
                                                             choices = list("50" = fifty,
                                                                            "100" = hundred)
                                                             )
                                                             ),
                                                DT::dataTableOutput("lasso_model")))
                           )
                  )
                  )
                  )
      )



#Define server logic 
server <- function(input, output) {
  
###### CODE FOR RENDERING RAW DATA
  
# Render data table with raw efficacy data
  output$raw_efficacy_table <- DT::renderDataTable({
    efficacy_file <- input$efficacy
    
    # Make sure you don't show an error by trying to run code before a file's been uploaded
    if(is.null(efficacy_file)){
      return(NULL)
    }
    
    ext <- tools::file_ext(efficacy_file$name)
    file.rename(efficacy_file$datapath, 
                paste(efficacy_file$datapath, ext, sep = "."))
    read_excel(paste(efficacy_file$datapath, ext, sep = "."), sheet = 1)
  })
  
# Render data table with raw plasma data
    output$raw_plasma_table <- DT::renderDataTable({
      plasma_file <- input$plasma
      
      # Make sure you don't show an error by trying to run code before a file's been uploaded
      if(is.null(plasma_file)){
        return(NULL)
      }
      
      ext <- tools::file_ext(plasma_file$name)
      file.rename(plasma_file$datapath, 
                  paste(plasma_file$datapath, ext, sep = "."))
      read_excel(paste(plasma_file$datapath, ext, sep = "."), sheet = 1)
      
    })
    
# Render data table for raw tissue laser data
    output$raw_tissue_laser_table <- DT::renderDataTable({
      tissue_laser_file <- input$tissue_laser
      
      # Make sure you don't show an error by trying to run code before a file's been uploaded
      if(is.null(tissue_laser_file)){
        return(NULL)
      }
      
      ext <- tools::file_ext(tissue_laser_file$name)
      file.rename(tissue_laser_file$datapath, 
                  paste(tissue_laser_file$datapath, ext, sep = "."))
      read_excel(paste(tissue_laser_file$datapath, ext, sep = "."), sheet = 1)
      
    })
    
# Render data table for raw tissue std pk data
    output$raw_tissue_std_pk_table <- DT::renderDataTable({
      tissue_std_pk_file <- input$tissue_std_pk
      
      # Make sure you don't show an error by trying to run code before a file's been uploaded
      if(is.null(tissue_std_pk_file)){
        return(NULL)
      }
      
      ext <- tools::file_ext(tissue_std_pk_file$name)
      file.rename(tissue_std_pk_file$datapath, 
                  paste(tissue_std_pk_file$datapath, ext, sep = "."))
      read_excel(paste(tissue_std_pk_file$datapath, ext, sep = "."), sheet = 1)
      
    })
    
# Render data table for raw in vitro data
    output$raw_invitro_table <- DT::renderDataTable({
      invitro_file <- input$invitro
      
      # Make sure you don't show an error by trying to run code before a file's been uploaded
      if(is.null(invitro_file)){
        return(NULL)
      }
      
      ext <- tools::file_ext(invitro_file$name)
      file.rename(invitro_file$datapath, 
                  paste(invitro_file$datapath, ext, sep = "."))
      read_excel(paste(invitro_file$datapath, ext, sep = "."), sheet = 1)
      
    })
    

######## CODE FOR RENDERING CLEAN DATA
  
# Render data table with clean efficacy data
    output$clean_efficacy_table <- DT::renderDataTable({
    efficacy_file <- input$efficacy
    
    # Make sure you don't show an error by trying to run code before a file's been uploaded
    if(is.null(efficacy_file)){
      return(NULL)
    }
    
    ext <- tools::file_ext(efficacy_file$name)
    file.rename(efficacy_file$datapath, 
                paste(efficacy_file$datapath, ext, sep = "."))
    efficacy_df <- read_excel(paste(efficacy_file$datapath, ext, sep = "."), sheet = 1)
    efficacy_function(efficacy_df)
    })
  
# Render data table with clean plasma data
    output$clean_plasma_table <- DT::renderDataTable({
    plasma_file <- input$plasma
    
    # Make sure you don't show an error by trying to run code before a file's been uploaded
    if(is.null(plasma_file)){
      return(NULL)
    }
    
    ext <- tools::file_ext(plasma_file$name)
    file.rename(plasma_file$datapath, 
                paste(plasma_file$datapath, ext, sep = "."))
    plasma_df <- read_excel(paste(plasma_file$datapath, ext, sep = "."), sheet = 1)
    plasma_function(plasma_df)
    }) 

# Render data table with clean tissue laser data
    output$clean_tissue_laser_table <- DT::renderDataTable({
    tissue_laser_file <- input$tissue_laser
    
    # Make sure you don't show an error by trying to run code before a file's been uploaded
    if(is.null(tissue_laser_file)){
      return(NULL)
    }
    
    ext <- tools::file_ext(tissue_laser_file$name)
    file.rename(tissue_laser_file$datapath, 
                paste(tissue_laser_file$datapath, ext, sep = "."))
    tissue_laser_df <- read_excel(paste(tissue_laser_file$datapath, ext, sep = "."), sheet = 1)
    tissue_laser_function(tissue_laser_df)
    })
  
# Render data table with clean tissue std pk data
    output$clean_tissue_std_pk_table <- DT::renderDataTable({
    tissue_std_pk_file <- input$tissue_std_pk
    
    # Make sure you don't show an error by trying to run code before a file's been uploaded
    if(is.null(tissue_std_pk_file)){
      return(NULL)
    }
    
    ext <- tools::file_ext(tissue_std_pk_file$name)
    file.rename(tissue_std_pk_file$datapath, 
                paste(tissue_std_pk_file$datapath, ext, sep = "."))
    tissue_std_pk_df <- read_excel(paste(tissue_std_pk_file$datapath, ext, sep = "."), sheet = 1)
    tissue_std_pk_function(tissue_std_pk_df)
    })
    
# Render data table with clean in vitro data
    output$clean_invitro_table <- DT::renderDataTable({
      invitro_file <- input$invitro
      
      # Make sure you don't show an error by trying to run code before a file's been uploaded
      if(is.null(invitro_file)){
        return(NULL)
      }
      
      ext <- tools::file_ext(invitro_file$name)
      file.rename(invitro_file$datapath, 
                  paste(invitro_file$datapath, ext, sep = "."))
      invitro_df <- read_excel(paste(invitro_file$datapath, ext, sep = "."), sheet = 1)
      in_vitro_function(invitro_df)
    })
    
# Render data table with cleaned efficacy summary data
    output$clean_efficacy_summary_table <- DT::renderDataTable({ 
      # Grab the files you need
      efficacy_file <- input$efficacy
      plasma_file <- input$plasma
      tissue_laser_file <- input$tissue_laser
      tissue_std_pk_file <- input$tissue_std_pk
      invitro_file <- input$invitro
      
      # Make sure you don't show an error by trying to run code before a file's been uploaded
      if(is.null(efficacy_file) | is.null(plasma_file) | is.null(tissue_laser_file) | 
         is.null(tissue_std_pk_file) | is.null(invitro_file)){
        return(NULL)
      }
      
      ## Clean efficacy file
      ext <- tools::file_ext(efficacy_file$name)
      file.rename(efficacy_file$datapath,
                  paste(efficacy_file$datapath, ext, sep = "."))
      efficacy_df <- read_excel(paste(efficacy_file$datapath, ext, sep = "."), sheet = 1)
      efficacy_clean <- efficacy_function(efficacy_df)
      efficacy_clean_summarized <- efficacy_summary_function(efficacy_clean)
      
      ## Clean plasma file
      ext <- tools::file_ext(plasma_file$name)
      file.rename(plasma_file$datapath,
                  paste(plasma_file$datapath, ext, sep = "."))
      plasma_df <- read_excel(paste(plasma_file$datapath, ext, sep = "."), sheet = 1)
      plasma_clean <- plasma_function(plasma_df)
      plasma_summarized <- plasma_summarize(plasma_clean)

      ## Clean laser file
      ext <- tools::file_ext(tissue_laser_file$name)
      file.rename(tissue_laser_file$datapath,
                  paste(tissue_laser_file$datapath, ext, sep = "."))
      tissue_laser_df <- read_excel(paste(tissue_laser_file$datapath, ext, sep = "."), sheet = 1)
      tissue_laser_clean <- tissue_laser_function(tissue_laser_df)
      tissue_laser_summarized <- tissue_laser_summary(tissue_laser_clean)

      ## Clean standard tissue file
      ext <- tools::file_ext(tissue_std_pk_file$name)
      file.rename(tissue_std_pk_file$datapath,
                  paste(tissue_std_pk_file$datapath, ext, sep = "."))
      tissue_std_pk_df <- read_excel(paste(tissue_std_pk_file$datapath, ext, sep = "."), sheet = 1)
      tissue_std_pk_clean <- tissue_std_pk_function(tissue_std_pk_df)
      tissue_std_pk_summarized <- tissue_std_pk_summarize(tissue_std_pk_clean)

      ## Clean in vitro file
      ext <- tools::file_ext(invitro_file$name)
      file.rename(invitro_file$datapath,
                  paste(invitro_file$datapath, ext, sep = "."))
      invitro_df <- read_excel(paste(invitro_file$datapath, ext, sep = "."), sheet = 1)
      in_vitro_clean <- in_vitro_function(invitro_df)

      # Combine everything into the efficacy summary file
      efficacy_summary_file <- create_summary_df(efficacy_clean_summarized,
                                                 plasma_summarized,
                                                 tissue_laser_summarized,
                                                 tissue_std_pk_summarized,
                                                 in_vitro_clean)
      efficacy_summary_file
      
    })
    
######## CODE FOR RENDERING VIS DATA PLOTS OF RAW DATA
  
# Render plot with summary of clean efficacy data
      output$summary_efficacy_plot <- renderPlot({
      efficacy_file <- input$efficacy
      
      # Make sure you don't show an error by trying to run code before a file's been uploaded
      if(is.null(efficacy_file)){
        return(NULL)
      }
      
      ext <- tools::file_ext(efficacy_file$name)
      file.rename(efficacy_file$datapath, 
                  paste(efficacy_file$datapath, ext, sep = "."))
      efficacy_df <- read_excel(paste(efficacy_file$datapath, ext, sep = "."), sheet = 1)
      efficacy_clean <- efficacy_function(efficacy_df)
      vis_dat(efficacy_clean)
     }) 
  
# Render plot with summary of clean plasma data  
    output$summary_plasma_plot <- renderPlot({
    plasma_file <- input$plasma
    
    # Make sure you don't show an error by trying to run code before a file's been uploaded
    if(is.null(plasma_file)){
      return(NULL)
    }
    
    ext <- tools::file_ext(plasma_file$name)
    file.rename(plasma_file$datapath, 
                paste(plasma_file$datapath, ext, sep = "."))
    plasma_df <- read_excel(paste(plasma_file$datapath, ext, sep = "."), sheet = 1)
    plasma_clean <- plasma_function(plasma_df)
    vis_dat(plasma_clean)
    }) 

  
# Render plot with summary of clean tissue laser data
    output$summary_tissue_laser_plot <- renderPlot({
    tissue_laser_file <- input$tissue_laser
    
    # Make sure you don't show an error by trying to run code before a file's been uploaded
    if(is.null(tissue_laser_file)){
      return(NULL)
    }
    
    ext <- tools::file_ext(tissue_laser_file$name)
    file.rename(tissue_laser_file$datapath, 
                paste(tissue_laser_file$datapath, ext, sep = "."))
    tissue_laser_df <- read_excel(paste(tissue_laser_file$datapath, ext, sep = "."), sheet = 1)
    tissue_laser_clean <- tissue_laser_function(tissue_laser_df)
    vis_dat(tissue_laser_clean)
    }) 
  
# Render plot with summary of clean tissue std pk data
    output$summary_tissue_std_pk_plot <- renderPlot({
    tissue_std_pk_file <- input$tissue_std_pk
    
    # Make sure you don't show an error by trying to run code before a file's been uploaded
    if(is.null(tissue_std_pk_file)){
      return(NULL)
    }
    
    ext <- tools::file_ext(tissue_std_pk_file$name)
    file.rename(tissue_std_pk_file$datapath, 
                paste(tissue_std_pk_file$datapath, ext, sep = "."))
    tissue_std_pk_df <- read_excel(paste(tissue_std_pk_file$datapath, ext, sep = "."), sheet = 1)
    tissue_std_pk_clean <- tissue_std_pk_function(tissue_std_pk_df)
    vis_dat(tissue_std_pk_clean)
    }) 
    
# Render plot with summary of clean in vitro data
    output$summary_invitro_plot <- renderPlot({
      invitro_file <- input$invitro
      
      # Make sure you don't show an error by trying to run code before a file's been uploaded
      if(is.null(invitro_file)){
        return(NULL)
      }
      
      ext <- tools::file_ext(invitro_file$name)
      file.rename(invitro_file$datapath, 
                  paste(invitro_file$datapath, ext, sep = "."))
      invitro_df <- read_excel(paste(invitro_file$datapath, ext, sep = "."), sheet = 1)
      invitro_clean <- in_vitro_function(invitro_df)
      vis_dat(invitro_clean)
    }) 
    
# Render plot with summary of efficacy summary data
    output$summary_efficacy_summary_plot <- renderPlot({
      
      efficacy_file <- input$efficacy
      plasma_file <- input$plasma
      tissue_laser_file <- input$tissue_laser
      tissue_std_pk_file <- input$tissue_std_pk
      invitro_file <- input$invitro
      
      # Make sure you don't show an error by trying to run code before a file's been uploaded
      if(is.null(efficacy_file) | is.null(plasma_file) | is.null(tissue_laser_file) | 
         is.null(tissue_std_pk_file) | is.null(invitro_file)){
        return(NULL)
      }
      
      ## Clean efficacy file
      ext <- tools::file_ext(efficacy_file$name)
      file.rename(efficacy_file$datapath,
                  paste(efficacy_file$datapath, ext, sep = "."))
      efficacy_df <- read_excel(paste(efficacy_file$datapath, ext, sep = "."), sheet = 1)
      efficacy_clean <- efficacy_function(efficacy_df)
      efficacy_clean_summarized <- efficacy_summary_function(efficacy_clean)
      
      ## Clean plasma file
      ext <- tools::file_ext(plasma_file$name)
      file.rename(plasma_file$datapath,
                  paste(plasma_file$datapath, ext, sep = "."))
      plasma_df <- read_excel(paste(plasma_file$datapath, ext, sep = "."), sheet = 1)
      plasma_clean <- plasma_function(plasma_df)
      plasma_summarized <- plasma_summarize(plasma_clean)
      
      ## Clean laser file
      ext <- tools::file_ext(tissue_laser_file$name)
      file.rename(tissue_laser_file$datapath,
                  paste(tissue_laser_file$datapath, ext, sep = "."))
      tissue_laser_df <- read_excel(paste(tissue_laser_file$datapath, ext, sep = "."), sheet = 1)
      tissue_laser_clean <- tissue_laser_function(tissue_laser_df)
      tissue_laser_summarized <- tissue_laser_summary(tissue_laser_clean)
      
      ## Clean standard tissue file
      ext <- tools::file_ext(tissue_std_pk_file$name)
      file.rename(tissue_std_pk_file$datapath,
                  paste(tissue_std_pk_file$datapath, ext, sep = "."))
      tissue_std_pk_df <- read_excel(paste(tissue_std_pk_file$datapath, ext, sep = "."), sheet = 1)
      tissue_std_pk_clean <- tissue_std_pk_function(tissue_std_pk_df)
      tissue_std_pk_summarized <- tissue_std_pk_summarize(tissue_std_pk_clean)
      
      ## Clean in vitro file
      ext <- tools::file_ext(invitro_file$name)
      file.rename(invitro_file$datapath,
                  paste(invitro_file$datapath, ext, sep = "."))
      invitro_df <- read_excel(paste(invitro_file$datapath, ext, sep = "."), sheet = 1)
      in_vitro_clean <- in_vitro_function(invitro_df)
      
      # Combine everything into the efficacy summary file
      efficacy_summary_file <- create_summary_df(efficacy_clean_summarized,
                                                 plasma_summarized,
                                                 tissue_laser_summarized,
                                                 tissue_std_pk_summarized,
                                                 in_vitro_clean)
      efficacy_summary_file
      
      # Make sure you don't show an error by trying to run code before a file's been uploaded
      if(is.null(efficacy_summary_file)){
        return(NULL)
      }
      
      vis_dat(efficacy_summary_file)
    }) 
  
    
#####INDEPENDENT GROUPS FUNCTIONS
    #Beeswarm In Vitro
    
    output$beeswarm_invitro_plot <- renderPlotly({
      
      efficacy_file <- input$efficacy
      plasma_file <- input$plasma
      tissue_laser_file <- input$tissue_laser
      tissue_std_pk_file <- input$tissue_std_pk
      invitro_file <- input$invitro
      
      # Make sure you don't show an error by trying to run code before a file's been uploaded
      if(is.null(efficacy_file) | is.null(plasma_file) | is.null(tissue_laser_file) | 
         is.null(tissue_std_pk_file) | is.null(invitro_file)){
        return(NULL)
      }
      
      ## Clean efficacy file
      ext <- tools::file_ext(efficacy_file$name)
      file.rename(efficacy_file$datapath,
                  paste(efficacy_file$datapath, ext, sep = "."))
      efficacy_df <- read_excel(paste(efficacy_file$datapath, ext, sep = "."), sheet = 1)
      efficacy_clean <- efficacy_function(efficacy_df)
      efficacy_clean_summarized <- efficacy_summary_function(efficacy_clean)
      
      ## Clean plasma file
      ext <- tools::file_ext(plasma_file$name)
      file.rename(plasma_file$datapath,
                  paste(plasma_file$datapath, ext, sep = "."))
      plasma_df <- read_excel(paste(plasma_file$datapath, ext, sep = "."), sheet = 1)
      plasma_clean <- plasma_function(plasma_df)
      plasma_summarized <- plasma_summarize(plasma_clean)
      
      ## Clean laser file
      ext <- tools::file_ext(tissue_laser_file$name)
      file.rename(tissue_laser_file$datapath,
                  paste(tissue_laser_file$datapath, ext, sep = "."))
      tissue_laser_df <- read_excel(paste(tissue_laser_file$datapath, ext, sep = "."), sheet = 1)
      tissue_laser_clean <- tissue_laser_function(tissue_laser_df)
      tissue_laser_summarized <- tissue_laser_summary(tissue_laser_clean)
      
      ## Clean standard tissue file
      ext <- tools::file_ext(tissue_std_pk_file$name)
      file.rename(tissue_std_pk_file$datapath,
                  paste(tissue_std_pk_file$datapath, ext, sep = "."))
      tissue_std_pk_df <- read_excel(paste(tissue_std_pk_file$datapath, ext, sep = "."), sheet = 1)
      tissue_std_pk_clean <- tissue_std_pk_function(tissue_std_pk_df)
      tissue_std_pk_summarized <- tissue_std_pk_summarize(tissue_std_pk_clean)
      
      ## Clean in vitro file
      ext <- tools::file_ext(invitro_file$name)
      file.rename(invitro_file$datapath,
                  paste(invitro_file$datapath, ext, sep = "."))
      invitro_df <- read_excel(paste(invitro_file$datapath, ext, sep = "."), sheet = 1)
      in_vitro_clean <- in_vitro_function(invitro_df)
      
      # Combine everything into the efficacy summary file
      efficacy_summary_file <- create_summary_df(efficacy_clean_summarized,
                                                 plasma_summarized,
                                                 tissue_laser_summarized,
                                                 tissue_std_pk_summarized,
                                                 in_vitro_clean)
      efficacy_summary_file

        in_vitro <- efficacy_summary_file %>%
          rename(Drugs = "drug") %>% 
          unite(dosage_interval, dosage:dose_int, sep = "")
        
        in_vitro_SM <- in_vitro %>% 
          gather(key = variable, value = value, -Drugs, -dosage_interval) %>% 
          mutate(variable_filtered = variable) %>% 
          mutate(variable = factor(variable, levels = c("Caseum_binding", "cLogP", "huPPB", "muPPB", "MIC_Erdman",
                                                        "MICserumErd", "MIC_Rv", "MacUptake"),
                                   labels = c("Caseum \nBinding", "cLogP", 
                                              "Human \nPlasma \nBinding", "Mouse \nPlasma \nBinding", 
                                              "MIC Erdman \nStrain", "MIC Erdman \nStrain \nwith Serum", "MIC Rv Strain",
                                              "Macrophage \nUptake (Ratio)"))) %>% 
          mutate(dosage_interval = factor(dosage_interval, levels = c("50BID", "100QD"))) %>% 
          filter(Drugs %in% c(input$CheckBeeDrugInVitro))
        
        if(is.null(input$CheckBeeVarInVitro)) {
          return(NULL)
        }
      
        
        if(!is.null(input$CheckBeeVarInVitro)) {
          in_vitro_SM <- in_vitro_SM %>% 
            dplyr::filter(variable_filtered %in% input$CheckBeeVarInVitro)
        }
        
        if(!is.null(input$CheckBeeDrugInVitro)) {
          in_vitro_SM <- in_vitro_SM %>%
            dplyr::filter(Drugs %in% input$CheckBeeDrugInVitro)
        }

        in_vitro_SMplot <- in_vitro_SM %>% 
          ggplot(aes(x = dosage_interval, y = as.numeric(as.character(value)), color = Drugs)) +
          geom_beeswarm(alpha = 0.5, size = 1.5, groupOnX = FALSE) +
          labs(x = 'Dosage-Interval', y = 'Value') +
          ggtitle('In-Vitro Distribution of TB Drugs') +
          theme_few() +
          facet_wrap(~ input$CheckBeeVarInVitro, ncol = 2, scale="free")
      
        in_vitro_plotly <- ggplotly(in_vitro_SMplot)
        
        return(in_vitro_plotly)
        
})
    
### beeswarm IN VIVO plot
    
    output$beeswarm_invivo_plot <- renderPlotly({
      
      efficacy_file <- input$efficacy
      plasma_file <- input$plasma
      tissue_laser_file <- input$tissue_laser
      tissue_std_pk_file <- input$tissue_std_pk
      invitro_file <- input$invitro
      
      # Make sure you don't show an error by trying to run code before a file's been uploaded
      if(is.null(efficacy_file) | is.null(plasma_file) | is.null(tissue_laser_file) | 
         is.null(tissue_std_pk_file) | is.null(invitro_file)){
        return(NULL)
      }
      
      ## Clean efficacy file
      ext <- tools::file_ext(efficacy_file$name)
      file.rename(efficacy_file$datapath,
                  paste(efficacy_file$datapath, ext, sep = "."))
      efficacy_df <- read_excel(paste(efficacy_file$datapath, ext, sep = "."), sheet = 1)
      efficacy_clean <- efficacy_function(efficacy_df)
      efficacy_clean_summarized <- efficacy_summary_function(efficacy_clean)
      
      ## Clean plasma file
      ext <- tools::file_ext(plasma_file$name)
      file.rename(plasma_file$datapath,
                  paste(plasma_file$datapath, ext, sep = "."))
      plasma_df <- read_excel(paste(plasma_file$datapath, ext, sep = "."), sheet = 1)
      plasma_clean <- plasma_function(plasma_df)
      plasma_summarized <- plasma_summarize(plasma_clean)
      
      ## Clean laser file
      ext <- tools::file_ext(tissue_laser_file$name)
      file.rename(tissue_laser_file$datapath,
                  paste(tissue_laser_file$datapath, ext, sep = "."))
      tissue_laser_df <- read_excel(paste(tissue_laser_file$datapath, ext, sep = "."), sheet = 1)
      tissue_laser_clean <- tissue_laser_function(tissue_laser_df)
      tissue_laser_summarized <- tissue_laser_summary(tissue_laser_clean)
      
      ## Clean standard tissue file
      ext <- tools::file_ext(tissue_std_pk_file$name)
      file.rename(tissue_std_pk_file$datapath,
                  paste(tissue_std_pk_file$datapath, ext, sep = "."))
      tissue_std_pk_df <- read_excel(paste(tissue_std_pk_file$datapath, ext, sep = "."), sheet = 1)
      tissue_std_pk_clean <- tissue_std_pk_function(tissue_std_pk_df)
      tissue_std_pk_summarized <- tissue_std_pk_summarize(tissue_std_pk_clean)
      
      ## Clean in vitro file
      ext <- tools::file_ext(invitro_file$name)
      file.rename(invitro_file$datapath,
                  paste(invitro_file$datapath, ext, sep = "."))
      invitro_df <- read_excel(paste(invitro_file$datapath, ext, sep = "."), sheet = 1)
      in_vitro_clean <- in_vitro_function(invitro_df)
      
      # Combine everything into the efficacy summary file
      efficacy_summary_file <- create_summary_df(efficacy_clean_summarized,
                                                 plasma_summarized,
                                                 tissue_laser_summarized,
                                                 tissue_std_pk_summarized,
                                                 in_vitro_clean)

      in_vivo <- efficacy_summary_file %>%
        rename(Drugs = "drug") %>% 
        unite(dosage_interval, dosage:dose_int, sep = "")
      
      in_vivo_SM <- in_vivo %>% 
        gather(key = variable, value = value, -Drugs, -dosage_interval) %>% 
        mutate(variable_filtered = variable) %>% 
        mutate(variable = factor(variable, levels = c("RIM", "OCS","ICS","ULU","SLU","SLE","PLA"),
                                 labels = c("Rim (of lesion)","Outer Caseum","Inner Caseum","Uninvolved Lung",
                                            "Standard Lung", "Standard Lesion", "Plasma"))) %>% 
        mutate(dosage_interval = factor(dosage_interval, levels = c("50BID", "100QD"))) %>% 
        filter(Drugs %in% c(input$CheckBeeDrugInVivo))
      
      if(is.null(input$CheckBeeVarInVivo)) {
        return(NULL)
      }
      
      
      if(!is.null(input$CheckBeeVarInVivo)) {
        in_vivo_SM <- in_vivo_SM %>% 
          dplyr::filter(variable_filtered %in% input$CheckBeeVarInVivo)
      }
      
      if(!is.null(input$CheckBeeDrugInVivo)) {
        in_vivo_SM <- in_vivo_SM %>%
          dplyr::filter(Drugs %in% input$CheckBeeDrugInVivo)
      }
      
      in_vivo_SMplot <- in_vivo_SM %>% 
        ggplot(aes(x = dosage_interval, y = as.numeric(as.character(value)), color = Drugs)) +
        geom_beeswarm(alpha = 0.5, size = 1.5, groupOnX = FALSE) +
        labs(x = 'Dosage-Interval', y = 'Value') +
        ggtitle('In-Vivo Distribution of TB Drugs') +
        theme_few() +
        facet_wrap(~ input$CheckBeeVarInVivo, ncol = 2, scale="free")
      
      in_vivo_plotly <- ggplotly(in_vivo_SMplot)
      
      return(in_vivo_plotly)
      
    })
    
##### Dendrogram output
    
    output$dendrogram <- renderPlot({
      
      efficacy_file <- input$efficacy
      plasma_file <- input$plasma
      tissue_laser_file <- input$tissue_laser
      tissue_std_pk_file <- input$tissue_std_pk
      invitro_file <- input$invitro
      
      # Make sure you don't show an error by trying to run code before a file's been uploaded
      if(is.null(efficacy_file) | is.null(plasma_file) | is.null(tissue_laser_file) | 
         is.null(tissue_std_pk_file) | is.null(invitro_file)){
        return(NULL)
      }
      
      ## Clean efficacy file
      ext <- tools::file_ext(efficacy_file$name)
      file.rename(efficacy_file$datapath,
                  paste(efficacy_file$datapath, ext, sep = "."))
      efficacy_df <- read_excel(paste(efficacy_file$datapath, ext, sep = "."), sheet = 1)
      efficacy_clean <- efficacy_function(efficacy_df)
      efficacy_clean_summarized <- efficacy_summary_function(efficacy_clean)
      
      ## Clean plasma file
      ext <- tools::file_ext(plasma_file$name)
      file.rename(plasma_file$datapath,
                  paste(plasma_file$datapath, ext, sep = "."))
      plasma_df <- read_excel(paste(plasma_file$datapath, ext, sep = "."), sheet = 1)
      plasma_clean <- plasma_function(plasma_df)
      plasma_summarized <- plasma_summarize(plasma_clean)
      
      ## Clean laser file
      ext <- tools::file_ext(tissue_laser_file$name)
      file.rename(tissue_laser_file$datapath,
                  paste(tissue_laser_file$datapath, ext, sep = "."))
      tissue_laser_df <- read_excel(paste(tissue_laser_file$datapath, ext, sep = "."), sheet = 1)
      tissue_laser_clean <- tissue_laser_function(tissue_laser_df)
      tissue_laser_summarized <- tissue_laser_summary(tissue_laser_clean)
      
      ## Clean standard tissue file
      ext <- tools::file_ext(tissue_std_pk_file$name)
      file.rename(tissue_std_pk_file$datapath,
                  paste(tissue_std_pk_file$datapath, ext, sep = "."))
      tissue_std_pk_df <- read_excel(paste(tissue_std_pk_file$datapath, ext, sep = "."), sheet = 1)
      tissue_std_pk_clean <- tissue_std_pk_function(tissue_std_pk_df)
      tissue_std_pk_summarized <- tissue_std_pk_summarize(tissue_std_pk_clean)
      
      ## Clean in vitro file
      ext <- tools::file_ext(invitro_file$name)
      file.rename(invitro_file$datapath,
                  paste(invitro_file$datapath, ext, sep = "."))
      invitro_df <- read_excel(paste(invitro_file$datapath, ext, sep = "."), sheet = 1)
      in_vitro_clean <- in_vitro_function(invitro_df)
      
      # Combine everything into the efficacy summary file
      efficacy_summary_file <- create_summary_df(efficacy_clean_summarized,
                                                 plasma_summarized,
                                                 tissue_laser_summarized,
                                                 tissue_std_pk_summarized,
                                                 in_vitro_clean)
      
      if(input$dendrogram_radio == "by_test"){
        by_test <- efficacy_summary_file %>%
          dplyr::rename("plasma" = PLA,
                        "uninvolved_lung" = ULU,
                        "outer_caseum" = OCS,
                        "inner_caseum" = ICS,
                        "standard_lung" = SLU,
                        "standard_lesion" = SLE,
                        "macrophage_uptake" = MacUptake,
                        "human_plasma" =huPPB,
                        "mouse_plasma" = muPPB) %>% 
          dplyr::select(plasma:standard_lesion,cLogP:macrophage_uptake) %>% 
          dplyr::mutate_all(funs(scale(.))) %>%
          as.matrix() %>% 
          t() %>% 
          dist() %>% 
          hclust() %>% 
          as.dendrogram(horiz = TRUE, hang = .3) #%>% 
        #new function within function to plot colors red = invivo; blue = in vitro
      
        labelCol <- function(by_test) {
          if (is.leaf(by_test)) {
            ## fetch label
            label <- attr(by_test, "label") 
            ## set label color to red for A and B, to blue otherwise
            attr(by_test, "nodePar") <- list(lab.col=ifelse(label %in% 
                                                              c("macrophage_uptake","cLogP", "MIC_Erdman", "MICserumErd", 
                                                                "MIC_Rv","Caseum_binding",
                                                                "human_binding_plasma",
                                                                "mouse_binding_plasma"), "red", "blue"))  # red is invitro
          }
          return(by_test)
        }
        d <- dendrapply(as.dendrogram(by_test), labelCol)
        plot_horiz.dendrogram(d, side = TRUE, main = "Comparison by Test")
        #plot(d, horiz = TRUE, main = "by test", sub="color coded by test type", xlab = "")
        cols <- c("red","blue")
        legend("topright", legend = c("invitro","invivo"),
               fill = cols, border = cols, bty = "n")
        par(cex = 0.6, mar=c(9,11,10,10)) %>% #cex magnifies text; mar does axis
          par(cex = 0.6) 
        #base plot oldpar<- par(mar xxxx, oma xxx)  run at start
        # par(oldpar) rest    at begining and end of function ; side effect of function
        # try ggplot or ggdend with colors 
      } else {
        by_drug <- efficacy_summary_file %>% 
          tidyr::unite(drugdetail, drug:level, sep = "_") %>% #combine identifying data into one column, 
          mutate_at(funs(scale(.) %>% as.vector),
                    .vars = c("PLA", "ULU", "RIM", "OCS", "ICS", "SLU", "SLE", "cLogP", "huPPB","muPPB",
                              "MIC_Erdman", "MICserumErd", "MIC_Rv", "Caseum_binding", "MacUptake")) %>% #scales
          select(drugdetail, PLA:MacUptake, -ELU, -ESP) %>%  #remove efficacy 
          column_to_rownames (var = "drugdetail") %>%  #make drugdetail leaf name!  ignore warning
          dist() %>% 
          hclust() %>%  #can change method 
          as.dendrogram(horiz = TRUE, hang = .1) 
        ggdendrogram(by_drug, rotate = TRUE, theme_dendro = TRUE) +
          labs(title = "Comparison by drug, dose, dose-int, and level") +
          theme(axis.title.x = element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank())
        
      }
    })
   

##### Mouse model output
    
    output$mouse_model <- renderPlot({
      
      efficacy_file <- input$efficacy
      plasma_file <- input$plasma
      tissue_laser_file <- input$tissue_laser
      tissue_std_pk_file <- input$tissue_std_pk
      invitro_file <- input$invitro
      
      # Make sure you don't show an error by trying to run code before a file's been uploaded
      if(is.null(efficacy_file) | is.null(plasma_file) | is.null(tissue_laser_file) | 
         is.null(tissue_std_pk_file) | is.null(invitro_file)){
        return(NULL)
      }
      
      ## Clean efficacy file
      ext <- tools::file_ext(efficacy_file$name)
      file.rename(efficacy_file$datapath,
                  paste(efficacy_file$datapath, ext, sep = "."))
      efficacy_df <- read_excel(paste(efficacy_file$datapath, ext, sep = "."), sheet = 1)
      efficacy_clean <- efficacy_function(efficacy_df)
      efficacy_clean_summarized <- efficacy_summary_function(efficacy_clean)
      
      ## Clean plasma file
      ext <- tools::file_ext(plasma_file$name)
      file.rename(plasma_file$datapath,
                  paste(plasma_file$datapath, ext, sep = "."))
      plasma_df <- read_excel(paste(plasma_file$datapath, ext, sep = "."), sheet = 1)
      plasma_clean <- plasma_function(plasma_df)
      plasma_summarized <- plasma_summarize(plasma_clean)
      
      ## Clean laser file
      ext <- tools::file_ext(tissue_laser_file$name)
      file.rename(tissue_laser_file$datapath,
                  paste(tissue_laser_file$datapath, ext, sep = "."))
      tissue_laser_df <- read_excel(paste(tissue_laser_file$datapath, ext, sep = "."), sheet = 1)
      tissue_laser_clean <- tissue_laser_function(tissue_laser_df)
      tissue_laser_summarized <- tissue_laser_summary(tissue_laser_clean)
      
      ## Clean standard tissue file
      ext <- tools::file_ext(tissue_std_pk_file$name)
      file.rename(tissue_std_pk_file$datapath,
                  paste(tissue_std_pk_file$datapath, ext, sep = "."))
      tissue_std_pk_df <- read_excel(paste(tissue_std_pk_file$datapath, ext, sep = "."), sheet = 1)
      tissue_std_pk_clean <- tissue_std_pk_function(tissue_std_pk_df)
      tissue_std_pk_summarized <- tissue_std_pk_summarize(tissue_std_pk_clean)
      
      ## Clean in vitro file
      ext <- tools::file_ext(invitro_file$name)
      file.rename(invitro_file$datapath,
                  paste(invitro_file$datapath, ext, sep = "."))
      invitro_df <- read_excel(paste(invitro_file$datapath, ext, sep = "."), sheet = 1)
      in_vitro_clean <- in_vitro_function(invitro_df)
      
      # Combine everything into the efficacy summary file
      efficacy_summary_file <- create_summary_df(efficacy_clean_summarized,
                                                 plasma_summarized,
                                                 tissue_laser_summarized,
                                                 tissue_std_pk_summarized,
                                                 in_vitro_clean)    
      
      example_data <- efficacy_summary_file %>% 
        select(drug, dosage, dose_int, level, PLA, SLU, SLE) %>% 
        unite(drug_dosing, drug, dosage, dose_int, sep = "-") %>% 
        filter(level == input$mouse_level) %>% 
        gather(PLA:SLE, key = "ELEMENT", value = concentration) %>% 
        mutate(ELEMENT = factor(ELEMENT, levels = c("PLA", "SLU", "SLE"),
                                labels = c("MOUSE", "LUNGS", "LESION")),
               ELEMENT = as.character(ELEMENT))
      
      mouse <- paste0("https://raw.githubusercontent.com/KatieKey/",
                      "input_output_shiny_group/master/Shiny_App/MouseCoord.csv")
      mouse <- read_csv(mouse)
      
      mouse <- mouse %>% 
        dplyr::select("ELEMENT","HOLE","X","Y") %>% 
        left_join(example_data, by = "ELEMENT")
      
      
      #Plot drug distribution, facetted by drug_dosing
      mouse_plot <- mouse %>% 
        ggplot(aes(mapping = TRUE, x = X, y = Y, group = HOLE, 
                   fill = concentration)) +
        geom_polypath(rule = "evenodd") +
        geom_path(colour = "black", size = .5) +
        geom_segment(x=-6, y=-2, xend=-28, yend=12) +
        geom_segment(x=-6, y=-4, xend=-28, yend=-12) +
        theme_void() +
        theme(legend.position = 'right') +
        labs(title = "Biodistribution by drug and dosage", 
             subtitle = "For plasma (mouse), standard lung (lungs), standard lesion (small lesion), uninvolved lung \n(box inset), lesion rim (inset), outer caseum (inset) and inner caseum (inset) concentrations \n") +
        coord_fixed()  +
        scale_fill_viridis(option = "magma") + 
        facet_wrap(~ drug_dosing)
      
      mouse_plot
      
    })
    
######Lesion model output
    
    output$lesion_model <- renderPlot({
      
      efficacy_file <- input$efficacy
      plasma_file <- input$plasma
      tissue_laser_file <- input$tissue_laser
      tissue_std_pk_file <- input$tissue_std_pk
      invitro_file <- input$invitro
      
      # Make sure you don't show an error by trying to run code before a file's been uploaded
      if(is.null(efficacy_file) | is.null(plasma_file) | is.null(tissue_laser_file) | 
         is.null(tissue_std_pk_file) | is.null(invitro_file)){
        return(NULL)
      }
      
      ## Clean efficacy file
      ext <- tools::file_ext(efficacy_file$name)
      file.rename(efficacy_file$datapath,
                  paste(efficacy_file$datapath, ext, sep = "."))
      efficacy_df <- read_excel(paste(efficacy_file$datapath, ext, sep = "."), sheet = 1)
      efficacy_clean <- efficacy_function(efficacy_df)
      efficacy_clean_summarized <- efficacy_summary_function(efficacy_clean)
      
      ## Clean plasma file
      ext <- tools::file_ext(plasma_file$name)
      file.rename(plasma_file$datapath,
                  paste(plasma_file$datapath, ext, sep = "."))
      plasma_df <- read_excel(paste(plasma_file$datapath, ext, sep = "."), sheet = 1)
      plasma_clean <- plasma_function(plasma_df)
      plasma_summarized <- plasma_summarize(plasma_clean)
      
      ## Clean laser file
      ext <- tools::file_ext(tissue_laser_file$name)
      file.rename(tissue_laser_file$datapath,
                  paste(tissue_laser_file$datapath, ext, sep = "."))
      tissue_laser_df <- read_excel(paste(tissue_laser_file$datapath, ext, sep = "."), sheet = 1)
      tissue_laser_clean <- tissue_laser_function(tissue_laser_df)
      tissue_laser_summarized <- tissue_laser_summary(tissue_laser_clean)
      
      ## Clean standard tissue file
      ext <- tools::file_ext(tissue_std_pk_file$name)
      file.rename(tissue_std_pk_file$datapath,
                  paste(tissue_std_pk_file$datapath, ext, sep = "."))
      tissue_std_pk_df <- read_excel(paste(tissue_std_pk_file$datapath, ext, sep = "."), sheet = 1)
      tissue_std_pk_clean <- tissue_std_pk_function(tissue_std_pk_df)
      tissue_std_pk_summarized <- tissue_std_pk_summarize(tissue_std_pk_clean)
      
      ## Clean in vitro file
      ext <- tools::file_ext(invitro_file$name)
      file.rename(invitro_file$datapath,
                  paste(invitro_file$datapath, ext, sep = "."))
      invitro_df <- read_excel(paste(invitro_file$datapath, ext, sep = "."), sheet = 1)
      in_vitro_clean <- in_vitro_function(invitro_df)
      
      # Combine everything into the efficacy summary file
      efficacy_summary_file <- create_summary_df(efficacy_clean_summarized,
                                                 plasma_summarized,
                                                 tissue_laser_summarized,
                                                 tissue_std_pk_summarized,
                                                 in_vitro_clean)    
      
      example_data1 <- efficacy_summary_file %>% 
        select(drug, dosage, dose_int, level, ULU, RIM, OCS, ICS) %>% 
        unite(drug_dosing, drug, dosage, dose_int, sep = "-") %>% 
        filter(level == input$lesion_level) %>% 
        gather(ULU:ICS, key = "AREA", value = concentration) %>% 
        mutate(AREA = factor(AREA, levels = c("ULU", "RIM", "OCS", "ICS"),
                             labels = c("LUNG", "RIM", "OUTER", "INNER")),
               AREA = as.character(AREA))
      
      lesion <- paste0("https://raw.githubusercontent.com/KatieKey/",
                       "input_output_shiny_group/master/Shiny_App/LesionCoord.csv")
      lesion <- read_csv(lesion)
        
        lesion <- lesion %>% 
        left_join(example_data1, by = "AREA")
      
      #Plot drug distribution, facetted by drug_dosing
     lesion_plot <- ggplot(data =lesion, aes(mapping = TRUE, x = X, y = Y, group = HOLE, fill = concentration)) +
        geom_polypath(rule = "evenodd") +
        geom_path(colour = "black", size = .5) +
        theme_void() +
        theme(legend.position = 'right') +
        labs(title = "Biodistribution by drug and dosage",
             subtitle = "For uninvolved lung, rim (of lesion), outer caseum,  
             and inner caseum concentrations \n") +
        coord_fixed()  +
        scale_fill_viridis(option = "magma") + 
        facet_wrap(~ drug_dosing)
      
      lesion_plot
  
    })
    

######INDEPENDENT DEPENDENT GROUP FUNCTIONS
    ##regression tree
    
    output$regression_tree <- renderPlot({
      
      efficacy_file <- input$efficacy
      plasma_file <- input$plasma
      tissue_laser_file <- input$tissue_laser
      tissue_std_pk_file <- input$tissue_std_pk
      invitro_file <- input$invitro
      
      # Make sure you don't show an error by trying to run code before a file's been uploaded
      if(is.null(efficacy_file) | is.null(plasma_file) | is.null(tissue_laser_file) | 
         is.null(tissue_std_pk_file) | is.null(invitro_file)){
        return(NULL)
      }
      
      ## Clean efficacy file
      ext <- tools::file_ext(efficacy_file$name)
      file.rename(efficacy_file$datapath,
                  paste(efficacy_file$datapath, ext, sep = "."))
      efficacy_df <- read_excel(paste(efficacy_file$datapath, ext, sep = "."), sheet = 1)
      efficacy_clean <- efficacy_function(efficacy_df)
      efficacy_clean_summarized <- efficacy_summary_function(efficacy_clean)
      
      ## Clean plasma file
      ext <- tools::file_ext(plasma_file$name)
      file.rename(plasma_file$datapath,
                  paste(plasma_file$datapath, ext, sep = "."))
      plasma_df <- read_excel(paste(plasma_file$datapath, ext, sep = "."), sheet = 1)
      plasma_clean <- plasma_function(plasma_df)
      plasma_summarized <- plasma_summarize(plasma_clean)
      
      ## Clean laser file
      ext <- tools::file_ext(tissue_laser_file$name)
      file.rename(tissue_laser_file$datapath,
                  paste(tissue_laser_file$datapath, ext, sep = "."))
      tissue_laser_df <- read_excel(paste(tissue_laser_file$datapath, ext, sep = "."), sheet = 1)
      tissue_laser_clean <- tissue_laser_function(tissue_laser_df)
      tissue_laser_summarized <- tissue_laser_summary(tissue_laser_clean)
      
      ## Clean standard tissue file
      ext <- tools::file_ext(tissue_std_pk_file$name)
      file.rename(tissue_std_pk_file$datapath,
                  paste(tissue_std_pk_file$datapath, ext, sep = "."))
      tissue_std_pk_df <- read_excel(paste(tissue_std_pk_file$datapath, ext, sep = "."), sheet = 1)
      tissue_std_pk_clean <- tissue_std_pk_function(tissue_std_pk_df)
      tissue_std_pk_summarized <- tissue_std_pk_summarize(tissue_std_pk_clean)
      
      ## Clean in vitro file
      ext <- tools::file_ext(invitro_file$name)
      file.rename(invitro_file$datapath,
                  paste(invitro_file$datapath, ext, sep = "."))
      invitro_df <- read_excel(paste(invitro_file$datapath, ext, sep = "."), sheet = 1)
      in_vitro_clean <- in_vitro_function(invitro_df)
      
      # Combine everything into the efficacy summary file
      efficacy_summary_file <- create_summary_df(efficacy_clean_summarized,
                                                 plasma_summarized,
                                                 tissue_laser_summarized,
                                                 tissue_std_pk_summarized,
                                                 in_vitro_clean)

      if(is.null(efficacy_summary_file)){
        return(NULL)
      }
      
      if (input$regression == "ELU") {
        
        function_data <- efficacy_summary_file %>%
          filter(!is.na(ELU)) %>% 
          rename(plasma = PLA, `Uninvolved lung` = ULU,
                 `Rim (of Lesion)` = RIM, `Outer Caseum` = OCS, `Inner Caseum` = ICS,
                 `Standard Lung` = SLU, `Standard Lesion` = SLE, `Human Plasma Binding` = huPPB,
                 `Mouse Plasma Binding` = muPPB, `MIC Erdman Strain` = MIC_Erdman,
                 `MIC Erdman Strain with Serum` = MICserumErd, `MIC rv strain` = MIC_Rv,
                 `Caseum binding` = Caseum_binding, `Macrophage Uptake (Ratio)` = MacUptake)
        
        tree <- rpart(ELU ~  drug + dosage + level + 
                        plasma + `Uninvolved lung` + `Rim (of Lesion)` + `Outer Caseum` + 
                        `Inner Caseum` + `Standard Lung` + `Standard Lesion` + 
                        cLogP + `Human Plasma Binding` + `Mouse Plasma Binding` + 
                        `MIC Erdman Strain` + `MIC Erdman Strain with Serum` + `MIC rv strain` + 
                        `Caseum binding` + `Macrophage Uptake (Ratio)`,
                      data = function_data, 
                      control = rpart.control(cp = -1, minsplit = input$min_split, 
                                              minbucket = input$min_bucket))
        return(rpart.plot(tree))
      }
      
      if (input$regression == "ESP") {
        
        function_data <- efficacy_summary_file %>%
          filter(!is.na(ESP)) %>% 
          rename(plasma = PLA, `Uninvolved lung` = ULU,
                 `Rim (of Lesion)` = RIM, `Outer Caseum` = OCS, `Inner Caseum` = ICS,
                 `Standard Lung` = SLU, `Standard Lesion` = SLE, `Human Plasma Binding` = huPPB,
                 `Mouse Plasma Binding` = muPPB, `MIC Erdman Strain` = MIC_Erdman,
                 `MIC Erdman Strain with Serum` = MICserumErd, `MIC rv strain` = MIC_Rv,
                 `Caseum binding` = Caseum_binding, `Macrophage Uptake (Ratio)` = MacUptake)
        
        tree <- rpart(ESP ~  drug + dosage + level + 
                        plasma + `Uninvolved lung` + `Rim (of Lesion)` + `Outer Caseum` + 
                        `Inner Caseum` + `Standard Lung` + `Standard Lesion` + 
                        cLogP + `Human Plasma Binding` + `Mouse Plasma Binding` + 
                        `MIC Erdman Strain` + `MIC Erdman Strain with Serum` + `MIC rv strain` + 
                        `Caseum binding` + `Macrophage Uptake (Ratio)`,
                      data = function_data, 
                      control = rpart.control(cp = -1, minsplit = input$min_split, 
                                              minbucket = input$min_bucket))
        return(rpart.plot(tree))
      }
        
}) 
    
    
###### best variables output
    
    output$best_variables <- renderPlotly({
      
      efficacy_file <- input$efficacy
      plasma_file <- input$plasma
      tissue_laser_file <- input$tissue_laser
      tissue_std_pk_file <- input$tissue_std_pk
      invitro_file <- input$invitro
      
      # Make sure you don't show an error by trying to run code before a file's been uploaded
      if(is.null(efficacy_file) | is.null(plasma_file) | is.null(tissue_laser_file) | 
         is.null(tissue_std_pk_file) | is.null(invitro_file)){
        return(NULL)
      }
      
      ## Clean efficacy file
      ext <- tools::file_ext(efficacy_file$name)
      file.rename(efficacy_file$datapath,
                  paste(efficacy_file$datapath, ext, sep = "."))
      efficacy_df <- read_excel(paste(efficacy_file$datapath, ext, sep = "."), sheet = 1)
      efficacy_clean <- efficacy_function(efficacy_df)
      efficacy_clean_summarized <- efficacy_summary_function(efficacy_clean)
      
      ## Clean plasma file
      ext <- tools::file_ext(plasma_file$name)
      file.rename(plasma_file$datapath,
                  paste(plasma_file$datapath, ext, sep = "."))
      plasma_df <- read_excel(paste(plasma_file$datapath, ext, sep = "."), sheet = 1)
      plasma_clean <- plasma_function(plasma_df)
      plasma_summarized <- plasma_summarize(plasma_clean)
      
      ## Clean laser file
      ext <- tools::file_ext(tissue_laser_file$name)
      file.rename(tissue_laser_file$datapath,
                  paste(tissue_laser_file$datapath, ext, sep = "."))
      tissue_laser_df <- read_excel(paste(tissue_laser_file$datapath, ext, sep = "."), sheet = 1)
      tissue_laser_clean <- tissue_laser_function(tissue_laser_df)
      tissue_laser_summarized <- tissue_laser_summary(tissue_laser_clean)
      
      ## Clean standard tissue file
      ext <- tools::file_ext(tissue_std_pk_file$name)
      file.rename(tissue_std_pk_file$datapath,
                  paste(tissue_std_pk_file$datapath, ext, sep = "."))
      tissue_std_pk_df <- read_excel(paste(tissue_std_pk_file$datapath, ext, sep = "."), sheet = 1)
      tissue_std_pk_clean <- tissue_std_pk_function(tissue_std_pk_df)
      tissue_std_pk_summarized <- tissue_std_pk_summarize(tissue_std_pk_clean)
      
      ## Clean in vitro file
      ext <- tools::file_ext(invitro_file$name)
      file.rename(invitro_file$datapath,
                  paste(invitro_file$datapath, ext, sep = "."))
      invitro_df <- read_excel(paste(invitro_file$datapath, ext, sep = "."), sheet = 1)
      in_vitro_clean <- in_vitro_function(invitro_df)
      
      # Combine everything into the efficacy summary file
      efficacy_summary_file <- create_summary_df(efficacy_clean_summarized,
                                                 plasma_summarized,
                                                 tissue_laser_summarized,
                                                 tissue_std_pk_summarized,
                                                 in_vitro_clean)
      
      variable_definitions <- paste0("https://raw.githubusercontent.com/KatieKey/",
                                     "input_output_shiny_group/master/Shiny_App/variable_definitions.csv")
      variable_definitions <- read_csv(variable_definitions)
      
    if(input$variable == "ELU"){
      dataset <- efficacy_summary_file %>% 
        select(-ESP) %>% 
        mutate(huPPB = as.numeric(huPPB), 
               muPPB = as.numeric(muPPB), 
               dosage = as.factor(dosage), 
               dose_int = as.factor(dose_int), 
               level = as.factor(level), 
               drug = as.factor(drug))
      
      efficacy.rf <- randomForest( ELU~ ., data =dataset,
                                   na.action = na.roughfix,
                                   ntree= 500, 
                                   importance = TRUE)
      graph <-importance(efficacy.rf, type = 1) %>% 
        as.data.frame() %>% 
        rownames_to_column() %>% 
        rename(variable = rowname, 
               mse = `%IncMSE`) %>% 
        left_join(variable_definitions, 
                  by = c("variable" = "Name"))
      
      
      test <- graph %>% 
        filter(mse > 0) %>% 
        ggplot()+
        geom_point(aes(x = mse, 
                       y = reorder(Label, mse), 
                       color = Vitro_or_Vivo, 
                       text = paste('Mean Standard Error: ', round(mse, digits = 2), '\n',
                                    'Variable: ', Label, '\n',
                                    'Definition: ', Definition
                       )))+
        theme_minimal()+
        ggtitle("Predicting Variable Importance Using Lung Efficacy")+
        labs(y = "Variable", 
             x = "Importance", 
             color = " " )
      
      
      return(ggplotly(test, tooltip = c("text")))
      
      
    }
    
    if (input$variable == "ESP"){
      dataset <- efficacy_summary_file %>% 
        select(-ELU) %>% 
        mutate(huPPB = as.numeric(huPPB), 
               muPPB = as.numeric(muPPB), 
               dosage = as.factor(dosage), 
               dose_int = as.factor(dose_int), 
               level = as.factor(level), 
               drug = as.factor(drug))
      
      efficacy.rf <- randomForest( ESP ~ ., data =dataset,
                                   na.action = na.roughfix,
                                   ntree= 500, 
                                   importance = TRUE)
      
      graph <-importance(efficacy.rf, type = 1) %>% 
        as.data.frame() %>% 
        rownames_to_column() %>% 
        rename(variable = rowname, 
               mse = `%IncMSE`) %>% 
        left_join(variable_definitions, 
                  by = c("variable" = "Name"))
      
      
      test <- graph %>% 
        filter(mse > 0) %>% 
        ggplot()+
        geom_point(aes(x = mse, 
                       y = reorder(Label, mse), 
                       color = Vitro_or_Vivo, 
                       text = paste('Mean Standard Error: ', round(mse, digits = 2), '\n',
                                    'Variable: ', Label, '\n',
                                    'Definition: ', Definition
                       )))+
        theme_minimal()+
        ggtitle("Predicting Variable Importance Using Spleen Efficacy")+
        labs(y = "Variable", 
             x = "Importance", 
             color = " " )
      
      
      return(ggplotly(test, tooltip = c("text")))
    }
    
    })
    
    
    
####Scatter Plot Output
    
    output$scatter_plot <- renderPlot({
      
      efficacy_file <- input$efficacy
      plasma_file <- input$plasma
      tissue_laser_file <- input$tissue_laser
      tissue_std_pk_file <- input$tissue_std_pk
      invitro_file <- input$invitro
      
      # Make sure you don't show an error by trying to run code before a file's been uploaded
      if(is.null(efficacy_file) | is.null(plasma_file) | is.null(tissue_laser_file) | 
         is.null(tissue_std_pk_file) | is.null(invitro_file)){
        return(NULL)
      }
      
      ## Clean efficacy file
      ext <- tools::file_ext(efficacy_file$name)
      file.rename(efficacy_file$datapath,
                  paste(efficacy_file$datapath, ext, sep = "."))
      efficacy_df <- read_excel(paste(efficacy_file$datapath, ext, sep = "."), sheet = 1)
      efficacy_clean <- efficacy_function(efficacy_df)
      efficacy_clean_summarized <- efficacy_summary_function(efficacy_clean)
      
      ## Clean plasma file
      ext <- tools::file_ext(plasma_file$name)
      file.rename(plasma_file$datapath,
                  paste(plasma_file$datapath, ext, sep = "."))
      plasma_df <- read_excel(paste(plasma_file$datapath, ext, sep = "."), sheet = 1)
      plasma_clean <- plasma_function(plasma_df)
      plasma_summarized <- plasma_summarize(plasma_clean)
      
      ## Clean laser file
      ext <- tools::file_ext(tissue_laser_file$name)
      file.rename(tissue_laser_file$datapath,
                  paste(tissue_laser_file$datapath, ext, sep = "."))
      tissue_laser_df <- read_excel(paste(tissue_laser_file$datapath, ext, sep = "."), sheet = 1)
      tissue_laser_clean <- tissue_laser_function(tissue_laser_df)
      tissue_laser_summarized <- tissue_laser_summary(tissue_laser_clean)
      
      ## Clean standard tissue file
      ext <- tools::file_ext(tissue_std_pk_file$name)
      file.rename(tissue_std_pk_file$datapath,
                  paste(tissue_std_pk_file$datapath, ext, sep = "."))
      tissue_std_pk_df <- read_excel(paste(tissue_std_pk_file$datapath, ext, sep = "."), sheet = 1)
      tissue_std_pk_clean <- tissue_std_pk_function(tissue_std_pk_df)
      tissue_std_pk_summarized <- tissue_std_pk_summarize(tissue_std_pk_clean)
      
      ## Clean in vitro file
      ext <- tools::file_ext(invitro_file$name)
      file.rename(invitro_file$datapath,
                  paste(invitro_file$datapath, ext, sep = "."))
      invitro_df <- read_excel(paste(invitro_file$datapath, ext, sep = "."), sheet = 1)
      in_vitro_clean <- in_vitro_function(invitro_df)
      
      # Combine everything into the efficacy summary file
      efficacy_summary_file <- create_summary_df(efficacy_clean_summarized,
                                                 plasma_summarized,
                                                 tissue_laser_summarized,
                                                 tissue_std_pk_summarized,
                                                 in_vitro_clean)
      efficacy_summary_file
        
        function_data <- efficacy_summary_file %>% 
          filter(level == input$scatter_level) %>% 
          gather(key = independent_var, value = indep_measure, -drug, -dosage, -dose_int, -level, -ELU, -ESP, na.rm = TRUE) %>% 
          select(drug, dosage, dose_int, level, input$scatter_variable, indep_measure, independent_var) 
        
        if(input$scatter_variable == "ELU") {vect <- function_data$ELU}
        if(input$scatter_variable == "ESP") {vect <- function_data$ESP}
        
        scatter_plot <- function_data %>%
          ggplot(aes(x = indep_measure, y = vect, color = dose_int)) +
          geom_point(alpha = 0.5) +
          geom_smooth(method = lm, se = FALSE) +
          labs(x = "Independent variable", y = "Dependent Variable") +
          facet_wrap(~independent_var, scales = "free_x")
        
        return(scatter_plot)

    })
    
#######LINEAR MODEL
    
    output$linear_model <- renderPlot({
      
      efficacy_file <- input$efficacy
      plasma_file <- input$plasma
      tissue_laser_file <- input$tissue_laser
      tissue_std_pk_file <- input$tissue_std_pk
      invitro_file <- input$invitro
      
      # Make sure you don't show an error by trying to run code before a file's been uploaded
      if(is.null(efficacy_file) | is.null(plasma_file) | is.null(tissue_laser_file) | 
         is.null(tissue_std_pk_file) | is.null(invitro_file)){
        return(NULL)
      }
      
      ## Clean efficacy file
      ext <- tools::file_ext(efficacy_file$name)
      file.rename(efficacy_file$datapath,
                  paste(efficacy_file$datapath, ext, sep = "."))
      efficacy_df <- read_excel(paste(efficacy_file$datapath, ext, sep = "."), sheet = 1)
      efficacy_clean <- efficacy_function(efficacy_df)
      efficacy_clean_summarized <- efficacy_summary_function(efficacy_clean)
      
      ## Clean plasma file
      ext <- tools::file_ext(plasma_file$name)
      file.rename(plasma_file$datapath,
                  paste(plasma_file$datapath, ext, sep = "."))
      plasma_df <- read_excel(paste(plasma_file$datapath, ext, sep = "."), sheet = 1)
      plasma_clean <- plasma_function(plasma_df)
      plasma_summarized <- plasma_summarize(plasma_clean)
      
      ## Clean laser file
      ext <- tools::file_ext(tissue_laser_file$name)
      file.rename(tissue_laser_file$datapath,
                  paste(tissue_laser_file$datapath, ext, sep = "."))
      tissue_laser_df <- read_excel(paste(tissue_laser_file$datapath, ext, sep = "."), sheet = 1)
      tissue_laser_clean <- tissue_laser_function(tissue_laser_df)
      tissue_laser_summarized <- tissue_laser_summary(tissue_laser_clean)
      
      ## Clean standard tissue file
      ext <- tools::file_ext(tissue_std_pk_file$name)
      file.rename(tissue_std_pk_file$datapath,
                  paste(tissue_std_pk_file$datapath, ext, sep = "."))
      tissue_std_pk_df <- read_excel(paste(tissue_std_pk_file$datapath, ext, sep = "."), sheet = 1)
      tissue_std_pk_clean <- tissue_std_pk_function(tissue_std_pk_df)
      tissue_std_pk_summarized <- tissue_std_pk_summarize(tissue_std_pk_clean)
      
      ## Clean in vitro file
      ext <- tools::file_ext(invitro_file$name)
      file.rename(invitro_file$datapath,
                  paste(invitro_file$datapath, ext, sep = "."))
      invitro_df <- read_excel(paste(invitro_file$datapath, ext, sep = "."), sheet = 1)
      in_vitro_clean <- in_vitro_function(invitro_df)
      
      # Combine everything into the efficacy summary file
      efficacy_summary_file <- create_summary_df(efficacy_clean_summarized,
                                                 plasma_summarized,
                                                 tissue_laser_summarized,
                                                 tissue_std_pk_summarized,
                                                 in_vitro_clean)
    
      if(input$linear_variable == "ELU") {
      
      function_data <- efficacy_summary_file %>% 
        filter(level == input$linear_level) %>% 
        gather(key = independent_var, value = indep_measure, -drug, -dosage, 
               -dose_int, -level, -ELU, -ESP, na.rm = TRUE) %>% 
        select(drug, dosage, dose_int, level, ELU, indep_measure, independent_var) 
      
      function_data$vect <- function_data$ELU
      
      model_function <- function(data) {
        model_results <- lm(vect ~ scale(indep_measure), data = data)
      }
      
      estimate_results <- function_data %>% 
        group_by(independent_var, dose_int) %>% 
        nest() %>% 
        mutate(mod_results = purrr::map(data, model_function)) %>% 
        mutate(mod_coefs = purrr::map(mod_results, broom::tidy)) %>% 
        select(independent_var, dose_int, mod_results, mod_coefs) %>% 
        unnest(mod_coefs) %>% 
        filter(term == "scale(indep_measure)")
      
      coef_plot <- estimate_results %>%
        mutate(independent_var = forcats::fct_reorder(independent_var, estimate, fun = max)) %>%
        rename(Dose_Interval = dose_int) %>% 
        ggplot(aes(x = estimate, y = independent_var, color = Dose_Interval)) +
        geom_point(aes(size = 1 / std.error)) +
        scale_size_continuous(guide = FALSE) +
        theme_few() + 
        ggtitle(label = "Linear model coefficients as function of independent variables, \n by drug dose and model uncertainty", subtitle = "smaller points have more uncertainty than larger points") +
        geom_vline(xintercept = 0, color = "cornflower blue") 
      
      return(coef_plot)
    }
      
      if(input$linear_variable == "ESP") {
      
      function_data <- efficacy_summary_file %>% 
        filter(level == input$linear_level) %>% 
        gather(key = independent_var, value = indep_measure, -drug, -dosage, 
               -dose_int, -level, -ELU, -ESP, na.rm = TRUE) %>% 
        select(drug, dosage, dose_int, level, ESP, indep_measure, independent_var) 
      
      function_data$vect <- function_data$ESP
      
      model_function <- function(data) {
        model_results <- lm(vect ~ scale(indep_measure), data = data)
      }
      
      estimate_results <- function_data %>% 
        group_by(independent_var, dose_int) %>% 
        nest() %>% 
        mutate(mod_results = purrr::map(data, model_function)) %>% 
        mutate(mod_coefs = purrr::map(mod_results, broom::tidy)) %>% 
        select(independent_var, dose_int, mod_results, mod_coefs) %>% 
        unnest(mod_coefs) %>% 
        filter(term == "scale(indep_measure)")
      
      coef_plot <- estimate_results %>%
        mutate(independent_var = forcats::fct_reorder(independent_var, estimate, fun = max)) %>%
        rename(Dose_Interval = dose_int) %>% 
        ggplot(aes(x = estimate, y = independent_var, color = Dose_Interval)) +
        geom_point(aes(size = 1 / std.error)) +
        scale_size_continuous(guide = FALSE) +
        theme_few() + 
        ggtitle(label = "Linear model coefficients as function of independent variables, \n by drug dose and model uncertainty", subtitle = "smaller points have more uncertainty than larger points") +
        geom_vline(xintercept = 0, color = "cornflower blue") 
      
      return(coef_plot)
      
      }
    })
    
######### not sure if we need this plot since it uses clean_2_combined.csv
    
    
    
#########LASSO Model Output
    
    output$lasso_model <- DT::renderDataTable ({
      
      efficacy_file <- input$efficacy
      plasma_file <- input$plasma
      tissue_laser_file <- input$tissue_laser
      tissue_std_pk_file <- input$tissue_std_pk
      invitro_file <- input$invitro
      
      # Make sure you don't show an error by trying to run code before a file's been uploaded
      if(is.null(efficacy_file) | is.null(plasma_file) | is.null(tissue_laser_file) | 
         is.null(tissue_std_pk_file) | is.null(invitro_file)){
        return(NULL)
      }
      
      ## Clean efficacy file
      ext <- tools::file_ext(efficacy_file$name)
      file.rename(efficacy_file$datapath,
                  paste(efficacy_file$datapath, ext, sep = "."))
      efficacy_df <- read_excel(paste(efficacy_file$datapath, ext, sep = "."), sheet = 1)
      efficacy_clean <- efficacy_function(efficacy_df)
      efficacy_clean_summarized <- efficacy_summary_function(efficacy_clean)
      
      ## Clean plasma file
      ext <- tools::file_ext(plasma_file$name)
      file.rename(plasma_file$datapath,
                  paste(plasma_file$datapath, ext, sep = "."))
      plasma_df <- read_excel(paste(plasma_file$datapath, ext, sep = "."), sheet = 1)
      plasma_clean <- plasma_function(plasma_df)
      plasma_summarized <- plasma_summarize(plasma_clean)
      
      ## Clean laser file
      ext <- tools::file_ext(tissue_laser_file$name)
      file.rename(tissue_laser_file$datapath,
                  paste(tissue_laser_file$datapath, ext, sep = "."))
      tissue_laser_df <- read_excel(paste(tissue_laser_file$datapath, ext, sep = "."), sheet = 1)
      tissue_laser_clean <- tissue_laser_function(tissue_laser_df)
      tissue_laser_summarized <- tissue_laser_summary(tissue_laser_clean)
      
      ## Clean standard tissue file
      ext <- tools::file_ext(tissue_std_pk_file$name)
      file.rename(tissue_std_pk_file$datapath,
                  paste(tissue_std_pk_file$datapath, ext, sep = "."))
      tissue_std_pk_df <- read_excel(paste(tissue_std_pk_file$datapath, ext, sep = "."), sheet = 1)
      tissue_std_pk_clean <- tissue_std_pk_function(tissue_std_pk_df)
      tissue_std_pk_summarized <- tissue_std_pk_summarize(tissue_std_pk_clean)
      
      ## Clean in vitro file
      ext <- tools::file_ext(invitro_file$name)
      file.rename(invitro_file$datapath,
                  paste(invitro_file$datapath, ext, sep = "."))
      invitro_df <- read_excel(paste(invitro_file$datapath, ext, sep = "."), sheet = 1)
      in_vitro_clean <- in_vitro_function(invitro_df)
      
      # Combine everything into the efficacy summary file
      efficacy_summary_file <- create_summary_df(efficacy_clean_summarized,
                                                 plasma_summarized,
                                                 tissue_laser_summarized,
                                                 tissue_std_pk_summarized,
                                                 in_vitro_clean)
      
    if (input$lasso_dosage == "50"){
      dataz_1 <- efficacy_summary_file %>% 
        na.omit(efficacy_summary_file) %>% 
        dplyr::mutate(dosage = as.numeric(as.integer(dosage))) %>% 
        dplyr::select_if(is.numeric) %>%
        filter(dosage == 50)
      
      response_1 <- dataz_1 %>% 
        dplyr::select(input$variable_lasso)
      
      predictors_1 <- dataz_1 %>%
        dplyr::select(c("PLA", "ULU", "RIM", "OCS", "ICS", "SLU", "SLE", "cLogP", "huPPB", 
                        "muPPB", "MIC_Erdman", 'MICserumErd', "MIC_Rv", "Caseum_binding", "MacUptake"))
      
      y <- as.numeric(unlist(response_1))
      x <- as.matrix(predictors_1)
      
      fit =  glmnet(x, y)
      
      coeff <- coef(fit,s=0.1)
      coeff <- as.data.frame(as.matrix(coeff)) %>% 
        rownames_to_column() 
      colnames(coeff) <- c("predictor", "coeff")
      
      coeff <- coeff %>% 
        dplyr::filter(coeff > 0)
      return(coeff)
    }
      
      if (input$lasso_dosage == "100"){
        dataz_2 <- efficacy_summary_file %>% 
          na.omit(efficacy_summary_file) %>% 
          dplyr::mutate(dosage = as.numeric(as.integer(dosage))) %>% 
          dplyr::select_if(is.numeric) %>%
          filter(dosage == 100)
        
        response_2 <- dataz_2 %>% 
          dplyr::select(input$variable_lasso)
        
        predictors_2 <- dataz_2 %>%
          dplyr::select(c("PLA", "ULU", "RIM", "OCS", "ICS", "SLU", "SLE", "cLogP", "huPPB", 
                   "muPPB", "MIC_Erdman", 'MICserumErd', "MIC_Rv", "Caseum_binding", "MacUptake"))
        
        y <- as.numeric(unlist(response_2))
        x <- as.matrix(predictors_2)
        
        fit =  glmnet(x, y)
        
        coeff <- coef(fit,s=0.1)
        coeff <- as.data.frame(as.matrix(coeff)) %>% 
          rownames_to_column() 
        colnames(coeff) <- c("predictor", "coeff")
        
        coeff <- coeff %>% 
          dplyr::filter(coeff > 0)
        return(coeff)
      }
      
    })
    
       
}



# Run the application 
shinyApp(ui = ui, server = server)



