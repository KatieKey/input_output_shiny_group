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

source("helper.R")
source("Group3Functions.R")

# Define UI for application 
ui <- fluidPage(
  
  titlePanel("Mycobacteria Research Laboratories"),
  helpText("Upload Data and Explore Data Tables and Graphs Using the Tabs Below"),
  
  sidebarLayout(
    sidebarPanel(width = 3, (label = h3("Upload Data")),
                 
                 fileInput(label = "Efficacy", inputId = "efficacy",
                           buttonLabel = "Efficacy Data", multiple = TRUE, accept = ".xlsx"),
                 fileInput(label = "Plasma", inputId = "plasma", 
                           buttonLabel = "Plasma Data", multiple = TRUE, accept = ".xlsx"),
                 fileInput(label = "Tissue Laser", inputId = "tissue_laser", 
                           buttonLabel = "Tissue Laser Data", multiple = TRUE, accept = ".xlsx"),
                 fileInput(label = "Tissue Std PK", inputId = "tissue_std_pk", 
                           buttonLabel = "Tissue Std PK Data", multiple = TRUE, accept = ".xlsx")
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
                                       tabPanel("Efficacy Summary",
                                                helpText("Used for Beeswarm Data in 'Independent' Tab"),
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
                                                )
                                        )
                                        ),
                  
                  tabPanel("Independent", 
                           tabsetPanel(type = "tabs",
                                       tabPanel("Beeswarm",
                                checkboxGroupInput("CheckBeeVarInVitro", 
                                label = h3("Check Variables To Explore"), 
                                choices = list("Caseum_binding" = efficacy_summary_file$Caseum_binding, 
                                               "cLogP" = efficacy_summary_file$cLogP,
                                               "huPPB" = efficacy_summary_file$huPPB,
                                               "muPPB" = efficacy_summary_file$muPPB,
                                               "MIC_Erdman" = efficacy_summary_file$MIC_Erdman,
                                               "MICserumErd" = efficacy_summary_file$MICserumErd,
                                               "MIC_Rv" = efficacy_summary_file$MIC_Rv,
                                               "MacUptake" = efficacy_summary_file$MacUptake)
                                  ),
                                checkboxGroupInput("CheckBeeDrugInVitro", 
                                 label = h3("Check Drugs To Explore"), 
                                 choices = list("DRUG1" = 1, "DRUG2" = 2, 
                                                "DRUG3" = 3,
                                 "DRUG4" = 4, "DRUG5" = 5, "DRUG6" = 6,
                                 "DRUG7" = 7, "DRUG8" = 8, "DRUG9" = 9,
                                 "DRUG10" = 10, "DRUG11" = 11)
                                 ),
                                 plotOutput("beeswarm_invitro_plot")
                                 ),
                                       tabPanel("Plot2"),
                                       tabPanel("Plot3")
                                )
                                ),
                  
                  tabPanel("Independent ~ Dependent", 
                           tabsetPanel(type = "tabs",
                                       tabPanel("RegressionTree",
                                                radioButtons("regression", label = "Pick a Variable",
                                                             choices = list("Lung Efficacy" = efficacy_summary_file$ELU,
                                                                         "Spleen Efficacy" = efficacy_summary_file$ESP)
                                                             ),
                                                plotOutput("regression_tree")
                                                ),
                                       tabPanel("Drews"),
                                       tabPanel("KateScatter"),
                                       tabPanel("KateCoefficient"),
                                       tabPanel("Maggie")
                           )
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
    output$raw_efficacy_summary_table <- DT::renderDataTable({
      efficacy_summary_file <- input$efficacy_summary
      
      # Make sure you don't show an error by trying to run code before a file's been uploaded
      if(is.null(efficacy_summary_file)){
        return(NULL)
      }
      
      ext <- tools::file_ext(efficacy_summary_file$name)
      file.rename(efficacy_summary_file$datapath, 
                  paste(efficacy_summary_file$datapath, ext, sep = "."))
      read_excel(paste(efficacy_summary_file$datapath, ext, sep = "."), sheet = 1)
      
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
  
# Render data table with cleaned efficacy summary data
    output$clean_efficacy_summary_table <- DT::renderDataTable({ input$efficacy_summary_file
    # Make sure you don't show an error by trying to run code before a file's been uploaded
    if(is.null(efficacy_summary_file)){
      return(NULL)
    }
      efficacy_summary_file_1 <- paste0("https://raw.githubusercontent.com/KatieKey/input_output_shiny_group/",
                                      "master/CSV_Files/efficacy_summary.csv")
      efficacy_summary_file <- read_csv(efficacy_summary_file_1)
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
  
    
#####INDEPENDENT GROUPS FUNCTIONS
    #CheckBeeDrugInVitro
    #CheckBeeVarInVitro
    
    output$beeswarm_invitro_plot <- renderPlot({
    

    return(efficacy_summary_file)
    
    })
   
    
######INDEPENDENT DEPENDENT GROUP FUNCTIONS
    output$regression_tree <- renderPlot({

      if(is.null(efficacy_summary_file)){
        return(NULL)
      }
      
      dep_var <- input$regression
      regression_tree_function(dep_var, efficacy_summary_file)
 
    }) 
    
}


# Run the application 
shinyApp(ui = ui, server = server)



