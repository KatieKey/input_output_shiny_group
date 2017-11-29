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
library(DT) 

source("helper.R")

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
                           buttonLabel = "Tissue Std PK Data", multiple = TRUE, accept = ".xlsx"),
                 fileInput(label = "In Vitro", inputId = "in_vitro", 
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
                                      DT::dataTableOutput("raw_in_vitro_table")
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
                                                DT::dataTableOutput("clean_in_vitro_table")
                                               )
                           )
                           ),
                           
                  tabPanel("Summary", 
                           tabsetPanel(type = "tabs",
                                       tabPanel("Efficacy"),
                                       tabPanel("Plasma"),
                                       tabPanel("Tissue Laser"),
                                       tabPanel("Tissue Std PK"),
                                       tabPanel("In Vitro")
                           )
                           ),
                  
                  tabPanel("Independent", 
                           tabsetPanel(type = "tabs",
                                       tabPanel("Efficacy"),
                                       tabPanel("Plasma"),
                                       tabPanel("Tissue Laser"),
                                       tabPanel("Tissue Std PK"),
                                       tabPanel("In Vitro")
                           )
                           ),
                  
                  tabPanel("Independent ~ Dependent", 
                           tabsetPanel(type = "tabs",
                                       tabPanel("Efficacy"),
                                       tabPanel("Plasma"),
                                       tabPanel("Tissue Laser"),
                                       tabPanel("Tissue Std PK"),
                                       tabPanel("In Vitro")
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
    output$raw_in_vitro_table <- DT::renderDataTable({
      in_vitro_file <- input$in_vitro
      
      # Make sure you don't show an error by trying to run code before a file's been uploaded
      if(is.null(in_vitro_file)){
        return(NULL)
      }
      
      ext <- tools::file_ext(in_vitro_file$name)
      file.rename(in_vitro_file$datapath, 
                  paste(in_vitro_file$datapath, ext, sep = "."))
      read_excel(paste(in_vitro_file$datapath, ext, sep = "."), sheet = 1)
      
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
  
# Render data table with in vitro data
  output$clean_in_vitro_table <- DT::renderDataTable({
    in_vitro_file <- input$in_vitro
    
    # Make sure you don't show an error by trying to run code before a file's been uploaded
    if(is.null(in_vitro_file)){
      return(NULL)
    }
    
    ext <- tools::file_ext(in_vitro_file$name)
    file.rename(in_vitro_file$datapath, 
                paste(in_vitro_file$datapath, ext, sep = "."))
    in_vitro_df <- read_excel(paste(in_vitro_file$datapath, ext, sep = "."), sheet = 1)
    in_vitro_function(in_vitro_df)
  })
  
  
}

 
# Run the application 
shinyApp(ui = ui, server = server)


# NOTES:
# Work-around for `readxl` functions, based on: 
# https://stackoverflow.com/questions/30624201/read-excel-in-a-shiny-app

