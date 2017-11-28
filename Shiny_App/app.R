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
                  tabPanel("Raw Data Sets", 
                           tabsetPanel(type = "tabs",
                             tabPanel("Efficacy",
                               DT::dataTableOutput("raw_efficacy_table")
                               ),
                             tabPanel("Plasma"),
                             tabPanel("Tissue Laser"),
                             tabPanel("Tissue Std PK"),
                             tabPanel("In Vitro")
                           )
                           ),
                  tabPanel("Clean Data Set",
                           tabsetPanel(type = "tabs",
                                       tabPanel("Efficacy",
                                                DT::dataTableOutput("clean_efficacy_table")
                                                ),
                                       tabPanel("Next cleaned data"),
                                       tabPanel("The one after that")
                                       )),
                  tabPanel("Summary", tableOutput("plot")),
                  tabPanel("Independent", verbatimTextOutput("summary")),
                  tabPanel("Independent ~ Dependent", tableOutput("indepdep"))
      )
    )
  )
)


# Define server logic 
server <- function(input, output) 
  
{
  # Render data table with raw efficacy data
  output$raw_efficacy_table <- DT::renderDataTable({
    efficacy_file <- input$efficacy
    
    # Make sure you don't show an error by trying to run code before 
    # a file's been uploaded
    if(is.null(efficacy_file)){
      return(NULL)
    }
    
    # Work-around for `readxl` functions, based on 
    # https://stackoverflow.com/questions/30624201/read-excel-in-a-shiny-app
    ext <- tools::file_ext(efficacy_file$name)
    file.rename(efficacy_file$datapath, 
                paste(efficacy_file$datapath, ext, sep = "."))
    read_excel(paste(efficacy_file$datapath, ext, sep = "."), sheet = 1)
    
  })
  
  # Render data table with clean efficacy data
  output$clean_efficacy_table <- DT::renderDataTable({
    efficacy_file <- input$efficacy
    
    # Make sure you don't show an error by trying to run code before 
    # a file's been uploaded
    if(is.null(efficacy_file)){
      return(NULL)
    }
    
    # Work-around for `readxl` functions, based on 
    # https://stackoverflow.com/questions/30624201/read-excel-in-a-shiny-app
    ext <- tools::file_ext(efficacy_file$name)
    file.rename(efficacy_file$datapath, 
                paste(efficacy_file$datapath, ext, sep = "."))
    efficacy_df <- read_excel(paste(efficacy_file$datapath, ext, sep = "."), sheet = 1)
    efficacy_function(efficacy_df)
    })
}



# Run the application 
shinyApp(ui = ui, server = server)

