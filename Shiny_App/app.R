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

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Mycobacteria Research Laboratories"),
  helpText("Upload Data and Explore Data Tables and Graphs for Each of the Respective
           Data Excel Files (i.e., Efficacy)"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(width = 4, (label = h3("Upload Data")),
                 
                 efficacy_input <- fileInput(label = "Efficacy", inputId = "efficacy",
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
    
    # Show a plot of the generated distribution
    
    mainPanel(radioButtons("radio", (label = h3("Pick a Data Set")),
                           helpText("Select a Data Set to View Data Table"),
                           choices = list("Efficacy" = efficacy_function(),
                                          "Plasma" = 2, "Tissue Laser" = 3,
                                          "Tissue Std PK" = 4, "In Vitro" = 5)),
              
              tableOutput("radio")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) 
  
{
  output$table <- DT::renderDataTable({ input$radio })
}



# Run the application 
shinyApp(ui = ui, server = server)

