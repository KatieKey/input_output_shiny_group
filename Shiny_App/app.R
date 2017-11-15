#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("R Programming Group"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
                   
         fileInput(label = "Efficacy", inputId = "efficacy",
                    buttonLabel = "Efficacy Data"),
         fileInput(label = "Plasma", inputId = "plasma", 
                   buttonLabel = "Plasma Data"),
         fileInput(label = "Tissue Laser", inputId = "tissue_laser", 
                   buttonLabel = "Tissue Laser Data"),
         fileInput(label = "Tissue Std PK", inputId = "tissue_std_pk", 
                   buttonLabel = "Tissue Std PK Data"),
         fileInput(label = "In Vitro", inputId = "in_vitro", 
                   buttonLabel = "In Vitro Data")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        radioButtons("radio", label = h2("Pick a Data Set"),
                     choices = list("Efficacy" = efficacy_function(input = "efficacy"),
                                    "Plasma" = 2, "Tissue Laser" = 3,
                                    "Tissue Std PK" = 4, "In Vitro" = 5), selected = 1),
        
        hr(),
        fluidRow(column(3, verbatimTextOutput("value")))
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) 
  
  {
    output$value <- DT::renderDataTable({ input$radio })
  }



# Run the application 
shinyApp(ui = ui, server = server) 

