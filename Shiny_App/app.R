#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
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
                   buttonLabel = "Tissue Std PK Data")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         radioButtons(label = "Pick Data Set", 
                      choiceNames = 
                      "Efficacy", 
                      "Plasma",
                      "Tissue Laser",
                      "Tissue Std PK")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

