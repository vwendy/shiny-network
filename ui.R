#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Model Multivariate Time-Series as Networks"),
    
    # input file
    sidebarPanel(
        fileInput("i_file", "Upload your CSV file",
                  accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
        ),
        # actionButton(inputId="dataVizButton", "Data visualization"),
        # actionButton(inputId="modelButton", "Model time-series"),
        # actionButton(inputId="impulseButton", "Impulse response analysis"),
    ),
    
    verbatimTextOutput("datatab"),
    # verbatimTextOutput("summary"),
    plotOutput("dataviz"), 
    plotOutput("modelnetwork"),
    plotOutput("impulse")
    
    
    
    
))
