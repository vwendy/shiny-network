#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

datasets <- data(package = "ggplot2")$results[c(2,8), "Item"]

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Model the dynamics in multivariate time-series as networks"),
    # 
    # # Sidebar with a slider input for number of bins
    # sidebarLayout(
    #     sidebarPanel(
    #         sliderInput("bins",
    #                     "Number of bins:",
    #                     min = 1,
    #                     max = 50,
    #                     value = 30)
    #     ),
    # 
    #     # Show a plot of the generated distribution
    #     mainPanel(
    #         plotOutput("distPlot")
    #     )
    # )
    
    selectInput("dataset", "Dataset", choices = datasets),
    verbatimTextOutput("summary"),
    plotOutput("plot")
    
    
    
    
))
