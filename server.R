#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    dataset <- reactive({
        get(input$dataset, "package:ggplot2")
    })
    output$summmry <- renderPrint({
        summary(dataset())
    })
    output$plot <- renderPlot({
        plot(dataset())
    }, res = 96)
    
    
    


    
})
