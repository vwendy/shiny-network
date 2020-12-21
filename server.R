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
library(reshape2)
library(pompom)

set.seed(1234)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    mySeries_raw <- reactive({
        inFile <- input$i_file
        if (is.null(inFile))
            return(NULL)
        mySeries <- read.csv(inFile$datapath, 
                             header = T,
                             strip.white=T,
                             stringsAsFactors=F,
                             fill=T)
    })
    
    usem <- reactive({
        # how ot pass parameters into reactive, e.g., var.number
        # and out of reactive, e.g., mdl versus model.fit
        
        mySeries <- mySeries_raw()
        # mySeries <- read.csv("c:/users/Xiao Yang/Downloads/time-series.csv")
        var.number <- ncol(mySeries) # number of variables
        lag.order <- 1 # lag order of the model
        model.fit <- uSEM(var.number, 
                          mySeries,
                          lag.order, 
                          verbose = FALSE, 
                          trim = TRUE)

        
    })
    
    output$datatab <- renderPrint({
        mySeries <- mySeries_raw()
        names(mySeries) <- paste("v", 1:ncol(mySeries), sep ="")
        print(head(mySeries))
    })
    
    output$summmry <- renderPrint({
        mySeries <- mySeries_raw()
        print(summary(mySeries))
    })
    
    output$dataviz <- renderPlot({
        mySeries <- mySeries_raw()

        # mySeries <- read.csv("c:/users/Xiao Yang/Downloads/shiny example data.csv")

        col_no <- ncol(mySeries)
        names(mySeries) <- paste("v",1:col_no)
        mySeries$index <- 1:nrow(mySeries)

        mySeries_long <- reshape(mySeries,
                                 timevar = "index",
                                 varying = list(1:col_no),
                                 direction = "long")
        names(mySeries_long) <- c("variable","value","index")
        ggplot(mySeries_long, aes(x = index, y = value, color = factor(variable)))+
            geom_line()+
            facet_wrap(~variable, ncol = 1)+
            theme_classic()

    }, res = 96)
    
    output$modelnetwork <- renderPlot({
        mySeries <- mySeries_raw()
        # # mySeries <- read.csv("c:/users/Xiao Yang/Downloads/time-series.csv")
        var.number <- ncol(mySeries) # number of variables
        lag.order <- 1 # lag order of the model
        
        # model.fit <- uSEM(var.number, 
        #                   mySeries,
        #                   lag.order, 
        #                   verbose = FALSE, 
        #                   trim = TRUE)
        # 
        
        model.fit <- usem()
        mdl <- model_summary(model.fit,
                             var.number,
                             lag.order)

        
        
        if(length(mdl)==1 ){
            
            # show something to let people know it didn't work 
        }
        else{
            rule <- ifelse(mdl$cfi >= 0.95,1,0) + 
                ifelse(mdl$tli >= 0.95,1,0) + 
                ifelse(mdl$rmsea <= 0.08 ,1,0) + 
                ifelse(mdl$srmr <= 0.08,1,0)
        
            if (rule >=3){
                beta.matrix <- parse_beta(var.number = var.number, 
                                          model.fit = model.fit, 
                                          lag.order = lag.order, 
                                          matrix = TRUE) # parse temporal relations in matrix format
                
                plot_network_graph(beta.matrix$est, 
                                   var.number)
            }
            else{
                # show something to let people know it didn't work 
            }
        }
        
    }, res = 96)
    
    
    
    output$impulse <- renderPlot({
        mySeries <- mySeries_raw()
        # mySeries <- read.csv("c:/users/Xiao Yang/Downloads/time-series.csv")
        var.number <- ncol(mySeries) # number of variables
        lag.order <- 1 # lag order of the model
        threshold <- 0.01
        replication <- 200
        steps <- 100
        
        # model.fit <- uSEM(var.number, 
        #                   mySeries,
        #                   lag.order, 
        #                   verbose = FALSE, 
        #                   trim = TRUE)
        # 
 
 
        model.fit <- usem()
        mdl <- model_summary(model.fit,
                             var.number,
                             lag.order)
        
        
        if(length(mdl)==1 ){
            
            # show something to let people know it didn't work 
        }
        else{
            rule <- ifelse(mdl$cfi >= 0.95,1,0) + 
                ifelse(mdl$tli >= 0.95,1,0) + 
                ifelse(mdl$rmsea <= 0.08 ,1,0) + 
                ifelse(mdl$srmr <= 0.08,1,0)
            
            if (rule >=3){
                beta.matrix <- parse_beta(var.number = var.number, 
                                          model.fit = model.fit, 
                                          lag.order = lag.order, 
                                          matrix = TRUE) # parse temporal relations in matrix format
                
                plot_network_graph(beta.matrix$est, 
                                   var.number)
                
                # point.estimate.iRAM <- iRAM(model.fit, 
                #                             beta = NULL, 
                #                             var.number = var.number, 
                #                             lag.order = lag.order, 
                #                             threshold = threshold,
                #                             boot = FALSE, 
                #                             replication = replication,
                #                             steps= steps)
                # 
                # # point.estimate.iRAM$recovery.time 
                # 
                # plot_time_profile(point.estimate.iRAM$time.series.data, 
                #                   var.number = 3,
                #                   threshold = threshold, 
                #                   xupper = 50)
                
                bootstrap.iRAM <- iRAM(model.fit, 
                                       beta = NULL, 
                                       var.number = var.number, 
                                       lag.order = lag.order,
                                       threshold = threshold,
                                       boot = TRUE, 
                                       replication = replication,
                                       steps= steps)
                
                plot_time_profile(bootstrap.iRAM$time.profile.data, 
                                                         var.number = var.number,
                                                         threshold = threshold, 
                                                         xupper = 25)
                
                
            }
            else{
                # show something to let people know it didn't work 
            }
        }
        
    }, res = 96)
    
    # mySeries_dataviz <- eventReactive(input$dataVizButton, {
    #     #dependency on 'data viz' button being pressed
    #     #input$dataVizButton
    #     
    #     if (nrow(mySeries_raw())==0) 
    #         return()
    #     
    #     #use existing reactive structures
    #     mySeries <- mySeries_raw()
    #    
    #     col_no <- ncol(mySeries)
    #     names(mySeries) <- paste("v",1:col_no)
    #     mySeries$index <- 1:nrow(mySeries)
    # 
    #     mySeries_long <- reshape(mySeries,
    #                              timevar = "index",
    #                              varying = list(1:col_no),
    #                              direction = "long")
    #     names(mySeries_long) <- c("variable","value","index")
    #     ggplot(mySeries_long, aes(x = index, y = value, color = factor(variable)))+
    #         geom_line()+
    #         facet_wrap(~variable, ncol = 1)
    #     print("hello")
    # })
    
    


    
})
