library(plotly)
library(data.table)

#---------------------------------------------------------
#                       DATA
#---------------------------------------------------------
# setwd("/home/evanhenrich/Documents/FHCRC/ISAnalyteExplorer/app/") # for local testing

# GE Input
geData <- readRDS("data/GE_boxplot_data.rds")
btms <- readRDS("data/btms.rds")
btmNames <- names(btms)
genes <- unique(geData$Gene$gbValue)

# Non-GE Input
nonGEData <- readRDS("data/nonGE_boxplot_data.rds")
analytes <- lapply(nonGEData, "[[", "analyte")
analytes <- lapply(analytes, unique)

shinyServer(function(input, output, session) {

    #---------------------------------------------------------
    #                       FOR TESTING
    #---------------------------------------------------------

    # input <- list(isGene = "Btm",
    #               geneOrBtmOptions = btmNames[[1]],
    #               groupingVar = "newCondition")


    #---------------------------------------------------------
    #                       MAIN
    #---------------------------------------------------------
    observeEvent(input$selectedAssay, {
        if(input$selectedAssay != "gene_expression"){
            options <- analytes[[input$selectedAssay]]
            updateSelectizeInput(session,
                                 'analyteOptions',
                                 choices = options,
                                 server = TRUE)
        }
    })
    
    observeEvent(input$isGene, {
        if(input$isGene == "Gene"){
            options <- genes
        }else{
            options <- btmNames
        }
        updateSelectizeInput(session,
                             'geneOrBtmOptions',
                             choices = options,
                             server = TRUE)
    })
    
    # Generate plot
    plotData <- reactiveValues(data = NULL)

    observeEvent(input$submit, {
        if(input$selectedAssay == 'gene_expression'){
            data <- geData
            tmp <- data[[input$isGene]]
            tmp <- tmp[ tmp$gbValue == input$geneOrBtmOptions ]
        }else{
            data <- nonGEData[[input$selectedAssay]]
            tmp <- data[ data$analyte == input$analyteOptions ]
        }
        setorder(tmp, -Condition, Study)
        plotData$data <- tmp
    })


    #---------------------------------------------------------
    #                       OUTPUTS
    #---------------------------------------------------------

    output$boxPlot <- plotly::renderPlotly({
        if(is.null(plotData$data)){
            return()
        }
        yform <- list(categoryorder = "array",
                      categoryarray = unique(plotData$data$Study),
                      title = list(text = ""))

        p <- plot_ly(plotData$data,
                     x = ~value,
                     y = ~Study,
                     color = ~Condition, # inside of box
                     colors = 'Dark2',
                     type = "box",
                     #boxpoints = FALSE, # allows marker.outliercolor, 'all' doesn't
                     #jitter = 0.4,
                     marker = list(color = 'rgba(0, 0, 0, 0.5)'),
                     line = list(color = 'rgba(0, 0, 0, 1)',
                                 width = 1)) %>%
          layout(boxgap = 0.1) %>%
          layout(yaxis = yform) %>%
          # layout(yaxis = list(title = list(text = ""))) %>%
          layout(xaxis = list(title = list(text = "log-FoldChange from Day 0"))) %>%
          layout(legend = list(traceorder = "normal"))

    })
})
