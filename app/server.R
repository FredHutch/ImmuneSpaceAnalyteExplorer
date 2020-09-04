library(plotly)
library(RColorBrewer)

#---------------------------------------------------------
#                       DATA
#---------------------------------------------------------
setwd("/home/evanhenrich/Documents/FHCRC/ISAnalyteExplorer/app/") # for local testing

# GE Input
allData.gene <- readRDS("data/geByTimepoint_gene.rds")
allData.btm <- readRDS("data/geByTimepoint_btm.rds")
btms <- readRDS("data/btms.rds")
btmNames <- names(btms)
genes <- unique(allData.gene$analyte)

getFigureList <- function(pd){
    selectedConditions <- unique(pd$mappedCondition)
    selectedAnalyte <- unique(pd$analyte)
    
    figList <- list()
    pal <- RColorBrewer::brewer.pal(n = 8, "Set3")
    pal <- sample(pal, length(selectedConditions))
    
    for(i in 1:length(selectedConditions)){
        condition <- as.character(selectedConditions[[i]])
        pd.subset <- pd[ pd$mappedCondition == condition, ]
        
        # Trendline should only be for points at which there is sufficient data
        minCohortsForTrend <- 0.4
        timepoints <- table(pd.subset$timepoint)
        timepoints <- timepoints[ timepoints > minCohortsForTrend * length(unique(pd.subset$cohort))]
        timepoints <- sort(names(timepoints))
        meanValues <- sapply(timepoints, function(pt){
            pd.pt <- pd.subset[ pd.subset$timepoint == pt]
            return(mean(pd.pt$mean_fold_change))
        })
        
        pd.trend <- data.frame(
            cohort = "Average",
            cell_type = "NA",
            study = "Trend",
            analyte = selectedAnalyte,
            mappedCondition = "Trend",
            timepoint = as.double(timepoints),
            mean_fold_change = meanValues,
            sd_fold_change = 0,
            stringsAsFactors = FALSE
        )
        
        pd.subset <- merge(pd.subset, pd.trend, all=TRUE)
        pd.subset <- pd.subset[ order(pd.subset$study), ] # must draw trend at end to visible
        cohorts <- unique(pd.subset$cohort)
        
        p <- plot_ly()
        colorMap <- c(Trend = "#5b5c5b")
        colorMap[[condition]] <- pal[[i]]
        
        for(selectedCohort in cohorts){
            p <- add_trace(p, data = pd.subset[which(pd.subset$cohort == selectedCohort),], 
                           x = ~timepoint, 
                           y = ~mean_fold_change, 
                           color = ~mappedCondition,
                           colors = colorMap,
                           text = selectedCohort,
                           customdata = ~study,
                           hovertemplate = paste('<b>Cohort</b>: %{text}',
                                                 '<br><b>Study</b>: %{customdata}',
                                                 '<br><b>Timepoint</b>: %{x}',
                                                 '<br><b>log2-FC</b>: %{y:.2f}',
                                                 '<extra></extra>'),
                           type = 'scatter', 
                           mode = 'lines+markers')
        }
        
        figList[[condition]] <- p %>% layout(showlegend = FALSE,
                                             yaxis = list(title = condition))
    }
    
    fig <- subplot(figList, 
                   shareY = TRUE, 
                   nrows = length(selectedConditions), 
                   titleX = FALSE)
    
    return(fig)
}

shinyServer(function(input, output, session) {

    #---------------------------------------------------------
    #                       FOR TESTING
    #---------------------------------------------------------

    # input <- list(isGene = "Gene",
    #               geneOrBtmOptions = "A2M",
    #               conditionStudied = "Influenza")


    #---------------------------------------------------------
    #                       MAIN
    #---------------------------------------------------------
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
        if(input$isGene == 'Gene'){
            data <- allData.gene
        }else{
            data <- allData.btm
        }
        plotData$data <- data[ data$analyte == input$geneOrBtmOptions &
                               data$mappedCondition %in% input$conditionStudied, ]
    })


    #---------------------------------------------------------
    #                       OUTPUTS
    #---------------------------------------------------------

    output$boxPlot <- plotly::renderPlotly({
        if(is.null(plotData$data)){
            return()
        }
        
        fig <- getFigureList(plotData$data)

    })
})
