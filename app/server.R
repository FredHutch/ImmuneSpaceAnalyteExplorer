library(plotly)
library(DT)
library(RColorBrewer)

#---------------------------------------------------------
#                       DATA
#---------------------------------------------------------
# setwd("/home/evanhenrich/Documents/FHCRC/ISAnalyteExplorer/app/") # for local testing

# data summarized by timepoint
allData.gene <- readRDS("data/geByTimepoint_gene.rds")
allData.btm <- readRDS("data/geByTimepoint_btm.rds")
allData.geneSignatures <- readRDS("data/geByTimepoint_geneSig.rds")

# Meta-Data
btmMetaData <- readRDS("data/btm_metadata.rds")
gs <- readRDS("data/geneSignatures.rds")


# Helpers
getFigureList <- function(pd){
    selectedConditions <- unique(pd$mappedCondition)
    selectedAnalyte <- unique(pd$analyte)
    
    figList <- list()
    
    # color-palette
    pal <- RColorBrewer::brewer.pal(n = 7, "Dark2")
    pal <- sample(pal, length(selectedConditions))
    
    # Axes
    dayRange <- range(pd$timepoint)
    fcRange <- range(pd$mean_fold_change)
    
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
                                             yaxis = list(title = condition,
                                                          range = fcRange),
                                             xaxis = list(range = dayRange))
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

    # input <- list(analyteType = "GeneSignature",
    #               gs.disease_studied = "Herpes",
    #               gs.timepoint_concat = "ALL",
    #               gs.updated_response_behavior = "ALL",
    #               analyteSelection = "Early patterns of gene expression correlate with the humoral immune response to influenza vaccination in humans",
    #               conditionStudied = "Influenza")


    #---------------------------------------------------------
    #                       MAIN
    #---------------------------------------------------------
    # STATE
    filters.stored <- list(disease_studied = "ALL",
                            timepoint_concat = "ALL",
                            updated_response_behavior = "ALL")
    
    # INIT
    for(nm in names(filters.stored)){
        uiElement <- paste0("gs.", nm)
        updateSelectizeInput(session,
                             uiElement,
                             choices = c("ALL", unique(gs[[nm]])),
                             server = TRUE)
    }
    
    
    # On Analyte Type Selection
    observeEvent(input$analyteType, {
        
        if(input$analyteType == "Gene"){
            options <- unique(allData.gene$analyte)
        }else if(input$analyteType == "Btm"){
            options <- btmMetaData$`Module Name`
        }else if(input$analyteType == "GeneSignature"){
            options <- gs$uid
        }
        
        updateSelectizeInput(session,
                             'analyteSelection',
                             choices = options,
                             server = TRUE)
        
    })
    
    # GeneSignatures Conditional Filters
    observeEvent(input$applyFilters, {
        filters.current <- list(disease_studied = input$gs.disease_studied,
                                timepoint_concat = input$gs.timepoint_concat,
                                updated_response_behavior = input$gs.updated_response_behavior)
        
        filters.unchanged <- filters.current[ unlist(filters.current) == unlist(filters.stored) ]

        gs.current <- gs
        
        for(fn in names(filters.current)){
            if(filters.current[ fn ] != "ALL"){
                gs.current <- gs.current[ gs.current[[fn]] == filters.current[[fn]], ]
            }
        }
        
        # Update all unchanged filters
        for(fn in names(filters.unchanged)){
            uiElement <- paste0("gs.", fn)
            if(nrow(gs.current) > 0){
                choices <- c("ALL", unique(gs.current[[fn]]))
            }else{
                choices <- "No Choices - Reset Filters!"
            }
            updateSelectizeInput(session,
                                 uiElement,
                                 choices = choices,
                                 server = TRUE)
        }
        
        # Update the analyteSelection
        if(nrow(gs.current) > 0){
            choices <- unique(gs.current$uid)
        }else{
            choices <- "No Choices - Reset Filters!"
        }
        updateSelectizeInput(session,
                             'analyteSelection',
                             choices = choices,
                             server = TRUE)
    })
    
    observeEvent(input$resetFilters, {
        filters.stored <- list(disease_studied = "ALL",
                               timepoint_concat = "ALL",
                               updated_response_behavior = "ALL")
        
        for(nm in names(filters.stored)){
            uiElement <- paste0("gs.", nm)
            updateSelectizeInput(session,
                                 uiElement,
                                 choices = c("ALL", unique(gs[[nm]])),
                                 server = TRUE)
        }
        
        updateSelectizeInput(session,
                             'analyteSelection',
                             choices = unique(gs$uid),
                             server = TRUE)
    })
    
    # Generate plot
    plotData <- reactiveValues(data = NULL)
    metaData <- reactiveValues(data = NULL)

    observeEvent(input$submit, {
        
        pubmedBaseUrl <- "https://pubmed.ncbi.nlm.nih.gov/"

        if(input$analyteType == 'Gene'){
            data <- allData.gene
            
            selectedGene <- paste0("(;|)", input$analyteSelection[1] ,"(;|)")
            info <- gs[ grepl(selectedGene, gs$updated_symbols), ]
            info$link <- paste0('<a href="', 
                                paste0(pubmedBaseUrl, info$publication_reference), 
                                '">', 
                                info$publication_reference,
                                '</a>')
            
            keepCols <- c("disease_studied", 
                          "timepoint_concat", 
                          "updated_response_behavior", 
                          "comparison",
                          "cohort",
                          "subgroup",
                          "pubmed_titles",
                          "link")
            info <- info[, colnames(info) %in% keepCols]
            info <- info[ order(match(colnames(info), keepCols))]
            
            colnames(info) <- c("Associated Gene Signature Disease", 
                                "Timepoint",
                                "Response Behavior", 
                                "Comparison",
                                "Cohort",
                                "Sub-group",
                                "Associated Gene Signature Article", 
                                "Article Link")
            
            metaDataOptions <- list(pageLength = 5)
            
        }else if(input$analyteType == 'Btm'){
            data <- allData.btm
            
            info <- btmMetaData[ btmMetaData$`Module Name` == input$analyteSelection[1] ]
            info <- info[, -1 ] # rm module name
            metaDataOptions <- list(pageLength = 1,
                                    searching  = FALSE,
                                    paging     = FALSE,
                                    info       = FALSE,
                                    ordering   = FALSE)
            
            
        }else if(input$analyteType == 'GeneSignature'){
            data <- allData.geneSignatures
            
            info <- gs[ gs$uid == input$analyteSelection[1], ]
            info$link <- paste0('<a href="', 
                                paste0(pubmedBaseUrl, info$publication_reference), 
                                '">', 
                                info$publication_reference,
                                '</a>')
            info$updated_symbols <- gsub(";", ", ", info$updated_symbols)
            keepCols <- c("updated_symbols", 
                          "comparison",
                          "cohort",
                          "subgroup",
                          "pubmed_titles", 
                          "link")
            info <- info[, colnames(info) %in% keepCols]
            info <- info[ order(match(colnames(info), keepCols))]
            colnames(info) <- c("Genes", 
                                "Comparison",
                                "Cohort",
                                "Sub-group",
                                "Article Title", 
                                "Article Link")
            metaDataOptions <- list(pageLength = 1,
                                    searching  = FALSE,
                                    paging     = FALSE,
                                    info       = FALSE,
                                    ordering   = FALSE)
            
        }
        plotData$data <- data[ data$analyte == input$analyteSelection &
                               data$mappedCondition %in% input$conditionStudied, ]
        
        metaData$data <- datatable(info, 
                                   options  = metaDataOptions, 
                                   rownames = FALSE,
                                   escape   = FALSE)
    })


    #---------------------------------------------------------
    #                       OUTPUTS
    #---------------------------------------------------------

    output$linePlots <- plotly::renderPlotly({
        if(is.null(plotData$data)){
            return()
        }
        
        fig <- getFigureList(plotData$data)

    })
    
    output$metaData <- renderDataTable(metaData$data)
})
