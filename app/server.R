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
geneSignatures <- readRDS("data/geneSignatures.rds")


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
    #               gs.diseaseStudied = "Influenza",
    #               gs.timepoint = "1-Days",
    #               gs.responseBehavior = "up-regulated",
    #               analyteSelection = "Early patterns of gene expression correlate with the humoral immune response to influenza vaccination in humans",
    #               conditionStudied = "Influenza")


    #---------------------------------------------------------
    #                       MAIN
    #---------------------------------------------------------
    observeEvent(input$analyteType, {
        
        if(input$analyteType == "Gene"){
            options <- unique(allData.gene$analyte)
        }else if(input$analyteType == "Btm"){
            options <- btmMetaData$`Module Name`
        }else if(input$analyteType == "GeneSignature"){
            options <- geneSignatures$pubmed_titles
        }
        
        updateSelectizeInput(session,
                             'analyteSelection',
                             choices = options,
                             server = TRUE)
        
        if(input$analyteType == "GeneSignature"){
            options.gs.diseaseStudied <- unique(geneSignatures$disease_studied)
            options.gs.timepoint <- unique(geneSignatures$timepoint_concat)
            options.gs.responseBehavior <- unique(geneSignatures$updated_response_behavior)
            
            updateSelectizeInput(session,
                                 'gs.diseaseStudied',
                                 choices = options.gs.diseaseStudied,
                                 server = TRUE)
            
            updateSelectizeInput(session,
                                 'gs.timepoint',
                                 choices = options.gs.timepoint,
                                 server = TRUE)
            
            updateSelectizeInput(session,
                                 'gs.responseBehavior',
                                 choices = options.gs.responseBehavior,
                                 server = TRUE)
        }
    })
    
    
    # Gene-Signature
    geneSignatureListener <- reactive({
        list(input$gs.diseaseStudied,
             input$gs.timepoint,
             input$gs.responseBehavior)
    })

    observeEvent(geneSignatureListener(), {
        if(input$analyteType == "GeneSignature"){
            valid.disease <- geneSignatures$disease_studied %in% input$gs.diseaseStudied
            valid.timepoint <- geneSignatures$timepoint_concat %in% input$gs.timepoint
            valid.response <- geneSignatures$updated_response_behavior %in% input$gs.responseBehavior
            
            validNames <- geneSignatures$pubmed_titles[ valid.disease &
                                              valid.timepoint &
                                              valid.response ]
            
            if(length(validNames) == 0){
                validNames <- "Update filters for more options"
            }
            
            updateSelectizeInput(session,
                                 'analyteSelection',
                                 choices = validNames,
                                 server = TRUE)
        }
    })
    
    # Generate plot
    plotData <- reactiveValues(data = NULL)
    metaData <- reactiveValues(data = NULL)

    observeEvent(input$submit, {
        
        pubmedBaseUrl <- "https://pubmed.ncbi.nlm.nih.gov/"

        if(input$analyteType == 'Gene'){
            data <- allData.gene
            
            selectedGene <- paste0("(;|)", input$analyteSelection[1] ,"(;|)")
            info <- geneSignatures[ grepl(selectedGene, geneSignatures$updated_symbols), ]
            info$link <- paste0('<a href="', 
                                paste0(pubmedBaseUrl, info$publication_reference), 
                                '">', 
                                info$publication_reference,
                                '</a>')
            
            keepCols <- c("disease_studied", 
                          "timepoint_concat", 
                          "updated_response_behavior", 
                          "pubmed_titles",
                          "link")
            info <- info[, colnames(info) %in% keepCols]
            
            colnames(info) <- c("Associated Gene Signature Disease", 
                                "Response Behavior", 
                                "Associated Gene Signature Article", 
                                "Timepoint",
                                "Article Link")
            
            metaDataOptions <- list(pageLength = 5)
            
            selection <- input$analyteSelection
            
        }else if(input$analyteType == 'Btm'){
            data <- allData.btm
            
            info <- btmMetaData[ btmMetaData$`Module Name` == input$analyteSelection[1] ]
            info <- info[, -1 ] # rm module name
            metaDataOptions <- list(pageLength = 1,
                                    searching  = FALSE,
                                    paging     = FALSE,
                                    info       = FALSE,
                                    ordering   = FALSE)
            
            selection <- input$analyteSelection
            
            
        }else if(input$analyteType == 'GeneSignature'){
            data <- allData.geneSignatures
            selected <- which(geneSignatures$disease_studied == input$gs.diseaseStudied &
                                  geneSignatures$updated_response_behavior == input$gs.responseBehavior &
                                  geneSignatures$disease_studied == input$gs.diseaseStudied &
                                  geneSignatures$pubmed_titles == input$analyteSelection)
            
            info <- geneSignatures[ selected, ]
            info$link <- paste0('<a href="', 
                                paste0(pubmedBaseUrl, info$publication_reference), 
                                '">', 
                                info$publication_reference,
                                '</a>')
            info$updated_symbols <- gsub(";", ", ", info$updated_symbols)
            keepCols <- c("updated_symbols", "pubmed_titles", "link")
            info <- info[, colnames(info) %in% keepCols]
            colnames(info) <- c("Genes", "Article Title", "Article Link")
            metaDataOptions <- list(pageLength = 1,
                                    searching  = FALSE,
                                    paging     = FALSE,
                                    info       = FALSE,
                                    ordering   = FALSE)
            
            
            
            selection <- geneSignatures$uid[ selected ]
            
        }
        plotData$data <- data[ data$analyte == selection &
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
