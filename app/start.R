#---------------------------------------------------------
#                       LIBRARIES
#---------------------------------------------------------
library(plotly)
library(data.table)

library(shiny)
options(shiny.host = "0.0.0.0",
        shiny.port = 7777)


#---------------------------------------------------------
#                       DATA
#---------------------------------------------------------
# setwd("/home/evanhenrich/Documents/FHCRC/ISAnalyteExplorer/app/") # for local testing

# GE Input
geData <- readRDS("data/new_boxplot_data.rds")
btms <- readRDS("data/btms.rds")
btmNames <- names(btms)
genes <- unique(geData$Gene$gbValue)

# Non-GE Input
nonGEData <- readRDS("data/nonGE_boxplot_data.rds")
analytes <- lapply(nonGEData, "[[", "analyte")
analytes <- lapply(analytes, unique)

#---------------------------------------------------------
#                       START APP
#---------------------------------------------------------

# Note: All of the above is only done once.
# Then `runApp` runs `shinyServer` function once each time a user visits the app.
# Finally, the `render*` functions inside ui.R are run many times upon change.
runApp(appDir = "./")
