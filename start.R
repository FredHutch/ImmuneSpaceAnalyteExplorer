#---------------------------------------------------------
#                       LIBRARIES
#---------------------------------------------------------
library(UpdateAnno)
library(plotly)
library(data.table)

library(shiny)
options(shiny.host = "0.0.0.0",
        shiny.port = 7777)


#---------------------------------------------------------
#                       DATA
#---------------------------------------------------------

# boxplot format: data[gene/btm]
data <- readRDS("data/full_boxplot_data.rds")

# Input Options
btms <- UpdateAnno::emory_blood_transcript_modules
btmNames <- names(btms)
genes <- unique(data$Gene$gbValue)

#---------------------------------------------------------
#                       START APP
#---------------------------------------------------------

# Note: All of the above is only done once.
# Then `runApp` runs `shinyServer` function once each time a user visits the app.
# Finally, the `render*` functions inside ui.R are run many times upon change.
runApp(appDir = "./")
