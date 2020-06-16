
library(shiny)
options(shiny.host = "0.0.0.0",
        shiny.port = 7777)

#---------------------------------------------------------
#                       START APP
#---------------------------------------------------------

# Note: All of the above is only done once.
# Then `runApp` runs `shinyServer` function once each time a user visits the app.
# Finally, the `render*` functions inside ui.R are run many times upon change.
runApp(appDir = "./")
