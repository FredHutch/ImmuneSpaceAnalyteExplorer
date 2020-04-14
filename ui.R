shinyUI(fluidPage(

    pageWithSidebar(

        # Application title
        headerPanel("ImmuneSpace Expression Explorer v2"),

        # Sidebar with four inputs
        sidebarPanel(
            h3("Overview"),
            p("This analysis allows the user to visualize log fold change of gene expression over time by study accession at either the gene symbol level or blood transcription module level (mean of all genes assigned to module)"),
            radioButtons(inputId = "isGene",
                         label = "Select whether to use Gene or BTM values",
                         choices = list("Gene" = "Gene",
                                        "Blood Transcription Module" = "Btm"),
                         selected = "gene"
            ),
            selectizeInput(inputId = "geneOrBtmOptions",
                           label = NULL,
                           choices = NULL,
                           options = list(
                               maxItems = 1,
                               placeholder = "Select Gene or BTM"
                           )),
            actionButton("submit","Submit"),
            h3(),
            p("Shiny app development by Evan Henrich @ Fred Hutch.")
        ),

        # Show grouped boxplots
        mainPanel(
            plotly::plotlyOutput("boxPlot", height = "700px")
        )
    )

))
