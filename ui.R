shinyUI(fluidPage(

    pageWithSidebar(

        # Application title
        headerPanel("ImmuneSpace Analyte Explorer"),

        # Sidebar with four inputs
        sidebarPanel(
            h3("Overview"),
            p("This analysis allows the user to visualize log fold change of gene or analyte over time by study accession. For gene expression, the user can look at either the gene symbol level or blood transcription module level (mean of all genes assigned to module)"),
            selectInput(inputId = "selectedAssay",
                       label = NULL,
                       choices = list(
                           "Gene Expression" = "gene_expression",
                           "MBAA" = "mbaa",
                           "ELISA" = "elisa",
                           "ELISPOT" = "elispot"
                       )),
            conditionalPanel(
                condition = "input.selectedAssay == 'gene_expression'",
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
                               )
                )
            ),
            conditionalPanel(
                condition = "input.selectedAssay != 'gene_expression'",
                selectizeInput(inputId = "analyteOptions",
                               label = NULL,
                               choices = NULL,
                               options = list(
                                   maxItems = 1,
                                   placeholder = "Select Analyte"
                               )
                )
            ),
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
