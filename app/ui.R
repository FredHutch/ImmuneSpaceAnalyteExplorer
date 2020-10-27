shinyUI(fluidPage(

    pageWithSidebar(

        # Application title
        headerPanel("ImmuneSpace Analyte Explorer"),

        # Sidebar with four inputs
        sidebarPanel(
            h3("Overview"),
            p("This analysis allows the user to visualize log fold change of a gene or gene set over time by cohort. In the case of genes and blood transcription modules, the fold change is the delta of log2-transformed expression values, while gene signatures uses fold change of the geometric mean. A trendline is drawn for points with sufficient data.
              For all cohorts other than 'Healthy', the exposure process was 'vaccination'."),
            selectizeInput(inputId = "conditionStudied",
                           label = "Select Conditions Studied for Plotting:",
                           choices = list("Influenza" = "Influenza",
                                          "Meningitis" = "Meningitis",
                                          "Herpes Zoster" = "Herpes_Zoster",
                                          "Yellow Fever" = "Yellow_Fever",
                                          "Malaria" = "Malaria",
                                          "Tuberculosis" = "Tuberculosis",
                                          "Ebola" = "Ebola",
                                          "HIV" = "HIV",
                                          "Palmoplantar Pustulosis" = "Palmoplantar_Pustulosis",
                                          "Dermatomyositis" = "Dermatomyositis",
                                          "Hepatitis" = "Hepatitis",
                                          "Varicella Zoster" = "Varicella_Zoster",
                                          "Smallpox" = "Smallpox",
                                          "Healthy" = "Healthy",
                                          "Pneumonia" = "Pneumonia"),
                           options = list(
                               maxItems = 5,
                               placeholder = "Select Conditions Studied"
                           )
            ),
            radioButtons(inputId = "analyteType",
                         label = "Select type of feature:",
                         choices = list("Gene" = "Gene",
                                        "Blood Transcription Module" = "Btm",
                                        "Gene Signature" = "GeneSignature"),
                         selected = "Gene"
            ),
            p(),
            conditionalPanel(
                h4("Gene Signature Filters:"),
                condition = "input.analyteType == 'GeneSignature'",
                selectizeInput(inputId = "gs.disease_studied",
                               label = "Disease Studied:",
                               choices = NULL,
                               options = list(
                                   maxItems = 1,
                                   placeholder = "Select Disease Studied"
                               )
                ),
                selectizeInput(inputId = "gs.timepoint_concat",
                               label = "Timepoint",
                               choices = NULL,
                               options = list(
                                   maxItems = 1,
                                   placeholder = "Select Timepoints"
                               )
                ),
                selectizeInput(inputId = "gs.updated_response_behavior",
                               label = "Response Behavior",
                               choices = NULL,
                               options = list(
                                   maxItems = 1,
                                   placeholder = "Select Response Behavior"
                               )
                ),
                actionButton("applyFilters","Apply Filters"),
                actionButton("resetFilters","Reset Filters"),
                p("Note: Apply filters one at a time to update dropdowns")
            ),
            strong("Selected Gene or Gene Set"),
            selectizeInput(inputId = "analyteSelection",
                           label = NULL,
                           choices = NULL,
                           options = list(
                               maxItems = 1,
                               placeholder = "Select Gene or Gene Set"
                           )
            ),
            actionButton("submit","Submit"),
            h3(),
            p("Shiny app development by Evan Henrich @ Fred Hutch.")
        ),

        # OUTPUT
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Plots",  plotly::plotlyOutput("linePlots", height = "700px")),
                        tabPanel("Meta-Data", DT::dataTableOutput('metaData'))
            )
        )
    )

))
