shinyUI(fluidPage(

    pageWithSidebar(

        # Application title
        headerPanel("ImmuneSpace Analyte Explorer"),

        # Sidebar with four inputs
        sidebarPanel(
            h3("Overview"),
            p("This analysis allows the user to visualize log-2 fold change of gene or btm over time by cohort. A trendline is drawn for points with sufficient data."),
            selectizeInput(inputId = "conditionStudied",
                           label = NULL,
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
                                          "Healthy" = "Healthy"),
                           options = list(
                               maxItems = 5,
                               placeholder = "Select Conditions Studied"
                           )
            ),
            radioButtons(inputId = "isGene",
                         label = "Select whether to use Gene or BTM values",
                         choices = list("Gene" = "Gene",
                                        "Blood Transcription Module" = "Btm"),
                         selected = "Gene"
            ),
            selectizeInput(inputId = "geneOrBtmOptions",
                           label = NULL,
                           choices = NULL,
                           options = list(
                               maxItems = 1,
                               placeholder = "Select Gene or BTM"
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
