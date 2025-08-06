geneUI <- function(id) {

    ns = NS(id)

    tabItem(
        tabName = "Gene",
        actionLink(ns("infoModal"), span("Walkthrough", icon("question-circle"))),
        fluidRow(
            box(
                width=12,
                title =  span("Gene Expression",actionButton(ns("gene_expression"), icon("info"), size ="xs")),
                selectizeInput(
                    inputId = ns('geneSearch'),
                    label = 'Search',
                    choices = NULL,
                    selected = NULL,
                    multiple = FALSE # allow for multiple inputs
                ),
                echarts4rOutput(ns("geneBoxplot"))
            )

        )
    )

}

geneServer <- function(id,data) {
    moduleServer(
        id,
        function(input,output,session) {

            # Section to update values in gene search
            observeEvent(data$vsd,{
                # req(data$vsd)
                updateSelectizeInput(session, 'geneSearch',  choices = rownames(assay(data$vsd)), selected=rownames(assay(data$vsd))[1], server = TRUE)
            })
            # geneHeatmapSearch
            output$geneBoxplot <- renderEcharts4r({
                req(data$vsd, data$pca, input$geneSearch)
                drawGeneExpression(data, geneName=input$geneSearch)
            })

            # Modal for Expression           
            observeEvent(input$gene_expression,{
                req(data$descriptions)
                getPopupWindow("Expression", HTML(data$descriptions$gene_expression))
            })

            geneDetails = reactiveValues()
            # User help section
            observeEvent(input$infoModal,{
                req(data$descriptions)
                getRnaAppGeneInfoModal(data)
            })
        }
    )
}