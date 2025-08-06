
sampleUI <- function(id) {
    ns = NS(id)

    tabItem(
        tabName = "Sample",
        actionLink(ns("infoModal"), span("Walkthrough", icon("question-circle"))),
        fluidRow( 
            box(
                title =  span("Samples in PCA",actionButton(ns("sample_pca"), icon("info"), size ="xs")),
                collapsible = TRUE,
                closable = TRUE,
                solidHeader = FALSE, 
                dropdownMenu = boxDropdown(
                    boxDropdownItem("add to report",id = ns("registerPca"),icon=icon("plus-square"))
                ),
                # elevation = 4,
                width = 12,
                fluidRow(
                    column(
                        6,
                        tagList(
                            h5("Sample Table", style="text-align:center;"),
                            DT::DTOutput(ns("sampleTable"))
                        )                    
                    ),
                    column(
                        6,
                        tagList(
                            h5("PCA Plot", style="text-align:center;"),
                            plotlyOutput(ns("pca3dPlot"))
                        )
                    )
                )
            )    
        ),  
        fluidRow(
            box(
            title =  span("Correlation matrix",actionButton(ns("sample_correlation_matrix"), icon("info"), size ="xs")),
            closable = TRUE,
            collapsible = TRUE,
            solidHeader = FALSE, 
            width = 6,
            # elevation = 12,
            plotlyOutput(ns("sampleCorPlot")),
            p("Click on a matrix cell to see sample vs sample correlation!", style="color:red;")
            ),
            box(
            title =  span("Sample vs Sample",actionButton(ns("sample_scatter"), icon("info"), size ="xs")),
            closable = TRUE,
            collapsible = TRUE,
            solidHeader = FALSE, 
            width = 6,
            # elevation = 12,
            plotlyOutput(ns("sampleScatter"))
            )
        )
    )
}


sampleServer <- function(id, parentSession, data){
    moduleServer(
        id,
        function(input,output,session){

            output$sampleTable <- DT::renderDataTable({
                req(data$samples)
                DT::datatable(data$samples[,c("name","group")],filter = "top",
                    options = list(
                        dom = 'tipr',
                        pageLength = 6
                    )
                )
            })

            output$sampleCorPlot <- renderPlotly(
                {
                    req(data$vsd)
                    drawSampleCorrelation(data)
                }
            )
            output$pca3dPlot <- renderPlotly(
                {
                    req(data$pca)
                    drawPCA(data)
                }
            )

            # sample scatter 
            output$sampleScatter<- renderPlotly({
                req(data$vsd, input$correlation_on_click)
                ns = NS(id)
                ## Plotly click event retrurn values:
                ##  "row"          "column"     "correlation"
                ##  "GSM4472566" "GSM4472578" "0.967"

                x = input$correlation_on_click[1]
                y = input$correlation_on_click[2]
                drawSampleScatter(data, x, y)
            })

            # register results 
            # observeEvent(input$registerPca,{
            #     data$rmd <- updateRmd(session=parentSession, boxId='pca', serverData=data )
            # })
            # Modal for PCA           
            observeEvent(input$sample_pca,{
                req(data$descriptions)
                getPopupWindow("PCA Plot", HTML(data$descriptions$sample_pca))
            })
            # Modal for correlation           
            observeEvent(input$sample_correlation_matrix,{
                req(data$descriptions)
                getPopupWindow("Sample Correlation", HTML(data$descriptions$sample_correlation_matrix))
            })
            # Modal for scatter           
            observeEvent(input$sample_scatter,{
                req(data$descriptions)
                getPopupWindow("Sample Scatter Plot", HTML(data$descriptions$sample_scatter))
            })
            observeEvent(input$infoModal,{
                req(data$descriptions)
                getRnaAppSampleInfoModal(data)
            })
        
        }
    )

}
