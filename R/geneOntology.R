goUI <- function(id) {
    ns <- NS(id)

    tabItem(
        tabName = "GO",
        actionLink(ns("infoModal"), span("Walkthrough", icon("question-circle"))),
        tabsetPanel(
            id = ns("GOTabsetpanel"),
            selected = "Settings",
            type = "tabs",
            tabPanel(
                "Settings",
                br(),
                fluidRow(
                    box(
                        width = 4,
                        title = span("Analysis Settings", actionButton(ns("go_settings_info"), icon("info"), size = "xs")),
                        solidHeader = FALSE,
                        selectInput(
                            ns("goComparison"),
                            "Comparison",
                            choices = NULL,
                            selected = NULL
                        ),
                        selectInput(
                            ns("goOnt"),
                            "Ontology",
                            choices = c(
                                "Biological Process" = "BP",
                                "Molecular Function" = "MF",
                                "Cellular Component" = "CC",
                                "All" = "ALL"
                            ),
                            selected = "BP"
                        ),
                        selectInput(
                            ns("goAnalysisType"),
                            "Analysis Type",
                            choices = c(
                                "Over-Representation Analysis (ORA)" = "ORA",
                                "Gene Set Enrichment Analysis (GSEA)" = "GSEA"
                            ),
                            selected = "ORA"
                        ),
                        conditionalPanel(
                            condition = sprintf("input['%s'] == 'ORA'", ns("goAnalysisType")),
                            checkboxGroupInput(
                                ns("goDirection"),
                                "Gene Direction",
                                choices = c("Up-regulated" = "Up", "Down-regulated" = "Down"),
                                selected = "Up"
                            )
                        ),
                        numericInput(ns("goPadjCutoff"), "Adjusted P-value Cutoff", value = 0.05, min = 0.001, max = 1, step = 0.01),
                        numericInput(ns("goMinGSSize"), "Min Gene Set Size", value = 10, min = 3, max = 100),
                        numericInput(ns("goMaxGSSize"), "Max Gene Set Size", value = 500, min = 50, max = 5000),
                        actionButton(ns("runGO"), "Run GO Analysis", status = "secondary", icon = icon("play"), class = "btn-block")
                    ),
                    box(
                        width = 8,
                        title = "Results Overview",
                        solidHeader = FALSE,
                        uiOutput(ns("goSummaryUI"))
                    )
                )
            ),
            tabPanel(
                "Plots",
                fluidRow(
                    box(
                        width = 12,
                        title = span("Enrichment Plots", actionButton(ns("go_plots_info"), icon("info"), size = "xs")),
                        sidebar = boxSidebar(
                            startOpen = FALSE,
                            id = ns("GOPlotSideBar"),
                            background = "#7f7f7f",
                            width = 25,
                            numericInput(ns("goTopN"), "Top N terms to show", value = 15, min = 5, max = 50)
                        ),
                        uiOutput(ns("goPlotsUI"))
                    )
                )
            ),
            tabPanel(
                "Table",
                fluidRow(
                    box(
                        width = 12,
                        title = "GO Enrichment Results",
                        uiOutput(ns("goTableUI"))
                    )
                )
            )
        )
    )
}


goServer <- function(id, parentSession, serverData) {
    moduleServer(
        id,
        function(input, output, session) {

            goResults <- reactiveValues(result = NULL, analysisType = NULL, ont = NULL)

            # Update comparison choices when DE results are available
            observe({
                req(serverData$comparisons)
                comparisons <- names(serverData$comparisons)
                if (length(comparisons) > 0) {
                    updateSelectInput(session, "goComparison", choices = comparisons, selected = comparisons[1])
                }
            })

            output$goSummaryUI <- renderUI({
                if (is.null(goResults$result)) {
                    div(
                        class = "text-center",
                        style = "padding: 60px;",
                        icon("circle-info", style = "font-size: 3em; color: #aaa;"),
                        h4("No GO analysis results yet", style = "color: #aaa; margin-top: 15px;"),
                        p("Configure settings on the left and click 'Run GO Analysis'", style = "color: #bbb;")
                    )
                } else {
                    result <- goResults$result
                    resultDf <- as.data.frame(result)
                    sigTerms <- sum(resultDf$p.adjust < input$goPadjCutoff, na.rm = TRUE)
                    fluidRow(
                        column(4, valueBox(sigTerms, "Significant Terms", color = "teal", icon = icon("list"), width = 12)),
                        column(4, valueBox(goResults$ont, "Ontology", color = "navy", icon = icon("dna"), width = 12)),
                        column(4, valueBox(goResults$analysisType, "Analysis Type", color = "purple", icon = icon("chart-bar"), width = 12))
                    )
                }
            })

            output$goPlotsUI <- renderUI({
                validate(need(!is.null(goResults$result), "Run GO Analysis first to see plots"))
                ns <- session$ns
                tabsetPanel(
                    tabPanel(
                        "Dot Plot",
                        plotOutput(ns("goDotPlot"), height = "550px") %>% shinycssloaders::withSpinner()
                    ),
                    tabPanel(
                        "Bar Plot",
                        plotOutput(ns("goBarPlot"), height = "550px") %>% shinycssloaders::withSpinner()
                    ),
                    tabPanel(
                        "Enrichment Map",
                        plotOutput(ns("goEmapPlot"), height = "550px") %>% shinycssloaders::withSpinner()
                    ),
                    tabPanel(
                        "Concept Network",
                        plotOutput(ns("goCnetPlot"), height = "650px") %>% shinycssloaders::withSpinner()
                    )
                )
            })

            output$goDotPlot <- renderPlot({
                req(goResults$result)
                tryCatch(
                    enrichplot::dotplot(goResults$result, showCategory = input$goTopN, font.size = 10) +
                        ggplot2::theme_bw() +
                        ggplot2::ggtitle(paste("GO", goResults$ont, "-", goResults$analysisType, "Dot Plot")),
                    error = function(e) { showNotification(paste("Dotplot error:", e$message), type = "error"); NULL }
                )
            })

            output$goBarPlot <- renderPlot({
                req(goResults$result)
                tryCatch(
                    barplot(goResults$result, showCategory = input$goTopN, font.size = 10) +
                        ggplot2::theme_bw() +
                        ggplot2::ggtitle(paste("GO", goResults$ont, "-", goResults$analysisType, "Bar Plot")),
                    error = function(e) { showNotification(paste("Barplot error:", e$message), type = "error"); NULL }
                )
            })

            output$goEmapPlot <- renderPlot({
                req(goResults$result)
                tryCatch({
                    result_pairwise <- enrichplot::pairwise_termsim(goResults$result)
                    enrichplot::emapplot(result_pairwise, showCategory = min(input$goTopN, 30)) +
                        ggplot2::ggtitle(paste("GO", goResults$ont, "- Enrichment Map"))
                },
                error = function(e) { showNotification(paste("Emap error:", e$message), type = "error"); NULL })
            })

            output$goCnetPlot <- renderPlot({
                req(goResults$result)
                tryCatch(
                    enrichplot::cnetplot(goResults$result, showCategory = min(input$goTopN, 10), circular = FALSE, colorEdge = TRUE) +
                        ggplot2::ggtitle(paste("GO", goResults$ont, "- Concept Network")),
                    error = function(e) { showNotification(paste("Cnet error:", e$message), type = "error"); NULL }
                )
            })

            output$goTableUI <- renderUI({
                validate(need(!is.null(goResults$result), "Run GO Analysis first"))
                ns <- session$ns
                tagList(
                    DT::DTOutput(ns("goTable")),
                    downloadButton(ns("downloadGOResults"), "Download Results", class = "btn-secondary")
                )
            })

            output$goTable <- DT::renderDataTable({
                req(goResults$result)
                df <- as.data.frame(goResults$result) %>%
                    dplyr::select(ID, Description, GeneRatio, BgRatio, pvalue, p.adjust, qvalue, Count) %>%
                    dplyr::arrange(p.adjust) %>%
                    dplyr::mutate(
                        pvalue = formatC(pvalue, format = "e", digits = 3),
                        p.adjust = formatC(p.adjust, format = "e", digits = 3),
                        qvalue = formatC(qvalue, format = "e", digits = 3)
                    )
                DT::datatable(
                    df,
                    class = "compact cell-border stripe",
                    options = list(scrollX = TRUE, pageLength = 20),
                    rownames = FALSE
                )
            })

            output$downloadGOResults <- downloadHandler(
                filename = function() {
                    paste0("GO_", goResults$ont, "_", input$goComparison, "_", Sys.Date(), ".csv")
                },
                content = function(file) {
                    write.csv(as.data.frame(goResults$result), file, row.names = FALSE)
                }
            )

            observeEvent(input$runGO, {
                req(input$goComparison, serverData$comparisons, serverData$organism)
                group <- input$goComparison
                deData <- serverData$comparisons[[group]][["DE"]]
                req(deData)

                withProgress(message = "Running GO Analysis...", value = 0, {
                    tryCatch({
                        incProgress(0.2, detail = "Preparing gene list...")

                        if (input$goAnalysisType == "ORA") {
                            req(length(input$goDirection) > 0)
                            geneList <- deData %>%
                                dplyr::filter(type %in% input$goDirection) %>%
                                dplyr::pull(geneID)

                            if (length(geneList) == 0) {
                                showNotification("No genes found for selected direction(s).", type = "warning")
                                return()
                            }

                            universe <- deData$geneID

                            incProgress(0.5, detail = "Running ORA...")
                            result <- clusterProfiler::enrichGO(
                                gene          = geneList,
                                universe      = universe,
                                OrgDb         = serverData$organism,
                                keyType       = "SYMBOL",
                                ont           = input$goOnt,
                                pAdjustMethod = "BH",
                                pvalueCutoff  = input$goPadjCutoff,
                                qvalueCutoff  = 0.2,
                                minGSSize     = input$goMinGSSize,
                                maxGSSize     = input$goMaxGSSize,
                                readable      = FALSE
                            )

                        } else {
                            geneList <- getGList(deData)
                            if (length(geneList) == 0) {
                                showNotification("No ranked gene list available.", type = "warning")
                                return()
                            }

                            incProgress(0.5, detail = "Running GSEA...")
                            result <- clusterProfiler::gseGO(
                                geneList      = geneList,
                                OrgDb         = serverData$organism,
                                keyType       = "SYMBOL",
                                ont           = input$goOnt,
                                minGSSize     = input$goMinGSSize,
                                maxGSSize     = input$goMaxGSSize,
                                pvalueCutoff  = input$goPadjCutoff,
                                pAdjustMethod = "BH",
                                verbose       = FALSE
                            )
                        }

                        incProgress(0.9, detail = "Storing results...")
                        goResults$result       <- result
                        goResults$analysisType <- input$goAnalysisType
                        goResults$ont          <- input$goOnt

                        # Store in serverData for RAG indexing
                        serverData$goResults[[group]] <- list(
                            result       = as.data.frame(result),
                            analysisType = input$goAnalysisType,
                            ont          = input$goOnt
                        )

                        setProgress(1)
                        nSig <- sum(as.data.frame(result)$p.adjust < input$goPadjCutoff, na.rm = TRUE)
                        showNotification(
                            paste0("GO Analysis complete: ", nSig, " significant terms found."),
                            type = "message"
                        )

                        updateTabsetPanel(session = parentSession, inputId = session$ns("GOTabsetpanel"), selected = "Plots")

                    },
                    error = function(e) {
                        showNotification(paste("GO Analysis error:", e$message), type = "error", duration = 10)
                        print(e)
                    })
                })
            })

            # Info modals
            observeEvent(input$go_settings_info, {
                req(serverData$descriptions)
                getPopupWindow("GO Settings", HTML(serverData$descriptions[["enrich_select"]]))
            })

            observeEvent(input$go_plots_info, {
                req(serverData$descriptions)
                getPopupWindow("GO Plots", HTML(serverData$descriptions[["enrich_result"]]))
            })

            observeEvent(input$infoModal, {
                shiny::showModal(modalDialog(
                    title = "Gene Ontology Module Help",
                    h4("Gene Ontology (GO) Enrichment Analysis"),
                    br(),
                    p("This module performs GO enrichment analysis on your differential expression results."),
                    br(),
                    h5("Analysis Types:"),
                    tags$ul(
                        tags$li(strong("ORA:"), " Over-Representation Analysis tests whether a set of genes (e.g., up-regulated) is overrepresented in GO terms."),
                        tags$li(strong("GSEA:"), " Gene Set Enrichment Analysis uses the full ranked gene list (by log2FC) to identify enriched pathways.")
                    ),
                    br(),
                    h5("Ontologies:"),
                    tags$ul(
                        tags$li(strong("BP:"), " Biological Process"),
                        tags$li(strong("MF:"), " Molecular Function"),
                        tags$li(strong("CC:"), " Cellular Component"),
                        tags$li(strong("ALL:"), " All three ontologies combined")
                    ),
                    size = "l",
                    easyClose = TRUE,
                    footer = NULL
                ))
            })
        }
    )
}
