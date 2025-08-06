degUI <- function(id) {

    ns = NS(id)


    tabItem(
        tabName = "DE",
        actionLink(ns("infoModal"), span("Walkthrough", icon("question-circle"))),
        # fluidRow(
            tabsetPanel(
                id = ns("DETabsetpanel"),
                selected = "Design",
                vertical = TRUE,
                type = "tabs",
                tabPanel(
                    "Design",
                    fluidRow(
                        column(12,
                        box(width = 12,
                            title =  span("Select design",actionButton(ns("de_select_design"), icon("info"), size ="xs")),
                            # title = "Select design",
                            uiOutput(ns("groupSelectUi")),
                            # p("Selected:"),
                            textOutput(ns("selectedDesign"))
                        )
                        ),
                        uiOutput(ns("addComparisonUi")),
                    # ),
                    # fluidRow(
                        column(12,
                            tooltip(
                                title = "This adds another comparison box",
                                placement = "bottom",
                                # actionButton(inputId = ns("addComparison"), label = "Add Comparison", class = "float-left", status="secondary")
                                actionButton(inputId = ns("addComparison"), label="Add Comparison", class="btn btn-default", icon("plus-circle", style="color:green;"))
                            ),
                            tooltip(
                                title = "This removes the last comparison box",
                                placement = "bottom",
                                # actionButton(inputId = ns("removeComparison"), label = "Remove Comparison", class = "float-left", status="secondary")
                                actionButton(inputId = ns("removeComparison"), label="Remove Comparison", class="btn btn-default", icon("trash-alt", style="color:red;"))
                            ), 
                            tooltip(
                                title = "This triggers the DESeq2 using selected design",
                                placement = "bottom",
                                actionButton(inputId = ns("runDE"), label = "Compare Groups", class = "float-right", status="secondary")
                            )
                        )
                    )
                ),
                tabPanel(
                    "Result",
                    fluidRow(
                        box(
                            title =  span("Plots",actionButton(ns("de_plots"), icon("info"), size ="xs")),
                            # title = "Plots", 
                            width = 12,
                            sidebar = boxSidebar(
                            startOpen = FALSE,
                            id = ns("DESumSideBar"),
                            background = "#7f7f7f",
                            width =25,
                            numericInput(ns("padjThreshold"), "Threshold of Padj:", 0.1, min = 0, max = 1),
                            numericInput(ns("fcThreshold"), "Threshold of Fold-change:", 1, min = 1, max = 100)        
                            ),
                            uiOutput(ns("dePlotsUi"))
                        )
                    ),
                    fluidRow(
                        box(
                            title =  span("Result table",actionButton(ns("de_result_table"), icon("info"), size ="xs")),
                            # title = "Result table",
                            width = 12,
                            uiOutput(ns("deResultTableUi"))
                        )                    
                    )
                ),
                tabPanel(
                    "Heatmap",
                    uiOutput(ns("deHeatmapUI"))
                )
            )
        # )
    )
}


degServer <- function(id,parentSession, serverData) {
    moduleServer(
        id,
        function(input,output,session) {

            output$groupSelectUi <- renderUI({
                ns <- session$ns  
                req(serverData$samples$group)
                getDeBucketList(
                    unselectedGroups=serverData$samples$group  %>% unique,
                    inputIdCol1=ns("treatSelected"),
                    inputIdCol2=ns("ctrlSelected"),
                    outputIdTextBox=ns(paste0("selectedDesign", count)),
                    titleCount=1,
                    selectedCol1=NULL,
                    selectedCol2=NULL,
                    onlyBucketList=TRUE
                )
            })

            observeEvent(serverData$groupComparisons$designSelected,{
                req(serverData$groupComparisons$designSelected, input$treatSelected, input$ctrlSelected)
                uniqueDesigns <- c()
                duplicateDesigns <- c()

                design <- paste0(input$treatSelected, "_vs_", input$ctrlSelected)
                if(! design %in% uniqueDesigns){
                    uniqueDesigns <- c(uniqueDesigns, design)
                }else{
                    if(!design %in% duplicateDesigns) duplicateDesigns <- c(duplicateDesigns, design)
                }
                for(comparison in names(serverData$groupComparisons$designSelected)){
                    design = paste0(serverData$groupComparisons$designSelected[[comparison]][1], "_vs_", serverData$groupComparisons$designSelected[[comparison]][2])
                    if(! design %in% uniqueDesigns){
                        uniqueDesigns <- c(uniqueDesigns, design)
                    }else{
                        if(!design %in% duplicateDesigns) duplicateDesigns <- c(duplicateDesigns, design)

                    }
                }
            })

            output$selectedDesign <- renderText(
                {   
                    validate(need(serverData$samples$group, message = "please upload data first"))
                    if(length(input$treatSelected)==0){
                        "Selected: None"
                    }else{
                        paste0("Selected: ",paste(input$treatSelected,"vs",input$ctrlSelected,sep="_"))                    
                    }
                }
            )
            ## DE summary 
            output$DEResSummary <- renderPrint({
                # metadata(resDE)$lfcThreshold <- log2(input$fcThreshold)
                group = paste0(serverData$treatment, "_vs_", serverData$control)
                cat(serverData$comparisons[[group]][["resDE_summary"]])
            })
 
            # observe input changes, https://stackoverflow.com/a/40182833
            observeEvent({
                input$padjThreshold
                input$fcThreshold
                1},{
                req(serverData$resDE.df)
                group = paste0(serverData$treatment, "_vs_", serverData$control)
                resDE = serverData$comparisons[[group]][["DE"]]

                ## Filtering results using updated thresholds
                resDE.df = prepare_resDE(resDE, input$padjThreshold, input$fcThreshold)
                serverData$resDE.df = reactive(resDE.df)
                serverData$comparisons[[group]][["DE"]] = resDE.df
                
                ## Updpating comparison thresholds
                serverData$comparisons[[group]]$fcThreshold = input$fcThreshold
                serverData$comparisons[[group]]$padjThreshold = input$padjThreshold

                #Update global values
                serverData$fcThreshold = input$fcThreshold
                serverData$padjThreshold = input$padjThreshold

                ## Updating resDE summary
                serverData$comparisons[[group]][["resDE_summary"]] = prepare_resDE_summary(serverData$comparisons[[group]][["DE"]], serverData$comparisons[[group]][["padjThreshold"]], serverData$comparisons[[group]][["fcThreshold"]], serverData$comparisons[[group]][["DE_mean_count"]])

                ## Resetting enrich cache
                serverData$comparisons[[group]][["enrichCache"]] = list("Up"=FALSE, "Down"=FALSE)
            })

            observeEvent({
                serverData$padjThreshold
                serverData$fcThreshold
                1},{
                req(serverData$resDE.df)
                ns <- session$ns
                group = paste0(serverData$treatment, "_vs_", serverData$control)
                if(serverData$padjThreshold>0) updateNumericInput(session=parentSession, inputId=ns('padjThreshold'),  value = serverData$padjThreshold)
                if(serverData$fcThreshold>0) updateNumericInput(session=parentSession, inputId=ns('fcThreshold'),  value = serverData$fcThreshold)    
            })            

            output$dePlotsUi <- renderUI({
                ns <- session$ns                
                validate(need(serverData$resDE.df, message = "please run DESeq first"))
                
                fluidRow(
                    box(
                        title = span("Summary", actionButton(ns("showDeSummapyInfo"), icon("info"), size ="xs")),
                        width = 4,
                        # echarts4rOutput(ns("DEResPie"))                            
                        plotlyOutput(ns("DEResPie"))                            
                    ),                        
                    box(
                        title = "Plots",
                        width = 8, 
                        plotlyOutput(ns("deResPlots"))
                    )                
                )
            })

            output$deResultTableUi <- renderUI({
                validate(need(serverData$resDE.df, message = "please run DESeq first"))
                ns = session$ns
                tagList(
                    DT::DTOutput(ns("DEResTab")),
                    downloadButton(ns("downloadDGEData"), "Download Results")                    
                )
            })

            observeEvent(input$showDeSummapyInfo,{
                showModal(
                    modalDialog(
                        title = "DE result summary",{
                            ns <- session$ns
                            verbatimTextOutput(ns("DEResSummary"))
                        }
                    )
                )
            })

            output$deHeatmapUI<- renderUI({
                validate(need(serverData$resDE.df, message = "please run DESeq first"))
                ns = session$ns
                fluidRow(
                    box(
                        title = "Select group",
                        width = 3,
                        selectizeInput(
                            inputId = ns("htGroup"),
                            label = "group",
                            choices = c("Up","Down"),
                            multiple = T
                            ),
                        actionButton(ns("drawHt"), "Draw heatmap", status = "secondary")
                    ),
                    box(
                        title= span("Heatmap",actionButton(ns("de_heatmap"), icon("info"), size ="xs")),
                        width = 9,
                        shinyjqui::jqui_resizable(plotOutput(ns("deHeatmap")))
                    )
                )
            })

            output$DEResPie <- renderPlotly({
                drawPieChart(serverData, group=paste0(serverData$treatment, "_vs_", serverData$control))
            })
            output$deResPlots <- renderPlotly({
                drawVolcanoMA(serverData, group=paste0(serverData$treatment, "_vs_", serverData$control))
            })

            ## heatmap 
            observeEvent(input$drawHt,{
                output$deHeatmap <- renderPlot({
                    req(serverData$vsd, serverData$pca, input$htGroup)
                    drawHeatmap(serverData, group=paste0(serverData$treatment, "_vs_", serverData$control), geneType=input$htGroup)
                })
            })


            # Downloadable csv of selected dataset ----
            output$downloadDGEData <- downloadHandler(
                filename = function() {
                    paste0(input$treatSelected, "_VS_",input$ctrlSelected, ".csv")
                },
                content = function(file) {
                    write.csv(serverData$resDE.df(), file, row.names = FALSE)
                }
            )
            # Modal for Select Desing, Plots, Table , Heatmap
            apply(data.frame(
                text=c("Select design","Plots","Result Table","Heatmap"),
                member=c("de_select_design","de_plots","de_result_table","de_heatmap")
                ),1,function(x){
                    observeEvent(input[[x[[2]]]],{
                        req(serverData$descriptions)
                        getPopupWindow(x[[1]], HTML(serverData$descriptions[[x[[2]]]]))
                    })    
                }
            )
            observeEvent(input$infoModal,{
                req(serverData$descriptions)
                getRnaAppDEGInfoModal(serverData)
            })
            observeEvent(input$runDE,{
                req(input$ctrlSelected, input$treatSelected)
                ns<- session$ns
                if(!"groupComparisons" %in% names(serverData) || is.null(serverData$groupComparisons)|| serverData$groupComparisons$designCount<1){
                        iterable = 0
                }else{
                    iterable = serverData$groupComparisons$designCount
                }
                inputComparison = list()
                for(x in seq(0,iterable)){
                ## comparison and group names
                    if(x==0){
                        inputComparison$ctrlSelected = input$ctrlSelected
                        inputComparison$treatSelected = input$treatSelected
                    }else{
                        inputComparison[[paste0("ctrlSelected", x)]] = input[[paste0("ctrlSelected", x)]]
                        inputComparison[[paste0("treatSelected", x)]] = input[[paste0("treatSelected", x)]]
                    }
                }
                returnVal <- runDE.async(
                    serverData=reactiveValuesToList(serverData),
                    inputComparison=inputComparison,
                    progressFile=progressFile
                )
                serverData$resDE.df <- reactive(returnVal$resDE.df)
                serverData$comparisons = returnVal$comparisons
                serverData$control = returnVal$control
                serverData$treatment = returnVal$treatment
                comparison_name = returnVal$comparison_name
                # Updating tabs
                updateTabsetPanel(session=parentSession,inputId= ns("DETabsetpanel"),selected="Result")
                # Updating filter values
                updateNumericInput(session=parentSession, inputId=ns('padjThreshold'),  value = returnVal$padjFilter)
                updateNumericInput(session=parentSession, inputId=ns('fcThreshold'),  value = returnVal$fcFilter)
                # Updating rmd
                # for(comparison in returnVal$allComparisons){
                #     serverData$rmd <- updateRmd(session=parentSession, boxId='DE-pie', serverData=serverData, group=comparison)
                #     serverData$rmd <- updateRmd(session=parentSession, boxId='DE-heatmap', serverData=serverData, group=comparison)
                #     serverData$rmd <- updateRmd(session=parentSession, boxId='DE-MA-Volcano', serverData=serverData, group=comparison)
                # }
            })
        }
    )
}


runDE.async = function(serverData, inputComparison, progressFile){
    tryCatch({
    allComparisons <- c()
    if(!"groupComparisons" %in% names(serverData) || is.null(serverData$groupComparisons)|| serverData$groupComparisons$designCount<1){
        iterable = 0
    }else{
        iterable = serverData$groupComparisons$designCount
    }
    for(x in seq(0,iterable)){
    if(x==0){
        serverData$control = inputComparison$ctrlSelected
        serverData$treatment = inputComparison$treatSelected
    }else{
        serverData$control = inputComparison[[paste0("ctrlSelected", x)]]
        serverData$treatment = inputComparison[[paste0("treatSelected", x)]]
    }
    if(is.null(serverData$control) || is.null(serverData$control)) next

    

    comparison_df = paste0("resDE_",serverData$treatment, "_vs_",  serverData$control)
    comparison_name = paste0(serverData$treatment, "_vs_", serverData$control)
    allComparisons <- c(allComparisons,comparison_name)
    
    ############
    ## run DE 
    ############        
    ## Setting filters to default values
    ## Storing comparison specific filters
    serverData$comparisons[[comparison_name]][["padjThreshold"]] = 0.1
    padjFilter = 0.1
    serverData$comparisons[[comparison_name]][["fcThreshold"]] = 1
    fcFilter = 1

    dds_sub <- serverData$dds[,serverData$dds$group %in% c(serverData$treatment,serverData$control)]
    dds_sub$group<- (dds_sub$group %>% droplevels) %>% relevel(.,ref = serverData$control)

    ## filter 
    keep <- rowSums(counts(dds_sub)) >= 10
    table(keep)
    dds_sub <- dds_sub[keep, ]

    ## run DESeq2
    dds_sub <- estimateSizeFactors(dds_sub)
    dds_sub <- estimateDispersions(dds_sub)
    dds_sub <- nbinomWaldTest(dds_sub)
    resDE <- results(dds_sub, contrast = c("group", serverData$treatment, serverData$control))
    mean_count <- round(metadata(resDE)$filterThreshold)[[1]]
    ## DE data frame
    resDE.df = prepare_resDE(resDE, padjFilter, fcFilter)
    
    # Common list to store comparison name and DF
    serverData$comparisons[[comparison_name]][["DE"]] = resDE.df
    serverData$comparisons[[comparison_name]][["enrich"]] = list()
    if(!"DE_mean_count" %in% names(serverData$comparisons[[comparison_name]])){
        serverData$comparisons[[comparison_name]][["DE_mean_count"]] = mean_count
    }
    ## Updating resDE summary
    serverData$comparisons[[comparison_name]][["resDE_summary"]] = prepare_resDE_summary(serverData$comparisons[[comparison_name]][["DE"]], serverData$comparisons[[comparison_name]][["padjThreshold"]], serverData$comparisons[[comparison_name]][["fcThreshold"]], serverData$comparisons[[comparison_name]][["DE_mean_count"]])
    }

    returnVal = list(
        padjFilter=padjFilter,
        fcFilter=fcFilter,
        resDE.df=resDE.df,
        comparisons=serverData$comparisons,
        comparison_name=comparison_name,
        control=serverData$control,
        treatment=serverData$treatment,
        allComparisons=unique(allComparisons)
    )
    return(returnVal)
    }, 
    interrupt = function(x){print(x); return(0)},
    error = function(x){print(x); return(0)}
    )
}

getDeBucketList = function(unselectedGroups, inputIdCol1, inputIdCol2, outputIdTextBox, titleCount=1, selectedCol1=NULL, selectedCol2=NULL, onlyBucketList=FALSE){
    bucketList = bucket_list(
        header = c(""),
        add_rank_list(
        text = "Candidate groups",
        labels = unselectedGroups
        ),
        add_rank_list(
        text = "Treatment",
        input_id = inputIdCol1,
        labels = selectedCol1
        ),
        add_rank_list(
        text = "Control",
        input_id = inputIdCol2,
        labels = selectedCol2
        )
    )
    if(onlyBucketList) return(bucketList)
    tagList(
    fluidRow(
        box(
            width=12,
            title =  span(paste0("Select design ", titleCount)),
            bucketList,
            textOutput(outputIdTextBox)
        )
    )
    )
}